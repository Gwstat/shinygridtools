/* Grid input for shinygridtools: a table of number cells with keyboard
   navigation, block paste from a spreadsheet, live row / column sums and a
   debounced value. The value sent to R is {rows, cols, values} with one
   array per row; an empty cell is null. */
(function () {
  var DEBOUNCE_MS = 500;

  function cells(root) {
    return Array.prototype.slice.call(root.querySelectorAll("input.sgt-grid-cell"));
  }

  /* The cells indexed once per rendered grid. A querySelector per cell made
     every keystroke cost O(cells^2): 24 ms at 1000 cells. A re-rendered grid
     is a new element, so the index never goes stale. */
  function index(root) {
    if (!root._sgtCells) {
      var byRow = [];
      cells(root).forEach(function (el) {
        var r = parseInt(el.dataset.r, 10), c = parseInt(el.dataset.c, 10);
        (byRow[r] = byRow[r] || [])[c] = el;
      });
      root._sgtCells = byRow;
    }
    return root._sgtCells;
  }

  function cellAt(root, r, c) {
    var line = index(root)[r];
    return line ? line[c] || null : null;
  }

  function parse(text) {
    if (text === null || text === undefined) return null;
    text = String(text).trim().replace(",", ".");
    if (!text.length) return null;
    var v = Number(text);
    return isNaN(v) ? null : v;
  }

  /* The decimal separator of the browser's language: "," for de, "." for en. */
  var LOCALE_DECIMAL = (function () {
    try {
      var parts = new Intl.NumberFormat(navigator.language).formatToParts(1.5);
      for (var i = 0; i < parts.length; i++) if (parts[i].type === "decimal") return parts[i].value;
    } catch (e) {}
    return ".";
  })();

  /* A number pasted from a spreadsheet, whose display format may carry
     thousands separators. Only for paste: a number cell already hands over
     what the user typed as a plain decimal.
     - Both "." and ",": the last one is the decimal separator
       ("1.234,5" and "1,234.5" are 1234.5).
     - One kind, several times: thousands ("1.234.567").
     - One kind, once, followed by groups of exactly three digits ("1.234",
       "1,234"): in a whole-number column always thousands, since a count
       cannot be 1.234; in a decimal column thousands only if it is not the
       browser's decimal separator.
     - Otherwise the separator is the decimal one ("1,5", "12.5").
     Returns null for an empty cell and undefined for text that is not a
     number (the cell is then left as it is). */
  function parsePasted(text, wholeNumbers, decimal) {
    if (text === null || text === undefined) return null;
    var t = String(text).replace(/[\s\u00a0\u202f']/g, "");
    if (!t.length) return null;
    var dots = (t.match(/\./g) || []).length;
    var commas = (t.match(/,/g) || []).length;
    if (dots && commas) {
      if (t.lastIndexOf(",") > t.lastIndexOf(".")) t = t.replace(/\./g, "").replace(",", ".");
      else t = t.replace(/,/g, "");
    } else if (dots > 1 || commas > 1) {
      t = t.replace(/[.,]/g, "");
    } else if (dots || commas) {
      var sep = dots ? "." : ",";
      var grouped = /^-?[1-9]\d{0,2}[.,]\d{3}$/.test(t);
      if (grouped && (wholeNumbers || sep !== (decimal || LOCALE_DECIMAL))) t = t.replace(sep, "");
      else t = t.replace(",", ".");
    }
    var v = Number(t);
    return isNaN(v) ? undefined : v;
  }

  /* A cell whose step is a whole number takes whole numbers only. */
  function wholeNumberCell(el) {
    var step = el.getAttribute("step");
    if (step === null || step === "" || step === "any") return false;
    var n = Number(step);
    return !isNaN(n) && n > 0 && Math.floor(n) === n;
  }

  /* Equal numbers up to float noise (see sgt_grid_same() in R). */
  function sameNumber(a, b) {
    if (a === null || b === null) return a === b;
    return Math.abs(a - b) <= 1e-12 * Math.max(1, Math.abs(a), Math.abs(b));
  }

  function values(root) {
    var rows = JSON.parse(root.dataset.rows || "[]");
    var cols = JSON.parse(root.dataset.cols || "[]");
    var out = [];
    for (var r = 0; r < rows.length; r++) {
      var line = [];
      for (var c = 0; c < cols.length; c++) {
        var el = cellAt(root, r, c);
        line.push(el ? parse(el.value) : null);
      }
      out.push(line);
    }
    var result = { rows: rows, cols: cols, values: out };
    if (root.dataset.token) result.token = root.dataset.token;
    // The last patch this grid applied: the server can tell a value read
    // before that patch from one read after it.
    result.seq = root._sgtSeq || 0;
    return result;
  }

  function fmt(x) {
    return Number.isInteger(x) ? String(x) : String(Math.round(x * 100) / 100);
  }

  function sumCells(root) {
    if (!root._sgtSums) {
      var rows = [], cols = [];
      root.querySelectorAll("[data-sum-row]").forEach(function (el) { rows[parseInt(el.dataset.sumRow, 10)] = el; });
      root.querySelectorAll("[data-sum-col]").forEach(function (el) { cols[parseInt(el.dataset.sumCol, 10)] = el; });
      root._sgtSums = { rows: rows, cols: cols, total: root.querySelector(".sgt-grid-total") };
    }
    return root._sgtSums;
  }

  function sums(root) {
    if (root.dataset.sums !== "true") return;
    var v = values(root);
    var out = sumCells(root);
    var colSum = v.cols.map(function () { return 0; });
    var total = 0;
    v.values.forEach(function (line, r) {
      var rowSum = 0;
      line.forEach(function (x, c) {
        if (x !== null) { rowSum += x; colSum[c] += x; total += x; }
      });
      var cell = out.rows[r];
      if (cell) cell.textContent = rowSum ? fmt(rowSum) : "";
      var tr = cell && cell.parentNode;
      if (tr) tr.classList.toggle("sgt-grid-row-filled", rowSum !== 0);
    });
    colSum.forEach(function (s, c) {
      var cell = out.cols[c];
      if (cell) cell.textContent = s ? fmt(s) : "";
    });
    var t = out.total;
    if (t) t.textContent = total ? fmt(total) : "";
  }

  function markFilled(el) {
    el.classList.toggle("sgt-grid-filled", parse(el.value) !== null && parse(el.value) !== 0);
  }

  function setValues(root, matrix) {
    if (!matrix) return;
    matrix.forEach(function (line, r) {
      (line || []).forEach(function (x, c) {
        var el = cellAt(root, r, c);
        if (!el) return;
        el.value = (x === null || x === undefined) ? "" : x;
        markFilled(el);
      });
    });
    sums(root);
  }

  function setHint(root, matrix) {
    if (!matrix) return;
    matrix.forEach(function (line, r) {
      (line || []).forEach(function (x, c) {
        var el = cellAt(root, r, c);
        if (!el) return;
        var span = el.parentNode.querySelector(".sgt-grid-hint");
        var show = x !== null && x !== undefined;
        if (show && !span) {
          span = document.createElement("span");
          span.className = "sgt-grid-hint";
          el.parentNode.appendChild(span);
        }
        if (span) {
          span.textContent = show ? x : "";
          span.style.display = show ? "" : "none";
        }
      });
    });
  }

  function move(root, el, dr, dc) {
    var r = parseInt(el.dataset.r, 10) + dr;
    var c = parseInt(el.dataset.c, 10) + dc;
    var target = cellAt(root, r, c);
    if (target) { target.focus(); target.select(); }
  }

  var binding = new Shiny.InputBinding();
  $.extend(binding, {
    find: function (scope) { return $(scope).find(".sgt-grid-input"); },
    getId: function (el) { return el.id; },
    getType: function () { return "shinygridtools.grid"; },
    initialize: function (el) { sums(el); },
    getValue: function (el) { return values(el); },
    setValue: function (el, value) { setValues(el, value && value.values ? value.values : value); },
    subscribe: function (el, callback) {
      $(el).on("input.sgtGrid", "input.sgt-grid-cell", function (e) {
        markFilled(e.target);
        sums(el);
        el._sgtDirty = true;
        /* Once the debounce has sent the value nothing is pending any more.
           A grid replaced after that (a refusal re-renders it while the
           cursor is still inside) sent its old value again. */
        clearTimeout(el._sgtDirtyTimer);
        el._sgtDirtyTimer = setTimeout(function () { el._sgtDirty = false; }, DEBOUNCE_MS + 50);
        callback(true);
      });
      $(el).on("change.sgtGrid", "input.sgt-grid-cell", function () { el._sgtDirty = false; callback(false); });
      $(el).on("sgt-grid-paste.sgtGrid", function () { el._sgtDirty = false; callback(false); });
    },
    /* The grid is being replaced (another group chosen) while keystrokes
       still wait in the debounce: send them now, as their own input. Sent as
       the grid's own value, the new grid's first value (same input id, same
       flush) would replace them, and what was typed would be lost. */
    unsubscribe: function (el) {
      if (el._sgtDirty) {
        el._sgtDirty = false;
        Shiny.setInputValue(el.id + "_late:shinygridtools.grid", values(el), { priority: "event" });
      }
      $(el).off(".sgtGrid");
    },
    getRatePolicy: function () { return { policy: "debounce", delay: DEBOUNCE_MS }; },
    receiveMessage: function (el, data) {
      if (data.patch) {
        // What others saved: applied to a cell only if it still shows the
        // value the server knew and the user is not in it. The server learns
        // which cells took the new value.
        if (String(el.dataset.token || "") !== String(data.token || "")) return;
        var applied = [];
        data.patch.forEach(function (p) {
          var cell = cellAt(el, p[0], p[1]);
          if (!cell || cell === document.activeElement) return;
          var current = parse(cell.value);
          var shown = p[2] === undefined ? null : p[2];
          if (!sameNumber(current, shown)) return;
          cell.value = (p[3] === null || p[3] === undefined) ? "" : p[3];
          markFilled(cell);
          applied.push([p[0], p[1]]);
        });
        sums(el);
        el._sgtSeq = data.seq || 0;
        Shiny.setInputValue(el.id + "_synced", { token: data.token, seq: data.seq, applied: applied }, { priority: "event" });
        return;
      }
      if (data.clear) {
        cells(el).forEach(function (cell) { cell.value = ""; markFilled(cell); });
        sums(el);
        $(el).trigger("sgt-grid-paste");
      }
      if (data.values) setValues(el, data.values);
      if (data.hint) setHint(el, data.hint);
      if (data.values) $(el).trigger("sgt-grid-paste");
    }
  });
  Shiny.inputBindings.register(binding, "shinygridtools.grid");

  /* Enter and the arrow keys walk the grid; Tab keeps the browser default. */
  document.addEventListener("keydown", function (e) {
    var el = e.target;
    if (!el.classList || !el.classList.contains("sgt-grid-cell")) return;
    var root = el.closest(".sgt-grid-input");
    if (!root) return;
    if (e.key === "Enter" || e.key === "ArrowDown") { e.preventDefault(); move(root, el, 1, 0); }
    else if (e.key === "ArrowUp") { e.preventDefault(); move(root, el, -1, 0); }
    else if (e.key === "ArrowRight") { e.preventDefault(); move(root, el, 0, 1); }
    else if (e.key === "ArrowLeft") { e.preventDefault(); move(root, el, 0, -1); }
  });

  /* A block copied from a spreadsheet lands from the focused cell onwards. */
  document.addEventListener("paste", function (e) {
    var el = e.target;
    if (!el.classList || !el.classList.contains("sgt-grid-cell")) return;
    var text = (e.clipboardData || window.clipboardData).getData("text");
    if (!text || (text.indexOf("\t") === -1 && text.indexOf("\n") === -1)) return;
    e.preventDefault();
    var root = el.closest(".sgt-grid-input");
    var r0 = parseInt(el.dataset.r, 10), c0 = parseInt(el.dataset.c, 10);
    // Only the newline a spreadsheet puts after the last row is dropped; an
    // empty line in the middle is an empty cell of a one-column block.
    var lines = text.replace(/\r/g, "").split("\n");
    if (lines.length > 1 && lines[lines.length - 1] === "") lines.pop();
    lines.forEach(function (line, i) {
      line.split("\t").forEach(function (val, j) {
        var target = cellAt(root, r0 + i, c0 + j);
        if (!target || target.disabled) return;
        var v = parsePasted(val, wholeNumberCell(target));
        if (v === undefined) return;
        target.value = v === null ? "" : v;
        markFilled(target);
      });
    });
    sums(root);
    $(root).trigger("sgt-grid-paste");
  });
})();
