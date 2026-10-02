/* Basket input for shinygridtools: a palette of items that are dragged (or
   clicked) into a basket, counts with + and -, a "kept on site" switch per
   line, and the remaining stock per item. The value sent to R is a list of
   lines {item, label, icon, n, on_site}. */
(function () {
  function parseJSON(text, fallback) {
    if (!text) return fallback;
    try { return JSON.parse(text); } catch (e) { return fallback; }
  }

  function state(root) {
    if (!root._sgtCart) {
      root._sgtCart = {
        items: parseJSON(root.getAttribute("data-items"), null),
        value: parseJSON(root.getAttribute("data-value"), []) || [],
        labels: parseJSON(root.getAttribute("data-labels"), {})
      };
    }
    return root._sgtCart;
  }

  function disabled(root) {
    return !!(root.closest(".shinyjs-disabled") || root.closest("[disabled]"));
  }

  function el(tag, cls, text) {
    var e = document.createElement(tag);
    if (cls) e.className = cls;
    if (text !== undefined && text !== null) e.textContent = text;
    return e;
  }

  function itemById(s, id) {
    if (!s.items) return null;
    for (var i = 0; i < s.items.length; i++) if (s.items[i].id === id) return s.items[i];
    return null;
  }

  /* What is still free of an item for this basket: the server's count minus
     what the basket takes (lines kept on site take nothing). null: no limit. */
  function free(s, id) {
    var item = itemById(s, id);
    if (!item || item.available === null || item.available === undefined) return null;
    var taken = 0;
    s.value.forEach(function (line) { if (line.item === id && !line.on_site) taken += line.n; });
    return item.available - taken;
  }

  function lineFor(s, id, onSite) {
    for (var i = 0; i < s.value.length; i++) {
      if (s.value[i].item === id && !!s.value[i].on_site === !!onSite) return s.value[i];
    }
    return null;
  }

  function add(root, id) {
    var s = state(root);
    if (disabled(root)) return;
    var item = itemById(s, id);
    if (!item) return;
    var left = free(s, id);
    if (left !== null && left <= 0) return;
    var line = lineFor(s, id, false);
    if (line) {
      line.n += 1;
    } else {
      s.value.push({ item: id, label: item.label, icon: item.icon || null, n: 1, on_site: false });
    }
    changed(root);
  }

  function changed(root) {
    draw(root);
    if (root._sgtCallback) root._sgtCallback(false);
  }

  function interpolate(text, n) {
    return String(text || "").replace("{n}", n);
  }

  function draw(root) {
    var s = state(root);
    var off = disabled(root);
    root.innerHTML = "";

    var palette = el("div", "sgt-cart-palette");
    if (!s.items) {
      palette.appendChild(el("div", "sgt-cart-note", s.labels.loading));
    } else {
      s.items.forEach(function (item) {
        var left = free(s, item.id);
        var tile = el("button", "sgt-cart-tile");
        tile.type = "button";
        tile.draggable = !off;
        tile.dataset.item = item.id;
        if (item.icon) tile.appendChild(el("span", "sgt-cart-icon", item.icon));
        tile.appendChild(el("span", "sgt-cart-name", item.label));
        if (left !== null) {
          tile.appendChild(el("span", "sgt-cart-left" + (left <= 0 ? " sgt-cart-out" : ""),
            left > 0 ? interpolate(s.labels.left, left) : s.labels.none_left));
        }
        tile.disabled = off || (left !== null && left <= 0);
        palette.appendChild(tile);
      });
    }

    var basket = el("div", "sgt-cart-basket");
    if (!s.value.length) basket.appendChild(el("div", "sgt-cart-note", s.labels.drop));
    s.value.forEach(function (line, index) {
      var row = el("div", "sgt-cart-line");
      row.dataset.index = index;
      var name = el("span", "sgt-cart-line-name");
      if (line.icon) name.appendChild(el("span", "sgt-cart-icon", line.icon));
      name.appendChild(el("span", null, line.label || line.item));
      row.appendChild(name);

      var minus = el("button", "sgt-cart-minus", "−");
      minus.type = "button"; minus.dataset.action = "minus"; minus.disabled = off;
      var count = el("span", "sgt-cart-count", line.n);
      var plus = el("button", "sgt-cart-plus", "+");
      plus.type = "button"; plus.dataset.action = "plus";
      var left = free(s, line.item);
      plus.disabled = off || (!line.on_site && left !== null && left <= 0);
      row.appendChild(minus); row.appendChild(count); row.appendChild(plus);

      var site = el("label", "sgt-cart-site");
      var box = document.createElement("input");
      box.type = "checkbox"; box.checked = !!line.on_site; box.dataset.action = "site"; box.disabled = off;
      site.appendChild(box);
      site.appendChild(document.createTextNode(" " + (s.labels.on_site || "")));
      row.appendChild(site);

      var remove = el("button", "sgt-cart-remove", "×");
      remove.type = "button"; remove.dataset.action = "remove"; remove.disabled = off;
      remove.title = s.labels.remove || "";
      remove.setAttribute("aria-label", s.labels.remove || "");
      row.appendChild(remove);
      basket.appendChild(row);
    });

    root.appendChild(palette);
    root.appendChild(basket);
  }

  /* One line per item and switch: merge a line into a twin after a toggle. */
  function merge(s) {
    var out = [];
    s.value.forEach(function (line) {
      var twin = null;
      out.forEach(function (o) { if (o.item === line.item && !!o.on_site === !!line.on_site) twin = o; });
      if (twin) twin.n += line.n; else out.push(line);
    });
    s.value = out.filter(function (line) { return line.n > 0; });
  }

  function onClick(e) {
    var root = e.currentTarget;
    var s = state(root);
    if (disabled(root)) return;
    var tile = e.target.closest(".sgt-cart-tile");
    if (tile) { add(root, tile.dataset.item); return; }
    var action = e.target.dataset.action;
    var row = e.target.closest(".sgt-cart-line");
    if (!action || !row || action === "site") return;
    var line = s.value[parseInt(row.dataset.index, 10)];
    if (!line) return;
    if (action === "plus") {
      var left = free(s, line.item);
      if (!line.on_site && left !== null && left <= 0) return;
      line.n += 1;
    } else if (action === "minus") {
      line.n -= 1;
    } else if (action === "remove") {
      line.n = 0;
    }
    merge(s);
    changed(root);
  }

  function onChange(e) {
    var root = e.currentTarget;
    if (e.target.dataset.action !== "site") return;
    e.stopPropagation();
    var s = state(root);
    var row = e.target.closest(".sgt-cart-line");
    var line = row && s.value[parseInt(row.dataset.index, 10)];
    if (!line || disabled(root)) return;
    line.on_site = e.target.checked;
    merge(s);
    changed(root);
  }

  var binding = new Shiny.InputBinding();
  $.extend(binding, {
    find: function (scope) { return $(scope).find(".sgt-cart-input"); },
    getId: function (el) { return el.id; },
    getType: function () { return "shinygridtools.cart"; },
    initialize: function (el) { draw(el); },
    /* The field asks the server for its palette once it is on the page: a
       dialog drawn by a renderUI (inline layout) may appear after the
       palette was sent on open. */
    subscribePalette: function (el) {
      if (window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue(el.id + "_palette", Date.now(), { priority: "event" });
      }
    },
    getValue: function (el) {
      return state(el).value.map(function (line) {
        return { item: line.item, label: line.label, icon: line.icon || null, n: line.n, on_site: !!line.on_site };
      });
    },
    setValue: function (el, value) { state(el).value = value || []; draw(el); },
    subscribe: function (el, callback) {
      el._sgtCallback = callback;
      binding.subscribePalette(el);
      el.addEventListener("click", onClick);
      el.addEventListener("change", onChange);
      el.addEventListener("dragstart", function (e) {
        var tile = e.target.closest && e.target.closest(".sgt-cart-tile");
        if (!tile || disabled(el)) return;
        e.dataTransfer.setData("text/plain", tile.dataset.item);
        e.dataTransfer.effectAllowed = "copy";
      });
      el.addEventListener("dragover", function (e) {
        if (e.target.closest(".sgt-cart-basket")) {
          e.preventDefault();
          e.target.closest(".sgt-cart-basket").classList.add("sgt-cart-over");
        }
      });
      el.addEventListener("dragleave", function (e) {
        var basket = e.target.closest && e.target.closest(".sgt-cart-basket");
        if (basket) basket.classList.remove("sgt-cart-over");
      });
      el.addEventListener("drop", function (e) {
        var basket = e.target.closest(".sgt-cart-basket");
        if (!basket) return;
        e.preventDefault();
        basket.classList.remove("sgt-cart-over");
        add(el, e.dataTransfer.getData("text/plain"));
      });
    },
    unsubscribe: function (el) {
      el._sgtCallback = null;
      el.removeEventListener("click", onClick);
      el.removeEventListener("change", onChange);
    },
    receiveMessage: function (el, data) {
      var s = state(el);
      if (data.clear) s.value = [];
      if (data.value) s.value = data.value;
      if (data.items) s.items = data.items;
      draw(el);
      if ((data.clear || data.value) && el._sgtCallback) el._sgtCallback(false);
    }
  });

  Shiny.inputBindings.register(binding, "shinygridtools.cart");
})();
