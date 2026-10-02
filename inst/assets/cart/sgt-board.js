/* Basket board for shinygridtools: the records of a form with a basket
   field on the left, the stock on the right. One drag moves one piece: from
   the stock onto a record, from one record to another, or back onto the
   stock. The server stores the move and sends the new state. */
(function () {
  function parseJSON(text, fallback) {
    if (!text) return fallback;
    try { return JSON.parse(text); } catch (e) { return fallback; }
  }

  function el(tag, cls, text) {
    var e = document.createElement(tag);
    if (cls) e.className = cls;
    if (text !== undefined && text !== null) e.textContent = text;
    return e;
  }

  function labels(root) {
    if (!root._sgtLabels) root._sgtLabels = parseJSON(root.getAttribute("data-labels"), {});
    return root._sgtLabels;
  }

  function send(root, from, to, item) {
    if (from === to) return;
    root._sgtSeq = (root._sgtSeq || 0) + 1;
    Shiny.setInputValue(root.id + "_move", { from: from, to: to, item: item, seq: root._sgtSeq }, { priority: "event" });
  }

  function dragData(e, item, from) {
    e.dataTransfer.setData("text/plain", JSON.stringify({ item: item, from: from }));
    e.dataTransfer.effectAllowed = "move";
  }

  function readDrag(e) {
    return parseJSON(e.dataTransfer.getData("text/plain"), null);
  }

  /* The frame (search box, list, stock) is built once, so the search text
     and the scroll position survive every update. */
  function frame(root) {
    if (root._sgtFrame) return root._sgtFrame;
    var l = labels(root);
    root.innerHTML = "";
    var left = el("div", "sgt-board-left");
    var search = el("input", "form-control sgt-board-search");
    search.type = "search";
    search.placeholder = l.search || "";
    search.setAttribute("aria-label", l.search || "");
    var list = el("div", "sgt-board-records");
    left.appendChild(search);
    left.appendChild(list);
    var stock = el("div", "sgt-board-stock");
    stock.appendChild(el("div", "sgt-board-stock-title", l.stock || ""));
    var tiles = el("div", "sgt-cart-palette");
    stock.appendChild(tiles);
    root.appendChild(left);
    root.appendChild(stock);

    search.addEventListener("input", function () { filter(root); });

    root.addEventListener("dragstart", function (e) {
      var source = e.target.closest && e.target.closest("[data-item]");
      if (!source || !root._sgtCanMove || source.dataset.fixed === "1") { if (source) e.preventDefault(); return; }
      var holder = source.closest(".sgt-board-record");
      dragData(e, source.dataset.item, holder ? Number(holder.dataset.id) : null);
    });
    root.addEventListener("dragover", function (e) {
      var target = e.target.closest(".sgt-board-record, .sgt-board-stock");
      if (!target || !root._sgtCanMove) return;
      e.preventDefault();
      target.classList.add("sgt-cart-over");
    });
    root.addEventListener("dragleave", function (e) {
      var target = e.target.closest && e.target.closest(".sgt-board-record, .sgt-board-stock");
      if (target && !target.contains(e.relatedTarget)) target.classList.remove("sgt-cart-over");
    });
    root.addEventListener("drop", function (e) {
      var target = e.target.closest(".sgt-board-record, .sgt-board-stock");
      if (!target) return;
      e.preventDefault();
      target.classList.remove("sgt-cart-over");
      var data = readDrag(e);
      if (!data || !data.item) return;
      var to = target.classList.contains("sgt-board-record") ? Number(target.dataset.id) : null;
      if (data.from === null && to === null) return;
      send(root, data.from, to, data.item);
    });

    root._sgtFrame = { search: search, list: list, tiles: tiles };
    return root._sgtFrame;
  }

  function filter(root) {
    var f = frame(root);
    var term = f.search.value.trim().toLowerCase();
    Array.prototype.forEach.call(f.list.children, function (card) {
      card.style.display = !term || (card.dataset.name || "").indexOf(term) >= 0 ? "" : "none";
    });
  }

  function draw(root, data) {
    var f = frame(root);
    var l = labels(root);
    root._sgtCanMove = !!data.can_move;
    root.classList.toggle("sgt-board-readonly", !data.can_move);

    f.list.innerHTML = "";
    (data.records || []).forEach(function (record) {
      var card = el("div", "sgt-board-record");
      card.dataset.id = record.id;
      card.dataset.name = String(record.label || "").toLowerCase();
      card.appendChild(el("div", "sgt-board-name", record.label));
      var chips = el("div", "sgt-board-chips");
      if (!record.cart.length) chips.appendChild(el("span", "sgt-cart-note", l.empty));
      record.cart.forEach(function (line) {
        var chip = el("span", "sgt-board-chip" + (line.on_site ? " sgt-board-onsite" : ""));
        chip.dataset.item = line.item;
        chip.draggable = !!data.can_move && !line.on_site;
        if (line.on_site) {
          chip.dataset.fixed = "1";
          chip.title = l.on_site || "";
        }
        if (line.icon) chip.appendChild(el("span", "sgt-cart-icon", line.icon));
        chip.appendChild(el("span", "sgt-board-chip-name", line.label));
        chip.appendChild(el("span", "sgt-board-chip-n", line.n));
        chips.appendChild(chip);
      });
      card.appendChild(chips);
      f.list.appendChild(card);
    });
    filter(root);

    f.tiles.innerHTML = "";
    (data.items || []).forEach(function (item) {
      var left = item.available === null || item.available === undefined ? null : item.available;
      var tile = el("div", "sgt-cart-tile");
      tile.dataset.item = item.id;
      tile.draggable = !!data.can_move && (left === null || left > 0);
      if (!tile.draggable) tile.dataset.fixed = "1";
      if (left !== null && left <= 0) tile.classList.add("sgt-board-empty");
      if (item.icon) tile.appendChild(el("span", "sgt-cart-icon", item.icon));
      tile.appendChild(el("span", "sgt-cart-name", item.label));
      if (left !== null) {
        tile.appendChild(el("span", "sgt-cart-left" + (left <= 0 ? " sgt-cart-out" : ""),
          left > 0 ? String(l.left || "").replace("{n}", left) : l.none_left));
      }
      f.tiles.appendChild(tile);
    });
  }

  function init(root) {
    if (root._sgtBoardReady) return;
    root._sgtBoardReady = true;
    frame(root);
    Shiny.setInputValue(root.id + "_ready", true);
  }

  Shiny.addCustomMessageHandler("sgt-cart-board", function (data) {
    var root = document.getElementById(data.id);
    if (root) draw(root, data);
  });

  function scan() {
    Array.prototype.forEach.call(document.querySelectorAll(".sgt-board"), init);
  }

  $(document).on("shiny:connected", scan);
  $(document).on("shiny:value", function () { setTimeout(scan, 0); });
  if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected && Shiny.shinyapp.isConnected()) scan();
})();
