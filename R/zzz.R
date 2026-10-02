# What the package tells Shiny and shinyformtools about the grid and the
# basket when it loads: the input handlers, the input types, the board as a
# part of form_ui(), and the labels and messages of the modules.
# shinyformtools does not know this package; everything is registered here.

.onLoad <- function(libname, pkgname) {
  # The grid input reports {rows, cols, values}; the handler turns that into a
  # matrix before it reaches input$<id>. The basket input reports a list of
  # lines; the handler makes it a data frame.
  shiny::registerInputHandler("shinygridtools.grid", sgt_grid_input_handler, force = TRUE)
  shiny::registerInputHandler("shinygridtools.cart", sgt_cart_input_handler, force = TRUE)
  sgt_register_grid_type()
  sgt_register_cart_type()
  sgt_register_texts()
  # form_ui(show_board = TRUE) draws this.
  shinyformtools::register_ui_part("board", function(id, labels) cart_board_ui(id, labels = labels))
  invisible(NULL)
}

# The grid as an `input_type` of shinyformtools::form_field().
sgt_register_grid_type <- function() {
  shinyformtools::register_input(
    "grid_input",
    fun = grid_input,
    value_arg = "value",
    update_fun = update_grid_input,
    encode = sgt_grid_encode,
    decode = sgt_grid_decode,
    format = sgt_grid_format,
    blank = list(value = NULL),
    clear = function(session, inputId) update_grid_input(session, inputId, value = NA),
    empty = NULL
  )
}

# The basket as an `input_type`, with its stock check on every save and its
# module parts (palette, board) inside form_server().
sgt_register_cart_type <- function() {
  shinyformtools::register_input(
    "cart_input",
    fun = cart_input,
    value_arg = "value",
    update_fun = update_cart_input,
    encode = sgt_cart_encode,
    decode = sgt_cart_decode,
    format = sgt_cart_format,
    sep = ", ",
    prepare_args = sgt_cart_prepare_args,
    blank = list(value = NULL),
    clear = function(session, inputId) update_cart_input(session, inputId, value = NA),
    empty = NULL,
    validate = sgt_cart_validate,
    server = sgt_cart_server
  )
}

# The modules' labels and messages, in both languages shinyformtools knows,
# so `language()`, `german()` and `english()` carry them like its own.
sgt_register_texts <- function() {
  shinyformtools::register_texts(
    "en",
    labels = list(
      grid_save = "Save",
      grid_saving = "Saving ...",
      grid_saved = "Saved {time}",
      grid_sum = "Sum",
      grid_not_allowed = "You may not change these rows: {rows}. The grid shows the stored values again.",
      grid_conflict = "Someone else changed {rows} in the meantime. The grid now shows the current values; please enter your change again.",
      grid_duplicates = "Several records are stored for {rows}; the grid shows the first. Delete the extra ones in the records table.",
      cart_drop = "Drag items here or click them",
      cart_on_site = "on site",
      cart_left = "{n} left",
      cart_none_left = "none left",
      cart_loading = "Loading items ...",
      cart_remove = "Remove",
      board_search = "Search",
      board_stock = "Stock",
      board_empty = "Nothing yet"
    ),
    messages = list(
      cart_not_enough = "'{label}': only {available} x {item} left.",
      cart_item_gone = "'{label}': {item} is no longer offered."
    )
  )
  shinyformtools::register_texts(
    "de",
    labels = list(
      grid_save = "Speichern",
      grid_saving = "Speichert ...",
      grid_saved = "Gespeichert {time}",
      grid_sum = "Summe",
      grid_not_allowed = "Diese Zeilen d\u00fcrfen Sie nicht \u00e4ndern: {rows}. Das Raster zeigt wieder die gespeicherten Werte.",
      grid_conflict = "Jemand anderes hat {rows} inzwischen ge\u00e4ndert. Das Raster zeigt jetzt den aktuellen Stand; bitte geben Sie Ihre \u00c4nderung erneut ein.",
      grid_duplicates = "F\u00fcr {rows} sind mehrere Datens\u00e4tze gespeichert; das Raster zeigt den ersten. L\u00f6schen Sie die \u00fcberz\u00e4hligen in der Tabelle.",
      cart_drop = "Gegenst\u00e4nde hierher ziehen oder anklicken",
      cart_on_site = "vor Ort",
      cart_left = "noch {n} frei",
      cart_none_left = "keine mehr frei",
      cart_loading = "Gegenst\u00e4nde werden geladen ...",
      cart_remove = "Entfernen",
      board_search = "Suchen",
      board_stock = "Lager",
      board_empty = "Noch nichts"
    ),
    messages = list(
      cart_not_enough = "'{label}': nur noch {available} x {item} frei.",
      cart_item_gone = "'{label}': {item} wird nicht mehr angeboten."
    )
  )
  invisible(NULL)
}
