# The basket board: cart_board_ui() (or form_ui(show_board = TRUE) in
# shinyformtools, which renders the same thing) shows every record of a form
# with a cart_input field as a drop target on the left and the stock on the
# right. One drag moves one piece: from the stock to a record, between two
# records, or back to the stock. Each move is one transaction through
# update_record(), so the stock check, the audit log and the table behave as
# for a save in the dialog.

sgt_board_dependency <- function() {
  htmltools::htmlDependency(
    name = "sgt-board",
    version = as.character(utils::packageVersion("shinygridtools")),
    src = c(file = system.file("assets", "cart", package = "shinygridtools")),
    script = "sgt-board.js",
    stylesheet = c("sgt-cart.css", "sgt-board.css")
  )
}

#' The basket board
#'
#' Every record of a form with a `cart_input` field as a drop target on the
#' left and the stock on the right, above the records table. One drag moves
#' one piece: from the stock onto a record, from one record to another, or
#' back onto the stock. Each move is saved at once through
#' [shinyformtools::update_record()], with the same stock check and rules as
#' the dialog (`can_view_table`, `can_view_record`, `can_edit`, the field's
#' `editable`, `editable_fields`; derived fields are recomputed and a basket
#' that `dynamic_visibility()` hides is not changed). A search box filters
#' the records, and moves made in other sessions show within a few seconds.
#' Lines kept on site show hatched and stay where they are. The record is
#' named by the form's first text or choice field.
#'
#' Place it on the page of the form module whose records it shows; the
#' module part that serves it runs inside [shinyformtools::form_server()] for
#' every form with a `cart_input` field. `form_ui(show_board = TRUE)` of
#' shinyformtools renders the same board above its table: this package
#' registers it there with [shinyformtools::register_ui_part()].
#'
#' @param id The id of the form module ([shinyformtools::form_ui()] /
#'   [shinyformtools::form_server()]) whose records the board shows.
#' @param labels Optional named list overriding the board's labels
#'   (`board_search`, `board_stock`, `board_empty`, `cart_on_site`, ...), as
#'   `form_ui(labels = )`.
#'
#' @return A UI element.
#' @seealso [cart_input()]
#' @export
cart_board_ui <- function(id, labels = list()) {
  sgt_board_ui(shiny::NS(id), ui_labels(labels))
}

sgt_board_ui <- function(ns, labels) {
  htmltools::attachDependencies(
    shiny::div(
      id = ns("board"),
      class = "sgt-board",
      `data-labels` = as.character(jsonlite::toJSON(list(
        search = labels$board_search,
        stock = labels$board_stock,
        empty = labels$board_empty,
        on_site = labels$cart_on_site,
        left = labels$cart_left,
        none_left = labels$cart_none_left,
        loading = labels$cart_loading
      ), auto_unbox = TRUE))
    ),
    sgt_board_dependency()
  )
}

# The field the board works on and the field that names a record.
sgt_board_fields <- function(form) {
  fields <- form_fields(form)
  cart <- Filter(sgt_is_cart_field, fields)

  if (length(cart) == 0L) {
    return(NULL)
  }

  text_types <- c("textInput", "textAreaInput", "selectInput", "selectizeInput", "radioButtons")
  named <- Filter(function(field) field$input_type %in% text_types, fields)

  list(cart = cart[[1L]], label = if (length(named) > 0L) named[[1L]])
}

# What the browser draws: the records with their baskets and the stock with
# what is still free overall.
sgt_board_payload <- function(conn, form, fields, records, can_move) {
  if ("sft_is_deleted" %in% names(records)) {
    records <- records[records$sft_is_deleted == 0L, , drop = FALSE]
  }
  column <- fields$cart$db_column
  label_column <- if (!is.null(fields$label)) fields$label$db_column

  cards <- lapply(seq_len(nrow(records)), function(i) {
    cart <- sgt_cart_decode(records[[column]][i])
    name <- if (!is.null(label_column)) as.character(records[[label_column]][i]) else NA_character_
    list(
      id = records$sft_id[i],
      label = if (is.na(name) || !nzchar(name)) paste0("#", records$sft_id[i]) else name,
      cart = if (is.null(cart)) list() else lapply(seq_len(nrow(cart)), function(k) {
        list(item = cart$item[k], label = cart$label[k],
             icon = if (!is.na(cart$icon[k])) cart$icon[k], n = cart$n[k], on_site = cart$on_site[k])
      })
    )
  })

  items <- sgt_cart_available_items(conn, form, fields$cart)

  list(
    records = cards,
    items = if (is.null(items)) list() else sgt_cart_items_json(items),
    can_move = isTRUE(can_move)
  )
}

# A basket with one piece of `item` taken out (delivered lines only; kept on
# site stays) or put in. NULL when there is nothing to take.
sgt_board_take <- function(cart, item) {
  hit <- which(cart$item == item & !cart$on_site)[1L]
  if (is.na(hit)) {
    return(NULL)
  }
  cart$n[hit] <- cart$n[hit] - 1L
  cart[cart$n > 0L, , drop = FALSE]
}

sgt_board_put <- function(cart, item, catalog) {
  hit <- which(cart$item == item & !cart$on_site)[1L]
  if (!is.na(hit)) {
    cart$n[hit] <- cart$n[hit] + 1L
    return(cart)
  }
  known <- if (!is.null(catalog)) match(item, catalog$id) else NA_integer_
  line <- data.frame(
    item = item,
    label = if (!is.na(known)) catalog$label[known] else item,
    icon = if (!is.na(known)) catalog$icon[known] else NA_character_,
    n = 1L, on_site = FALSE, stringsAsFactors = FALSE
  )
  rbind(cart, line)
}

sgt_board_stored_cart <- function(conn, form, field, record_id) {
  stored <- sgt_cart_query(
    conn,
    paste0(
      "SELECT ", sgt_cart_quote(conn, field$db_column), " AS cart FROM ",
      sgt_cart_quote(conn, form$table_name), " WHERE sft_id = ? AND sft_is_deleted = 0"
    ),
    list(as.integer(record_id))
  )$cart

  if (length(stored) == 0L) {
    stop("The record is no longer there; the board shows the current state again.", call. = FALSE)
  }

  sgt_cart_decode(stored[1L]) %||% sgt_cart_empty_frame()
}

# One move in one transaction: take from `from` (NULL: the stock), put into
# `to` (NULL: the stock). The source is written first, so the target's
# stock check already counts the piece as free.
# Writes one record's basket the way the dialog saves an edit: `rules`
# (apply_edit_rules() of the module) recomputes locked derived fields and
# drops a basket a dynamic_visibility binding hides on this record, which
# refuses the move.
sgt_board_write <- function(conn, form, field, record_id, cart, user, rules = NULL) {
  # The last piece taken: stored empty. An empty data frame would count as
  # "not given" and the update would be dropped.
  values <- stats::setNames(list(if (nrow(cart) == 0L) NA else cart), field$id)

  if (is.function(rules)) {
    applied <- rules(record_id, values)
    if (!field$id %in% names(applied$values)) {
      stop("'", field$label, "' is not shown for this record, so the board cannot change it.", call. = FALSE)
    }
    values <- applied$values
    form <- applied$form
  }

  update_record(form, values, record_id = record_id, conn = conn, user = user)
}

sgt_board_move <- function(conn, form, field, from, to, item, user, rules = NULL) {
  catalog <- sgt_cart_catalog_items(field, conn)

  # Taken before the first write: two crossing moves (1 -> 2 and 2 -> 1) on
  # MariaDB would otherwise each hold a row the other waits for.
  release <- sgt_cart_lock(conn, sgt_cart_stock_key(form, field))
  on.exit(release(), add = TRUE)

  with_transaction(conn, {
    if (!is.null(from)) {
      cart <- sgt_board_take(sgt_board_stored_cart(conn, form, field, from), item)
      if (is.null(cart)) {
        stop("That piece is no longer there; the board shows the current state again.", call. = FALSE)
      }
      sgt_board_write(conn, form, field, from, cart, user, rules)
    }
    if (!is.null(to)) {
      cart <- sgt_board_put(sgt_board_stored_cart(conn, form, field, to), item, catalog)
      sgt_board_write(conn, form, field, to, cart, user, rules)
    }
  })

  invisible(TRUE)
}

sgt_register_cart_board <- function(input, output, session, state) {
  fields <- sgt_board_fields(state$form)

  if (is.null(fields)) {
    return(invisible(list()))
  }

  board_id <- session$ns("board")
  last_stamp <- NULL

  # The same rights as editing the field in the dialog: seeing the table and
  # the records, can_edit, the field's `editable` for this user, and
  # permissions$editable_fields.
  can_move <- function() {
    if (!module_permission(state$permissions, "can_view_table") ||
        !state$permission("can_view_record") || !state$permission("can_edit")) {
      return(FALSE)
    }
    field <- find_field(resolve_editable(state$form, state$current_user()), fields$cart$id)
    if (isFALSE(field$editable)) {
      return(FALSE)
    }
    allowed <- tryCatch(
      module_editable_fields(state$permissions),
      error = function(err) character()
    )
    is.null(allowed) || fields$cart$id %in% allowed
  }

  push <- function() {
    state$guard(function() {
      if (!module_permission(state$permissions, "can_view_table")) {
        return()
      }
      payload <- sgt_board_payload(state$conn(), state$form, fields, state$records(), can_move())
      payload$id <- board_id
      session$sendCustomMessage("sgt-cart-board", payload)
    })
  }

  # The browser says when a board is on the page; nothing is sent before.
  shiny::observe({
    if (!isTRUE(input$board_ready)) {
      return()
    }
    # A failing read is reported by the table; it must not end the session.
    tryCatch(state$records(), error = function(err) NULL)
    shiny::isolate(push())
  })

  # What other sessions moved shows within a few seconds: one cheap query.
  shiny::observe({
    if (!isTRUE(input$board_ready)) {
      return()
    }
    shiny::invalidateLater(5000)
    stamp <- tryCatch(
      records_stamp(state$form, list(), shiny::isolate(state$conn())),
      error = function(err) NULL
    )
    if (!is.null(stamp) && !is.null(last_stamp) && !identical(stamp, last_stamp)) {
      shiny::isolate(state$refresh())
    }
    last_stamp <<- stamp
  })

  shiny::observeEvent(input$board_move, {
    move <- input$board_move
    from <- suppressWarnings(as.integer(move$from))
    to <- suppressWarnings(as.integer(move$to))
    from <- if (length(from) == 1L && !is.na(from)) from
    to <- if (length(to) == 1L && !is.na(to)) to
    item <- move$item
    item <- if (length(item) == 1L && !is.list(item)) as.character(item) else ""

    # Only from a page that shows a board (form_ui(show_board = TRUE)).
    if (!isTRUE(input$board_ready) ||
        !nzchar(item) || (is.null(from) && is.null(to)) || identical(from, to)) {
      return()
    }

    done <- state$guard(function() {
      if (!isTRUE(can_move())) {
        stop(ui_label("edit_not_allowed", labels = state$labels), call. = FALSE)
      }
      user <- state$current_user()
      sgt_board_move(
        conn = state$conn(),
        form = resolve_editable(state$form, user),
        field = fields$cart, from = from, to = to, item = item, user = user,
        rules = function(record_id, values) apply_edit_rules(state, record_id, values)
      )
      state$saved_tick(shiny::isolate(state$saved_tick()) + 1L)
    })

    # Also after a refusal: the browser shows the stored state again.
    state$refresh()
    if (!isTRUE(done)) {
      push()
    }
  })

  invisible(list())
}
