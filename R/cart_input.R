# A basket as a Shiny input: a palette of items (with an icon, emoji welcome)
# is dragged or clicked into the basket, counts are set with + and -, and
# each basket line can be marked as kept on site. The value is a data frame
# with one row per item. Assets in inst/assets/cart.

sgt_cart_dependency <- function() {
  htmltools::htmlDependency(
    name = "sgt-cart",
    version = as.character(utils::packageVersion("shinygridtools")),
    src = c(file = system.file("assets", "cart", package = "shinygridtools")),
    script = "sgt-cart.js",
    stylesheet = "sgt-cart.css"
  )
}

# Small local helpers, so this file needs nothing internal of shinyformtools.
sgt_cart_quote <- function(conn, x) {
  as.character(DBI::dbQuoteIdentifier(conn, x))
}

sgt_cart_query <- function(conn, sql, params = list()) {
  if (length(params) > 0L) {
    DBI::dbGetQuery(conn, sql, params = params)
  } else {
    DBI::dbGetQuery(conn, sql)
  }
}

# Field arguments as the schema registration stores them (sft_fields.args_json).
sgt_cart_args_json <- function(args) {
  as.character(jsonlite::toJSON(args, auto_unbox = TRUE, null = "null", POSIXt = "ISO8601"))
}

sgt_cart_db_key <- function(conn) {
  info <- tryCatch(DBI::dbGetInfo(conn), error = function(err) NULL)
  as.character(info$dbname %||% "")
}

sgt_cart_empty_frame <- function() {
  data.frame(
    item = character(), label = character(), icon = character(),
    n = integer(), on_site = logical(), stringsAsFactors = FALSE
  )
}

# Whatever a caller hands over (a data frame, a list of lines, a named
# vector of counts) as the value's data frame: one row per item, counts
# above zero, the same item on two lines added up (on-site and delivered
# stay apart).
sgt_cart_normalize <- function(value) {
  if (is.null(value) || length(value) == 0L ||
      (length(value) == 1L && !is.list(value) && is.na(value))) {
    return(sgt_cart_empty_frame())
  }

  if (is.numeric(value) && !is.null(names(value))) {
    value <- data.frame(item = names(value), n = unname(value), stringsAsFactors = FALSE)
  } else if (is.list(value) && !is.data.frame(value)) {
    # Lines that are not objects (a crafted client) are left out.
    value <- Filter(is.list, value)
    if (length(value) == 0L) {
      return(sgt_cart_empty_frame())
    }
    value <- do.call(rbind, lapply(value, function(line) {
      data.frame(
        item = as.character(line$item %||% NA_character_),
        label = as.character(line$label %||% NA_character_),
        icon = as.character(line$icon %||% NA_character_),
        n = suppressWarnings(as.numeric(line$n %||% NA)),
        on_site = isTRUE(as.logical(line$on_site %||% FALSE)),
        stringsAsFactors = FALSE
      )
    }))
  }

  if (!is.data.frame(value) || !"item" %in% names(value)) {
    stop("A cart value is a data frame with the columns item and n.", call. = FALSE)
  }

  n <- suppressWarnings(as.numeric(value$n %||% rep(1, nrow(value))))
  out <- data.frame(
    item = as.character(value$item),
    label = as.character(value$label %||% value$item),
    icon = as.character(value$icon %||% rep(NA_character_, nrow(value))),
    n = ifelse(is.na(n), 0, round(n)),
    on_site = as.logical(value$on_site %||% rep(FALSE, nrow(value))),
    stringsAsFactors = FALSE
  )
  out$on_site[is.na(out$on_site)] <- FALSE
  out$label[is.na(out$label) | !nzchar(out$label)] <- out$item[is.na(out$label) | !nzchar(out$label)]
  out <- out[!is.na(out$item) & nzchar(out$item) & out$n > 0, , drop = FALSE]

  # A count no basket holds (from a script or a crafted client): refused
  # with a message instead of an integer overflow further down.
  too_big <- function(n) any(!is.finite(n) | n > 1e6)
  if (too_big(out$n)) {
    stop("A basket count must be a whole number up to 1000000.", call. = FALSE)
  }

  if (nrow(out) == 0L) {
    return(sgt_cart_empty_frame())
  }

  key <- paste(out$item, out$on_site, sep = "\r")
  first <- !duplicated(key)
  totals <- tapply(out$n, factor(key, levels = unique(key)), sum)
  if (too_big(totals)) {
    stop("A basket count must be a whole number up to 1000000.", call. = FALSE)
  }
  out <- out[first, , drop = FALSE]
  out$n <- as.integer(totals[paste(out$item, out$on_site, sep = "\r")])
  rownames(out) <- NULL
  out
}

# Stored representation: a JSON array of lines. Label and icon are stored
# with the count, so the table still reads well after the catalog changed.
sgt_cart_encode <- function(value) {
  if (is.null(value) || (length(value) == 1L && !is.list(value) && is.na(value))) {
    return(NA_character_)
  }

  value <- sgt_cart_normalize(value)

  if (nrow(value) == 0L) {
    return(NA_character_)
  }

  as.character(jsonlite::toJSON(value, dataframe = "rows", na = "null", auto_unbox = TRUE))
}

sgt_cart_decode <- function(stored) {
  if (is.null(stored) || length(stored) == 0L || is.na(stored[1L]) || !nzchar(stored[1L])) {
    return(NULL)
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(stored[1L], simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (!is.list(parsed)) {
    return(NULL)
  }

  out <- tryCatch(sgt_cart_normalize(parsed), error = function(e) NULL)

  if (is.null(out) || nrow(out) == 0L) {
    return(NULL)
  }

  out
}

# What the records table and the export show: "2 x <icon> Scope, 1 x Compass
# (on site)".
sgt_cart_format <- function(value, sep = ", ") {
  if (length(value) == 0L || is.na(value[1L]) || !nzchar(value[1L])) {
    return("")
  }

  # Read straight from the JSON: a table of 5000 baskets took 5 s through
  # sgt_cart_decode(), which builds a data frame per basket.
  lines <- tryCatch(jsonlite::parse_json(value[1L]), error = function(e) NULL)
  if (!is.list(lines) || !is.null(names(lines))) {
    return(as.character(value[1L]))
  }
  lines <- Filter(function(line) is.list(line) && !is.null(line$item), lines)
  if (length(lines) == 0L) {
    return("")
  }

  on_site <- ui_labels()$cart_on_site
  text <- vapply(lines, function(line) {
    label <- line$label %||% line$item
    name <- trimws(paste(c(line$icon, label), collapse = " "))
    paste0(line$n %||% "", " \u00d7 ", name, if (isTRUE(line$on_site)) paste0(" (", on_site, ")"))
  }, character(1))
  paste(text, collapse = sep)
}

# Shiny input handler: the client's list of lines becomes the data frame.
sgt_cart_input_handler <- function(value, shinysession = NULL, name = NULL) {
  # More lines than a palette offers, or a shape no browser sends: no value,
  # rather than an error that ends the session or seconds of work.
  if (is.null(value) || length(value) == 0L || length(value) > 1000L) {
    return(NULL)
  }

  out <- tryCatch(sgt_cart_normalize(value), error = function(e) NULL)
  if (is.null(out)) {
    return(NULL)
  }

  if (nrow(out) == 0L) {
    return(NULL)
  }

  out
}

# Palette items as the client expects them: id, label, icon and how many are
# still available to this basket (NULL: no limit).
sgt_cart_items_json <- function(items) {
  if (is.null(items) || nrow(items) == 0L) {
    return(list())
  }

  lapply(seq_len(nrow(items)), function(i) {
    list(
      id = items$id[i],
      label = items$label[i],
      icon = if (!is.na(items$icon[i])) items$icon[i],
      available = if (!is.na(items$available[i])) items$available[i]
    )
  })
}

# The palette as a data frame with id, label, icon, stock, available.
sgt_cart_items_frame <- function(items) {
  if (is.null(items)) {
    return(NULL)
  }

  if (!is.data.frame(items)) {
    stop("items must be a data frame with the columns id and label.", call. = FALSE)
  }

  if (!"id" %in% names(items)) {
    stop("items must have a column id.", call. = FALSE)
  }

  stock <- if ("stock" %in% names(items)) suppressWarnings(as.numeric(items$stock)) else rep(NA_real_, nrow(items))
  available <- if ("available" %in% names(items)) suppressWarnings(as.numeric(items$available)) else stock

  data.frame(
    id = as.character(items$id),
    label = as.character(items$label %||% items$id),
    icon = if ("icon" %in% names(items)) as.character(items$icon) else rep(NA_character_, nrow(items)),
    stock = stock,
    available = available,
    stringsAsFactors = FALSE
  )
}

#' A basket as a Shiny input
#'
#' Renders a palette of items and a basket. Items are dragged into the basket
#' (or clicked, which also works on touch screens and with the keyboard);
#' `+` and `-` set the count, and each line can be marked as kept on site.
#' The palette shows how many of an item are still available and refuses more.
#' The input's value is a data frame with the columns `item`, `label`, `icon`,
#' `n` and `on_site`, or `NULL` for an empty basket.
#'
#' `cart_input` is also an `input_type` for [shinyformtools::form_field()]
#' (registered when this package loads): the basket is stored as one JSON text
#' and shown in the records table as "2 x Scope, 1 x Compass". The palette
#' then comes from `args`:
#'
#' * `items`: a data frame with the columns `id`, `label` and optionally
#'   `icon` and `stock`, fixed in the code; or
#' * `catalog`: [cart_catalog()], another form whose records are the items,
#'   read when the dialog opens.
#'
#' With a `stock` column, [shinyformtools::form_server()] shows each item's remaining count
#' and a save that would exceed it is refused with a validation error, also
#' from scripts and on restore. The stock of a catalog is shared by every
#' basket that uses it: other records, other basket fields of the same
#' record, and basket fields of other forms naming the same catalog. Other
#' forms are known from their registration, which [shinyformtools::init_db()] writes and
#' which a form brings up to date the first time it counts; a form that points
#' a basket field at another catalog and has not run since counts with its
#' old one. Lines kept on site do not
#' count. Deleting a record frees what it held. A record that already holds
#' more than the stock (the stock was lowered) can still be saved as long as
#' it does not ask for more.
#'
#' The dialog and the board store each line with the catalog's name and icon
#' at that moment. A script stores `label` and `icon` as it passes them (the
#' item id when it passes none); the stock check goes by `item` only.
#'
#' @param inputId Input id.
#' @param label Optional label above the input.
#' @param value Starting basket: a data frame as described above (`item` and
#'   `n` are enough), or `NULL`. A count is a whole number up to 1000000.
#' @param items Palette: a data frame with `id`, `label`, and optionally
#'   `icon` and `available` (how many may still be added; `NA` for no limit).
#' @param width Optional CSS width.
#'
#' @return A Shiny input tag.
#' @seealso [update_cart_input()], [cart_catalog()]
#' @examples
#' \dontrun{
#' library(shiny)
#' items <- data.frame(
#'   id = c("scope", "camera", "compass"),
#'   label = c("Spotting scope", "Camera", "Compass"),
#'   icon = c("\U0001F52D", "\U0001F4F7", "\U0001F9ED"),
#'   available = c(3, 10, NA)
#' )
#' ui <- fluidPage(cart_input("equipment", "Equipment", items = items),
#'                 tableOutput("value"))
#' server <- function(input, output, session) {
#'   output$value <- renderTable(input$equipment)
#' }
#' shinyApp(ui, server)
#' }
#' @export
cart_input <- function(inputId, label = NULL, value = NULL, items = NULL, width = NULL) {
  value <- sgt_cart_normalize(value)
  items <- sgt_cart_items_frame(items)
  labels <- ui_labels()

  tag <- shiny::tags$div(
    class = "form-group shiny-input-container",
    style = if (!is.null(width)) paste0("width:", htmltools::validateCssUnit(width), ";"),
    if (!is.null(label)) shiny::tags$label(class = "control-label", `for` = inputId, label),
    shiny::tags$div(
      id = inputId,
      class = "sgt-cart-input",
      `data-value` = as.character(jsonlite::toJSON(value, dataframe = "rows", na = "null", auto_unbox = TRUE)),
      `data-items` = if (!is.null(items)) {
        as.character(jsonlite::toJSON(sgt_cart_items_json(items), auto_unbox = TRUE, null = "null"))
      },
      `data-labels` = as.character(jsonlite::toJSON(list(
        drop = labels$cart_drop,
        on_site = labels$cart_on_site,
        left = labels$cart_left,
        none_left = labels$cart_none_left,
        loading = labels$cart_loading,
        remove = labels$cart_remove
      ), auto_unbox = TRUE))
    )
  )

  htmltools::attachDependencies(tag, sgt_cart_dependency())
}

#' Update a basket input from the server
#'
#' @param session The Shiny session.
#' @param inputId Input id.
#' @param value Optional new basket (see [cart_input()]); `NA` empties it.
#' @param items Optional new palette (see [cart_input()]).
#'
#' @return Called for its side effect.
#' @seealso [cart_input()]
#' @export
update_cart_input <- function(session, inputId, value = NULL, items = NULL) {
  message <- list()

  if (length(value) == 1L && !is.data.frame(value) && !is.list(value) && is.na(value)) {
    message$clear <- TRUE
  } else if (!is.null(value)) {
    message$value <- sgt_cart_normalize(value)
  }

  if (!is.null(items)) {
    message$items <- sgt_cart_items_json(sgt_cart_items_frame(items))
  }

  if (length(message) > 0L) {
    session$sendInputMessage(inputId, message)
  }

  invisible(NULL)
}

#' Items of a basket from another form
#'
#' Names the form whose records are the palette of a `cart_input` field, and
#' which of its fields hold the item id, the label, the icon and the stock.
#' Pass it as `form_field(input_type = "cart_input", args = list(catalog =
#' cart_catalog(...)))`. Its live records are read each time a dialog opens
#' and on every save, so an admin can maintain items and stock in that form.
#'
#' @param form Object created with [shinyformtools::form()]: the items.
#' @param id Field id of the item's id; stored in the basket, so keep it
#'   stable (a code such as `"scope"`).
#' @param label Field id of the item's name.
#' @param icon Optional field id of an icon or emoji shown with the name.
#' @param stock Optional field id of the number available in total, shared by
#'   every basket that uses this catalog. Without it the items are unlimited;
#'   so is an item whose stock is empty or not a number. Two items with the
#'   same id: the first record counts.
#'
#' @return A list to pass as `args$catalog`.
#' @export
cart_catalog <- function(form, id, label = id, icon = NULL, stock = NULL) {
  if (!inherits(form, "sft_form")) {
    stop("form must be a form object.", call. = FALSE)
  }

  column <- function(field_id) {
    if (is.null(field_id)) {
      return(NULL)
    }
    field <- find_field(form, field_id)
    if (is.null(field)) {
      stop("cart_catalog(): the form has no field '", field_id, "'.", call. = FALSE)
    }
    field$db_column
  }

  list(
    table = form$table_name,
    id = column(id),
    label = column(label),
    icon = column(icon),
    stock = column(stock)
  )
}

# The palette of a cart field: from args$items, or read from the catalog
# table with `conn`. id, label, icon, stock (NA: no limit).
sgt_cart_catalog_items <- function(field, conn = NULL) {
  args <- field$args %||% list()

  if (!is.null(args$items)) {
    return(sgt_cart_items_frame(args$items))
  }

  catalog <- args$catalog

  if (is.null(catalog) || is.null(conn)) {
    return(NULL)
  }

  columns <- unlist(catalog[c("id", "label", "icon", "stock")])
  rows <- tryCatch(
    DBI::dbGetQuery(
      conn,
      paste0(
        "SELECT ", paste(sgt_cart_quote(conn, unique(columns)), collapse = ", "),
        " FROM ", sgt_cart_quote(conn, catalog$table),
        " WHERE sft_is_deleted = 0 ORDER BY sft_id"
      )
    ),
    error = function(e) {
      stop(
        "The items of '", field$label, "' come from the table '", catalog$table,
        "', which is missing or lacks a column. Call init_db() on that form once. (",
        conditionMessage(e), ")",
        call. = FALSE
      )
    }
  )

  # The same id twice: the first record counts, in the palette and the check.
  rows <- rows[!duplicated(as.character(rows[[catalog$id]])), , drop = FALSE]

  sgt_cart_items_frame(data.frame(
    id = as.character(rows[[catalog$id]]),
    label = as.character(rows[[catalog$label]]),
    icon = if (!is.null(catalog$icon)) as.character(rows[[catalog$icon]]) else rep(NA_character_, nrow(rows)),
    stock = if (!is.null(catalog$stock)) suppressWarnings(as.numeric(rows[[catalog$stock]])) else rep(NA_real_, nrow(rows)),
    stringsAsFactors = FALSE
  ))
}

sgt_is_cart_field <- function(field) {
  identical(field$input_type, "cart_input")
}

# The item counts of many stored baskets in one parse: a data frame with the
# columns item and n (lines kept on site left out). Per record through
# sgt_cart_decode() this cost about 1 ms, 5 s for 5000 records on every save.
sgt_cart_counts <- function(stored) {
  stored <- stored[!is.na(stored) & nzchar(stored)]
  none <- data.frame(item = character(), n = numeric(), stringsAsFactors = FALSE)

  if (length(stored) == 0L) {
    return(none)
  }

  parsed <- tryCatch(
    jsonlite::parse_json(paste0("[", paste(stored, collapse = ","), "]")),
    error = function(e) NULL
  )
  # One unreadable value: parse them one by one and skip that one.
  if (is.null(parsed)) {
    parsed <- lapply(stored, function(x) tryCatch(jsonlite::parse_json(x), error = function(e) NULL))
  }

  lines <- do.call(c, Filter(function(x) is.list(x) && is.null(names(x)), parsed))
  lines <- Filter(function(line) is.list(line) && !is.null(line$item), lines)

  if (length(lines) == 0L) {
    return(none)
  }

  item <- vapply(lines, function(line) as.character(line$item[[1L]]), character(1))
  n <- vapply(lines, function(line) suppressWarnings(as.numeric(line$n %||% NA)[1L]), numeric(1))
  on_site <- vapply(lines, function(line) isTRUE(as.logical(line$on_site %||% FALSE)[1L]), logical(1))
  n <- round(n)
  keep <- !on_site & !is.na(n) & n > 0 & !is.na(item) & nzchar(item)

  data.frame(item = item[keep], n = n[keep], stringsAsFactors = FALSE)
}

# What one stock is shared by: a catalog form's table (every basket field of
# every form that names it), or a fixed item list (that one field).
sgt_cart_stock_key <- function(form, field) {
  catalog <- field$args$catalog
  if (!is.null(catalog)) {
    return(paste0("catalog:", catalog$table))
  }
  paste0("items:", form$table_name, ".", field$db_column)
}

# The basket columns that share the stock of `field`: this form's fields with
# the same catalog, plus those of every other registered form (sft_fields
# keeps each field's arguments). A data frame with table and column.
sgt_cart_stock_users <- function(conn, form, field) {
  key <- sgt_cart_stock_key(form, field)
  own <- Filter(
    function(f) sgt_is_cart_field(f) && identical(sgt_cart_stock_key(form, f), key),
    form_fields(form)
  )
  users <- data.frame(
    table = rep(form$table_name, length(own)),
    column = vapply(own, function(f) f$db_column, character(1)),
    stringsAsFactors = FALSE
  )

  catalog <- field$args$catalog
  if (is.null(catalog)) {
    return(users)
  }

  sgt_cart_sync_registration(conn, form)
  registered <- tryCatch(
    DBI::dbGetQuery(
      conn,
      paste0(
        "SELECT m.table_name, f.db_column, f.args_json FROM sft_fields f ",
        "JOIN sft_forms m ON m.form_id = f.form_id ",
        "WHERE f.status = 'active' AND f.input_type = 'cart_input' AND f.form_id <> ?"
      ),
      params = list(form$form_id)
    ),
    error = function(e) NULL
  )

  for (i in seq_len(if (is.null(registered)) 0L else nrow(registered))) {
    args <- tryCatch(jsonlite::fromJSON(registered$args_json[i], simplifyVector = TRUE), error = function(e) NULL)
    table <- args$catalog$table
    if (length(table) == 1L && identical(as.character(table), catalog$table)) {
      users <- rbind(users, data.frame(
        table = registered$table_name[i], column = registered$db_column[i], stringsAsFactors = FALSE
      ))
    }
  }

  unique(users)
}

# The registered arguments of this form's basket fields follow the code. A
# field pointed at another catalog keeps its columns, so the schema probe
# passes and init_db() would not rewrite them; other forms then counted
# against the old catalog. Its own fields a form always takes from the code
# (sgt_cart_stock_users()); this keeps what other forms read current.
.sgt_cart_synced <- new.env(parent = emptyenv())

sgt_cart_sync_registration <- function(conn, form) {
  for (field in Filter(sgt_is_cart_field, form_fields(form))) {
    args <- sgt_cart_args_json(field$args)
    # Once per R process and definition: no write on every save.
    key <- paste(sgt_cart_db_key(conn), form$table_name, field$id, args, sep = "\x1f")
    if (exists(key, envir = .sgt_cart_synced, inherits = FALSE)) {
      next
    }
    assign(key, TRUE, envir = .sgt_cart_synced)
    tryCatch(
      DBI::dbExecute(
        conn,
        "UPDATE sft_fields SET args_json = ? WHERE form_id = ? AND field_id = ? AND (args_json IS NULL OR args_json <> ?)",
        params = list(args, form$form_id, field$id, args)
      ),
      error = function(e) NULL
    )
  }
  invisible(NULL)
}

# How many of each item the live baskets sharing the stock of `field` hold
# (lines kept on site do not count). `exclude_id` leaves out that record of
# `form`: all of its baskets, or only the `exclude_column` one. On MariaDB
# the read takes the newest committed rows (LOCK IN SHARE MODE), not the
# transaction's snapshot.
sgt_cart_used <- function(conn, form, field, exclude_id = NULL, exclude_column = NULL) {
  users <- sgt_cart_stock_users(conn, form, field)
  counts <- list()

  for (i in seq_len(nrow(users))) {
    sql <- paste0(
      "SELECT ", sgt_cart_quote(conn, users$column[i]),
      " AS cart FROM ", sgt_cart_quote(conn, users$table[i]),
      " WHERE sft_is_deleted = 0"
    )
    params <- list()

    if (!is.null(exclude_id) && length(exclude_id) == 1L && !is.na(exclude_id) &&
        identical(users$table[i], form$table_name) &&
        (is.null(exclude_column) || identical(users$column[i], exclude_column))) {
      sql <- paste0(sql, " AND sft_id <> ?")
      params <- list(as.integer(exclude_id))
    }

    if (identical(db_backend(conn), "mariadb") && in_transaction(conn)) {
      sql <- paste0(sql, " LOCK IN SHARE MODE")
    }

    # A form registered once whose table is gone holds nothing. Any other
    # read error must stop the save: counted as 0 it would oversell.
    if (!identical(users$table[i], form$table_name) && !table_exists(conn, users$table[i])) {
      next
    }
    rows <- sgt_cart_query(conn, sql, params)
    counts[[length(counts) + 1L]] <- sgt_cart_counts(rows$cart)
  }

  counts <- do.call(rbind, counts)
  if (is.null(counts) || nrow(counts) == 0L) {
    return(numeric())
  }

  totals <- tapply(counts$n, counts$item, sum)
  stats::setNames(as.numeric(totals), names(totals))
}

# The palette with what is still available to the basket `field` of the
# record `record_id` (NULL for a new one).
sgt_cart_available_items <- function(conn, form, field, record_id = NULL) {
  items <- sgt_cart_catalog_items(field, conn)

  if (is.null(items) || nrow(items) == 0L) {
    return(items)
  }

  used <- sgt_cart_used(conn, form, field, exclude_id = record_id, exclude_column = field$db_column)
  taken <- unname(used[items$id])
  taken[is.na(taken)] <- 0
  items$available <- ifelse(is.na(items$stock), NA_real_, pmax(items$stock - taken, 0))
  items
}

# A record's baskets as item counts: `record` values where given, the
# stored row of `current_id` for the others.
sgt_cart_record_counts <- function(fields, record, stored) {
  counts <- lapply(fields, function(field) {
    value <- record_value(record, field)
    if (is.null(value) && !is.null(stored)) {
      value <- stored[[field$db_column]]
    }
    if (is.null(value)) {
      return(NULL)
    }
    cart <- if (is.character(value)) sgt_cart_decode(value) else sgt_cart_normalize(value)
    if (is.null(cart) || nrow(cart) == 0L) {
      return(NULL)
    }
    cart <- cart[!cart$on_site, , drop = FALSE]
    if (nrow(cart) == 0L) {
      return(NULL)
    }
    data.frame(item = cart$item, n = cart$n, label = cart$label, field = field$id, stringsAsFactors = FALSE)
  })
  counts <- do.call(rbind, counts)
  if (is.null(counts)) {
    counts <- data.frame(item = character(), n = numeric(), label = character(), field = character())
  }
  counts
}

# Validation: the baskets must not take more of an item than its stock
# leaves. The stock is shared by every basket that uses the same catalog,
# in this record, in other records and in other forms. Only increases are
# checked, so a record that holds more than a lowered stock can still be
# saved unchanged. Needs `conn`; skipped without one.
# The input type's hooks (see register_input()): validation of all basket
# fields of a record at once, and the module parts inside form_server().
sgt_cart_validate <- function(form, fields, record, conn = NULL, current_id = NULL) {
  sgt_cart_issues(form, record, conn = conn, current_id = current_id)
}

sgt_cart_server <- function(input, output, session, state, fields) {
  sgt_register_cart_palettes(input, output, session, state)
  sgt_register_cart_board(input, output, session, state)
  invisible(list())
}

sgt_cart_issues <- function(form, record, conn = NULL, current_id = NULL) {
  if (is.null(conn)) {
    return(list())
  }

  fields <- Filter(sgt_is_cart_field, form_fields(form))
  given <- Filter(function(field) !is.null(record_value(record, field)), fields)
  if (length(given) == 0L) {
    return(list())
  }

  keys <- vapply(fields, function(field) sgt_cart_stock_key(form, field), character(1))
  issues <- list()

  # A deleted record (being restored) holds nothing.
  stored <- NULL
  if (!is.null(current_id)) {
    stored <- sgt_cart_query(
      conn,
      paste0("SELECT * FROM ", sgt_cart_quote(conn, form$table_name),
             " WHERE sft_id = ? AND sft_is_deleted = 0"),
      list(as.integer(current_id))
    )
    stored <- if (nrow(stored) == 1L) stored else NULL
  }

  for (key in unique(vapply(given, function(field) sgt_cart_stock_key(form, field), character(1)))) {
    group <- fields[keys == key]
    field <- group[[1L]]
    wanted <- sgt_cart_record_counts(group, record, stored)
    if (nrow(wanted) == 0L) {
      next
    }

    items <- sgt_cart_catalog_items(field, conn)
    if (is.null(items)) {
      next
    }

    # Short inside a transaction: when the wait is a cross-wait with a row
    # this transaction holds, the retry (see sgt_cart_lock()) resolves it.
    release <- sgt_cart_lock(conn, key, timeout = if (in_transaction(conn)) 3 else 10)
    if (in_transaction(conn)) {
      after_transaction(conn, release)
    } else {
      on.exit(release(), add = TRUE)
    }

    before <- if (!is.null(stored)) sgt_cart_record_counts(group, list(), stored) else wanted[0, ]
    wanted_n <- tapply(wanted$n, wanted$item, sum)
    before_n <- tapply(before$n, before$item, sum)
    used <- sgt_cart_used(conn, form, field, exclude_id = current_id)

    for (item in names(wanted_n)) {
      had <- if (is.na(before_n[item])) 0 else before_n[item]
      if (wanted_n[[item]] <= had) {
        next
      }
      row <- match(item, items$id)
      where <- wanted[wanted$item == item, , drop = FALSE]
      label <- group[[match(where$field[1L], vapply(group, function(f) f$id, character(1)))]]$label
      if (is.na(row)) {
        message <- form_message(form, "cart_item_gone", list(label = label, item = where$label[1L]))
      } else if (!is.na(items$stock[row])) {
        taken <- if (is.na(used[item])) 0 else used[item]
        free <- max(items$stock[row] - taken, 0)
        if (wanted_n[[item]] <= free) {
          next
        }
        message <- form_message(form, "cart_not_enough", list(
          label = label, item = items$label[row], available = free
        ))
      } else {
        next
      }
      issues <- c(issues, list(validation_issue(fields = unique(where$field), message = message, source = "cart")))
    }
  }

  issues
}

# Two sessions taking the last item at the same moment: on MariaDB each
# check waits for the other's commit (GET_LOCK per stock, see
# sgt_cart_stock_key(); released when the transaction ends). SQLite and
# DuckDB refuse the second writer themselves, and the retry checks again.
# Returns a function that releases the lock.
sgt_cart_lock <- function(conn, key, timeout = 10) {
  if (!identical(db_backend(conn), "mariadb")) {
    return(function() invisible(NULL))
  }

  name_sql <- "CONCAT('sft_cart:', LEFT(SHA1(CONCAT(DATABASE(), '.', ?)), 40))"
  got <- DBI::dbGetQuery(
    conn,
    paste0("SELECT GET_LOCK(", name_sql, ", ?) AS got"),
    params = list(key, timeout)
  )$got

  if (!isTRUE(as.integer(got) == 1L)) {
    # Retryable: when this transaction already holds a row the other writer
    # waits for (a with_transaction() block that wrote first), both would
    # wait until the timeout. Rolled back and run again, it gets through.
    stop(structure(
      class = c("sft_lock_timeout", "sft_write_conflict", "error", "condition"),
      list(
        message = paste0(
          "Someone else is saving a basket with the same items, and the write waited ",
          timeout, " seconds without getting through. Nothing was changed - please save again."
        ),
        call = NULL
      )
    ))
  }

  function() {
    try(
      DBI::dbGetQuery(
        conn,
        paste0("SELECT RELEASE_LOCK(", name_sql, ") AS released"),
        params = list(key)
      ),
      silent = TRUE
    )
    invisible(NULL)
  }
}

# Drop the catalog from the widget's arguments; a fixed palette is shown with
# its stock until the module sends what is available.
sgt_cart_prepare_args <- function(args) {
  args$catalog <- NULL
  args
}

# form_server(): when a dialog opens, the palette of every basket field gets
# the items and what each still has available for this record.
sgt_register_cart_palettes <- function(input, output, session, state) {
  fields <- Filter(sgt_is_cart_field, form_fields(state$form))

  if (length(fields) == 0L) {
    return(invisible(list()))
  }

  send <- function(prefix, only = NULL) {
    state$guard(function() {
      record_id <- if (identical(prefix, "edit_")) {
        row <- shiny::isolate(state$current_edit_row())
        if (!is.null(row)) row$sft_id[1L]
      }
      conn <- state$conn()
      for (field in fields) {
        if (!is.null(only) && !identical(field$id, only)) {
          next
        }
        items <- sgt_cart_available_items(conn, state$form, field, record_id = record_id)
        if (!is.null(items)) {
          update_cart_input(session, paste0(prefix, field$id), items = items)
        }
      }
    })
  }

  for (prefix in c("add_", "edit_")) {
    local({
      local_prefix <- prefix
      open_id <- if (identical(prefix, "add_")) "open_add" else "open_edit"

      shiny::observeEvent(input[[open_id]], send(local_prefix), ignoreInit = TRUE, priority = -10)

      # Asked for by the field itself once it is on the page (see
      # sgt-cart.js): the inline layout draws it after the open event.
      for (field in fields) {
        local({
          local_field <- field$id
          shiny::observeEvent(
            input[[paste0(local_prefix, local_field, "_palette")]],
            send(local_prefix, only = local_field),
            ignoreInit = TRUE
          )
        })
      }
    })
  }

  invisible(list())
}
