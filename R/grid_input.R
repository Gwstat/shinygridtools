# A grid of number cells as a Shiny input: rows and columns from the caller,
# keyboard navigation, block paste from a spreadsheet, live sums, and a
# matrix as the value. Assets in inst/assets/grid. The grid is also an
# input_type of shinyformtools::form_field(); see zzz.R for the registration.

sgt_grid_dependency <- function() {
  htmltools::htmlDependency(
    name = "sgt-grid",
    version = as.character(utils::packageVersion("shinygridtools")),
    src = c(file = system.file("assets", "grid", package = "shinygridtools")),
    script = "sgt-grid.js",
    stylesheet = "sgt-grid.css"
  )
}

# A numeric matrix with the given row and column names, from whatever the
# caller passed as `value` (NULL fills with NA). A value with row and column
# names is placed by name. Stored rows or columns the field no longer lists
# are appended when they hold numbers (`keep_extra`), so a renamed or removed
# row neither shows its numbers under another label nor loses them silently;
# the user sees them and can clear them. Hints (`keep_extra = FALSE`) drop
# them. A value without names is placed by position and must fit.
sgt_grid_matrix <- function(value, rows, cols, keep_extra = TRUE) {
  if (is.null(value)) {
    value <- matrix(NA_real_, nrow = length(rows), ncol = length(cols))
  }

  value <- as.matrix(value)
  storage.mode(value) <- "double"

  value_rows <- rownames(value)
  value_cols <- colnames(value)

  if (!is.null(value_rows) && !is.null(value_cols) &&
      !anyDuplicated(value_rows) && !anyDuplicated(value_cols)) {
    if (isTRUE(keep_extra)) {
      filled_rows <- value_rows[rowSums(!is.na(value)) > 0L]
      filled_cols <- value_cols[colSums(!is.na(value)) > 0L]
      rows <- c(rows, setdiff(filled_rows, rows))
      cols <- c(cols, setdiff(filled_cols, cols))
    }

    out <- matrix(NA_real_, nrow = length(rows), ncol = length(cols))
    r <- match(value_rows, rows)
    c <- match(value_cols, cols)
    out[r[!is.na(r)], c[!is.na(c)]] <- value[!is.na(r), !is.na(c), drop = FALSE]
    dimnames(out) <- list(rows, cols)
    return(out)
  }

  if (nrow(value) != length(rows) || ncol(value) != length(cols)) {
    stop(
      "value must be a ", length(rows), " x ", length(cols), " matrix, got ",
      nrow(value), " x ", ncol(value), ".",
      call. = FALSE
    )
  }

  dimnames(value) <- list(rows, cols)
  value
}

# A matrix as the client expects it: one array per row, NA as null.
sgt_grid_json_rows <- function(value) {
  lapply(seq_len(nrow(value)), function(i) {
    unname(lapply(value[i, ], function(x) if (is.na(x)) NULL else x))
  })
}

# Stored representation: one JSON object holding names and values, so a
# stored grid decodes to the same matrix regardless of the field's current
# `rows` / `cols` arguments.
sgt_grid_encode <- function(value) {
  if (is.null(value) || length(value) == 0L || (length(value) == 1L && is.na(value))) {
    return(NA_character_)
  }

  value <- as.matrix(value)
  storage.mode(value) <- "double"

  as.character(jsonlite::toJSON(
    list(
      rows = rownames(value) %||% character(),
      cols = colnames(value) %||% character(),
      values = unname(value)
    ),
    digits = NA,
    na = "null"
  ))
}

sgt_grid_decode <- function(stored) {
  if (is.null(stored) || length(stored) == 0L || is.na(stored[1L]) || !nzchar(stored[1L])) {
    return(NULL)
  }

  # A value that is not a stored grid (text written by another tool, a field
  # switched to grid_input over old data) reads as no grid rather than
  # breaking the table for every row.
  parsed <- tryCatch(
    jsonlite::fromJSON(stored[1L], simplifyVector = TRUE, simplifyMatrix = TRUE),
    error = function(e) NULL
  )

  if (!is.list(parsed) || is.null(parsed$values)) {
    return(NULL)
  }

  rows <- as.character(parsed$rows)
  cols <- as.character(parsed$cols)
  values <- parsed$values

  if (is.null(values) || length(values) == 0L) {
    values <- matrix(NA_real_, nrow = length(rows), ncol = length(cols))
  } else if (is.list(values)) {
    # Ragged or null-holding rows come back as a list of rows.
    values <- do.call(rbind, lapply(values, function(line) {
      vapply(line, function(x) if (is.null(x) || is.na(x)) NA_real_ else as.numeric(x), numeric(1))
    }))
  }

  # A matrix stored without dimnames keeps its shape and stays unnamed; the
  # field's rows and cols then place it by position.
  if (length(rows) == 0L && length(cols) == 0L && length(values) > 0L) {
    values <- as.matrix(values)
    storage.mode(values) <- "double"
    return(unname(values))
  }

  # jsonlite hands a numeric matrix back in row order already; only its
  # storage mode and names are set here.
  values <- matrix(as.numeric(values), nrow = length(rows), ncol = length(cols))
  dimnames(values) <- list(rows, cols)
  values
}

# What the records table shows for a stored grid: the column totals.
sgt_grid_format <- function(value, sep = "; ") {
  m <- sgt_grid_decode(value)

  if (is.null(m)) {
    # Unreadable text is shown as it is, like other decoders do.
    if (length(value) > 0L && !is.na(value[1L])) {
      return(as.character(value[1L]))
    }
    return("")
  }

  totals <- colSums(m, na.rm = TRUE)
  names <- colnames(m) %||% paste0("#", seq_along(totals))
  paste(sprintf("%s %s", names, format(totals, trim = TRUE)), collapse = sep)
}

# Shiny input handler: the client's {rows, cols, values} becomes a matrix.
sgt_grid_input_handler <- function(value, shinysession = NULL, name = NULL) {
  if (is.null(value)) {
    return(NULL)
  }

  rows <- as.character(unlist(value$rows))
  cols <- as.character(unlist(value$cols))
  out <- matrix(NA_real_, nrow = length(rows), ncol = length(cols), dimnames = list(rows, cols))

  for (r in seq_along(value$values)) {
    line <- value$values[[r]]

    for (c in seq_along(line)) {
      cell <- line[[c]]

      if (!is.null(cell) && length(cell) == 1L && !is.na(cell)) {
        out[r, c] <- as.numeric(cell)
      }
    }
  }

  # Set by grid_server() on the grid it rendered, and the number of the last
  # patch the browser applied; see there.
  if (!is.null(value$token)) {
    attr(out, "token") <- as.character(unlist(value$token))
  }
  if (!is.null(value$seq)) {
    attr(out, "seq") <- as.integer(unlist(value$seq))
  }

  out
}

#' A grid of number cells as a Shiny input
#'
#' Renders a table whose rows and columns are fixed and whose cells are
#' number inputs, for entering many related values at once (a cross table of
#' counts, say). Enter and the arrow keys walk the cells, a block copied from
#' a spreadsheet is pasted from the focused cell onwards, and row, column and
#' total sums follow the entries. The input's value is a numeric matrix with
#' the row and column names; an empty cell is `NA`. The value updates half a
#' second after the last keystroke.
#'
#' `grid_input` is also an `input_type` for [shinyformtools::form_field()]
#' (registered when this package loads): the matrix is stored as one JSON text
#' with its row and column names and shown in the records table as its column
#' totals. Pass `rows`, `cols` and the other arguments through
#' `form_field(args = list(...))`. A stored grid is placed by
#' name, so `rows` can be reordered or extended later; stored rows or columns
#' the field no longer lists are shown after the others while they hold
#' numbers, never moved under another label.
#'
#' @param inputId Input id.
#' @param label Optional label above the grid.
#' @param value Numeric matrix of starting values, or `NULL` for empty cells.
#'   Its dimnames supply `rows` and `cols` when those are not given.
#' @param rows,cols Row and column labels.
#' @param hint Optional matrix of the same shape shown greyed inside each cell,
#'   for example yesterday's values. It is a hint only, never the value.
#' @param min,max,step Passed to the number cells: one value for all columns,
#'   or one per column (`NA` for none). A column whose step is a whole number
#'   reads a pasted "1.234" as 1234.
#' @param sums Logical. Show row, column and total sums.
#' @param sum_label Header of the sum column and row.
#' @param width Optional CSS width of the whole grid.
#'
#' @return A Shiny input tag.
#' @seealso [update_grid_input()]
#' @examples
#' \dontrun{
#' library(shiny)
#' ui <- fluidPage(
#'   grid_input("birds", "Birds counted", rows = c("Blackbird", "Robin"),
#'              cols = c("Adults", "Juveniles")),
#'   verbatimTextOutput("value")
#' )
#' server <- function(input, output, session) {
#'   output$value <- renderPrint(input$birds)
#' }
#' shinyApp(ui, server)
#' }
#' @export
grid_input <- function(inputId,
                       label = NULL,
                       value = NULL,
                       rows = rownames(value),
                       cols = colnames(value),
                       hint = NULL,
                       min = 0,
                       max = NULL,
                       step = 1,
                       sums = TRUE,
                       sum_label = "Sum",
                       width = NULL) {
  if (is.null(rows) || is.null(cols)) {
    stop("rows and cols are needed, either directly or as the dimnames of value.", call. = FALSE)
  }

  rows <- as.character(rows)
  cols <- as.character(cols)
  value <- sgt_grid_matrix(value, rows, cols)
  # A stored value may bring rows or columns the arguments no longer list.
  rows <- rownames(value)
  cols <- colnames(value)
  hint <- if (!is.null(hint)) sgt_grid_matrix(hint, rows, cols, keep_extra = FALSE)
  per_column <- function(x) {
    x <- if (is.null(x)) NA else x
    rep_len(x, length(cols))
  }
  min <- per_column(min)
  max <- per_column(max)
  step <- per_column(step)

  cell <- function(r, c) {
    x <- value[r, c]
    h <- if (!is.null(hint)) hint[r, c] else NA_real_

    shiny::tags$td(
      class = "sgt-grid-cell-wrap",
      shiny::tags$input(
        type = "number",
        class = paste(c("sgt-grid-cell", if (!is.na(x) && x != 0) "sgt-grid-filled"), collapse = " "),
        value = if (is.na(x)) "" else x,
        min = if (!is.na(min[c])) min[c],
        max = if (!is.na(max[c])) max[c],
        step = if (!is.na(step[c])) step[c] else "any",
        `data-r` = r - 1L, `data-c` = c - 1L,
        `aria-label` = paste(rows[r], cols[c])
      ),
      if (!is.na(h)) shiny::tags$span(class = "sgt-grid-hint", h)
    )
  }

  header <- shiny::tags$thead(shiny::tags$tr(
    shiny::tags$th(class = "sgt-grid-row-label", ""),
    lapply(cols, shiny::tags$th),
    if (isTRUE(sums)) shiny::tags$th(class = "sgt-grid-sum", sum_label)
  ))

  body <- shiny::tags$tbody(lapply(seq_along(rows), function(r) {
    shiny::tags$tr(
      shiny::tags$td(class = "sgt-grid-row-label", rows[r]),
      lapply(seq_along(cols), function(c) cell(r, c)),
      if (isTRUE(sums)) shiny::tags$td(class = "sgt-grid-sum", `data-sum-row` = r - 1L, "")
    )
  }))

  footer <- if (isTRUE(sums)) {
    shiny::tags$tfoot(shiny::tags$tr(
      shiny::tags$td(class = "sgt-grid-row-label", sum_label),
      lapply(seq_along(cols), function(c) shiny::tags$td(class = "sgt-grid-sum", `data-sum-col` = c - 1L, "")),
      shiny::tags$td(class = "sgt-grid-sum sgt-grid-total", "")
    ))
  }

  tag <- shiny::tags$div(
    class = "form-group shiny-input-container",
    style = if (!is.null(width)) paste0("width:", htmltools::validateCssUnit(width), ";"),
    if (!is.null(label)) shiny::tags$label(class = "control-label", `for` = inputId, label),
    shiny::tags$div(
      id = inputId,
      class = "sgt-grid-input",
      `data-rows` = as.character(jsonlite::toJSON(rows)),
      `data-cols` = as.character(jsonlite::toJSON(cols)),
      `data-sums` = tolower(isTRUE(sums)),
      shiny::tags$table(class = "sgt-grid", header, body, footer)
    )
  )

  htmltools::attachDependencies(tag, sgt_grid_dependency())
}

#' Update a grid input from the server
#'
#' @param session The Shiny session.
#' @param inputId Input id.
#' @param value Optional numeric matrix of the grid's shape; `NA` empties a
#'   cell, a single `NA` empties every cell.
#' @param hint Optional matrix of the same shape shown greyed inside the cells.
#'
#' @return Called for its side effect.
#' @seealso [grid_input()]
#' @export
update_grid_input <- function(session, inputId, value = NULL, hint = NULL) {
  message <- list()

  if (length(value) == 1L && is.na(value)) {
    message$clear <- TRUE
  } else if (!is.null(value)) {
    value <- as.matrix(value)
    storage.mode(value) <- "double"
    message$values <- sgt_grid_json_rows(value)
  }

  if (!is.null(hint)) {
    hint <- as.matrix(hint)
    storage.mode(hint) <- "double"
    message$hint <- sgt_grid_json_rows(hint)
  }

  if (length(message) > 0L) {
    session$sendInputMessage(inputId, message)
  }

  invisible(NULL)
}
