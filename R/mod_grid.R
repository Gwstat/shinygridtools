# Grid entry over several records: grid_ui() / grid_server(). Wide layout:
# one stored record per row of the grid, matched by a row key (and a group
# key that names the set of records the grid shows); the value columns are
# numeric fields of the form. Long layout (`col_key`): one stored record per
# CELL, matched by row key and column key, holding one number (`value`).
# Saving goes through upsert_records() in one transaction.

sgt_grid_empty_row <- function(values) {
  all(unlist(values) %in% c(0, NA))
}

# A reactive, a function or a plain value.
sgt_grid_resolve <- function(x) {
  if (is.function(x)) x() else x
}

# The rows of the grid as a data frame with `value` (the row key stored in
# the record) and `label` (shown in the grid).
sgt_grid_row_spec <- function(rows) {
  if (is.data.frame(rows)) {
    if (!"value" %in% names(rows)) {
      stop("rows given as a data frame need a `value` column.", call. = FALSE)
    }
    label <- if ("label" %in% names(rows)) rows$label else rows$value
    return(data.frame(value = as.character(rows$value), label = as.character(label), stringsAsFactors = FALSE))
  }

  # Names first: as.character() drops them, and they are the labels.
  labels <- names(rows)
  rows <- as.character(rows)

  if (is.null(labels)) {
    labels <- rows
  } else {
    labels[!nzchar(labels)] <- rows[!nzchar(labels)]
  }

  data.frame(value = unname(rows), label = unname(labels), stringsAsFactors = FALSE)
}

# The value fields (columns) of the grid: named explicitly, or every numeric
# input field that is neither the row key nor a group field.
sgt_grid_value_fields <- function(form, cols, exclude) {
  fields <- form_fields(form)
  ids <- vapply(fields, function(field) field$id, character(1))

  if (is.null(cols)) {
    numeric_types <- c("numericInput", "sliderInput")
    keep <- vapply(fields, function(field) field$input_type %in% numeric_types, logical(1)) & !ids %in% exclude
    fields <- fields[keep]
  } else {
    unknown <- setdiff(cols, ids)
    if (length(unknown) > 0L) {
      stop("cols names fields the form does not have: ", paste(unknown, collapse = ", "), ".", call. = FALSE)
    }
    fields <- fields[match(cols, ids)]
  }

  if (length(fields) == 0L) {
    stop("The grid needs at least one value field (numeric input fields, or `cols`).", call. = FALSE)
  }

  fields
}

# The stored records of the group as the grid's matrix (rows x columns, NA
# where no record exists). Attributes: `exists`, a logical matrix of the same
# shape (a record is stored for the cell; in the wide layout the same for a
# whole row), and `duplicates`, the labels of rows (or cells) with more than
# one live record (the grid shows the first; a restore can cause this).
sgt_grid_load <- function(conn, form, group_values, row_spec, key_field, value_fields) {
  stored <- find_records_by_key(form, group_values, conn)
  out <- matrix(
    NA_real_, nrow = nrow(row_spec), ncol = length(value_fields),
    dimnames = list(row_spec$label, vapply(value_fields, function(field) field$label, character(1)))
  )
  exists <- rep(FALSE, nrow(row_spec))
  duplicates <- character()

  if (nrow(stored) > 0L) {
    position <- match(as.character(stored[[key_field$db_column]]), row_spec$value)
    counts <- tabulate(position[!is.na(position)], nbins = nrow(row_spec))
    duplicates <- row_spec$label[counts > 1L]
    first <- !is.na(position) & !duplicated(position)

    for (j in seq_along(value_fields)) {
      column <- value_fields[[j]]$db_column
      ok <- first & !is.na(stored[[column]])
      out[position[ok], j] <- as.numeric(stored[[column]][ok])
    }

    exists[position[first]] <- TRUE
  }

  attr(out, "exists") <- matrix(rep(exists, ncol(out)), nrow = nrow(out))
  attr(out, "duplicates") <- duplicates
  out
}

# The long layout: one record per cell, placed by row key and column key.
sgt_grid_load_long <- function(conn, form, group_values, row_spec, key_field, col_spec, col_field, value_field) {
  stored <- find_records_by_key(form, group_values, conn)
  out <- matrix(
    NA_real_, nrow = nrow(row_spec), ncol = nrow(col_spec),
    dimnames = list(row_spec$label, col_spec$label)
  )
  exists <- matrix(FALSE, nrow = nrow(row_spec), ncol = nrow(col_spec))
  duplicates <- character()

  if (nrow(stored) > 0L) {
    r <- match(as.character(stored[[key_field$db_column]]), row_spec$value)
    c <- match(as.character(stored[[col_field$db_column]]), col_spec$value)
    placed <- !is.na(r) & !is.na(c)
    cell <- ifelse(placed, paste(r, c), NA_character_)
    first <- placed & !duplicated(cell)
    repeated <- placed & duplicated(cell)
    duplicates <- unique(paste(row_spec$label[r[repeated]], col_spec$label[c[repeated]], sep = " / "))

    for (k in which(first)) {
      value <- stored[[value_field$db_column]][k]
      out[r[k], c[k]] <- if (is.na(value)) NA_real_ else as.numeric(value)
      exists[r[k], c[k]] <- TRUE
    }
  }

  attr(out, "exists") <- exists
  attr(out, "duplicates") <- duplicates
  out
}

# Mandatory input fields a grid record would be saved without: every insert
# would fail validation. Group, row key and value columns are all written.
sgt_grid_missing_mandatory <- function(form, written_ids) {
  mandatory <- Filter(function(field) isTRUE(field$mandatory), form_fields(form))
  setdiff(vapply(mandatory, function(field) field$id, character(1)), written_ids)
}

# What saving row i asks for: "add", "edit", "delete" or "none".
sgt_grid_row_action <- function(existed, now_empty) {
  if (existed) {
    if (now_empty) "delete" else "edit"
  } else {
    if (now_empty) "none" else "add"
  }
}

# Two cell values are the same number: both empty, or equal up to float
# noise. A cell shows a stored double with 15 significant digits, so 0.1 * 3
# comes back from the browser as 0.3; compared exactly, the row counted as
# changed forever. The tolerance is relative and tiny: 1e9 vs 1e9 + 1 still
# differ.
sgt_grid_same <- function(a, b) {
  both_na <- is.na(a) & is.na(b)
  both_set <- !is.na(a) & !is.na(b)
  close <- both_set & abs(a - b) <= 1e-12 * pmax(1, abs(a), abs(b))
  both_na | close
}

# Cells whose stored value differs from what the grid shows, as the patch the
# client applies: list(row, col, shown, stored), 0-based, NULL for empty.
sgt_grid_patch <- function(shown, stored) {
  shown <- unname(shown)
  stored <- unname(stored)
  out <- list()

  for (i in seq_len(nrow(stored))) {
    for (j in seq_len(ncol(stored))) {
      a <- shown[i, j]
      b <- stored[i, j]

      if (!sgt_grid_same(a, b)) {
        out[[length(out) + 1L]] <- list(
          i - 1L, j - 1L,
          if (is.na(a)) NULL else a,
          if (is.na(b)) NULL else b
        )
      }
    }
  }

  out
}

# Sends a patch to the grid in the browser; its own function so tests can
# watch it (the test session has no sendInputMessage).
sgt_grid_send_patch <- function(session, input_id, message) {
  session$sendInputMessage(input_id, message)
}

#' Grid entry over several records
#'
#' A grid whose rows are records of a form and whose columns are the form's
#' numeric fields, for entering many related records at once: the species seen at
#' one survey site, the items of one order. Each row of the grid is one
#' stored record, identified by a row key (`key`) and, usually, a group key
#' (`group`) that names the set of records the grid shows. Saving writes the
#' rows the user changed through [shinyformtools::upsert_records()] in one transaction: a new
#' row is inserted, a changed row updated, a row emptied soft-deleted (so the
#' table holds only what was entered). Rows the user did not touch are not
#' written, and stored records of the group that the grid does not list stay
#' as they are.
#'
#' The grid itself is [grid_input()]: Enter and the arrow keys walk the cells,
#' a block from a spreadsheet can be pasted, sums follow the entries. It is
#' rendered once per group (and whenever `rows` change), never on save, so the
#' cursor stays where it is while values are written half a second after the
#' last keystroke (`autosave = TRUE`) or on a Save button.
#'
#' Two people editing the same group at the same time do not overwrite each
#' other. A save writes only the cells the user changed, and each only if it
#' is still stored the way the grid showed it; a row the user emptied is
#' deleted only if none of its cells changed meanwhile. A row that fails this
#' check is left as it is, the other rows are saved, the user is told which
#' row someone else changed, and the grid shows the current values. Every
#' write is audit-logged per record like any other.
#'
#' With `poll`, the grid checks every few seconds whether the group's stored
#' records changed and shows what others saved, in every cell the user has not
#' changed and is not typing in. The check is one small query per session;
#' it works across R processes, since it asks the database.
#'
#' `permissions` takes the same entries as in [shinyformtools::form_server()] (`can_add`,
#' `can_edit`, `can_delete`, `editable_fields`, each a value or a function),
#' and fields whose `editable` is `FALSE` or a function that refuses the
#' current user show as locked columns. A save that would need a permission
#' the user lacks is refused as a whole and the grid returns to the stored
#' values. A row the form's validation refuses (a rule, a unique value) is
#' named in a notification and stays in the grid as typed; the other rows of
#' the save are stored. Every mandatory field of the form must be written by
#' the grid (as group, row key or column); otherwise the grid shows an error instead of
#' failing on every save.
#'
#' @param id Module id.
#' @param label Optional label above the grid.
#' @param autosave Logical. `TRUE` saves half a second after the last change;
#'   `FALSE` shows a Save button instead. Give `grid_ui()` and `grid_server()`
#'   the same value.
#' @param width Optional CSS width of the grid.
#' @param form Object created with [shinyformtools::form()]. Its fields hold the row key, the
#'   group fields and the numeric value fields.
#' @param rows The rows of the grid: a character vector of row keys (names,
#'   when given, are the labels shown), a data frame with `value` and `label`
#'   columns, or a function / reactive returning one of these.
#' @param key Field id of the row key, for example `"species"`.
#' @param group Named list of field values that identify the group the grid
#'   shows, for example `list(visit = "1", site = "12")`, or a function /
#'   reactive returning one. `NULL` means the grid covers the whole table.
#' @param cols Wide layout: field ids of the value columns, in this order.
#'   Default: every numeric input field that is neither the row key nor a
#'   group field. Long layout (`col_key` given): the columns of the grid, as
#'   for `rows`: the values of the column key (names, when given, are the
#'   labels shown), a data frame with `value` and `label`, or a function /
#'   reactive returning one.
#' @param col_key Optional field id whose values are the grid's columns, for
#'   data stored long: one record per cell (a species' count in one distance band),
#'   matched by `group`, `key` and `col_key`, holding its number in `value`.
#'   An emptied cell's record is soft-deleted.
#' @param value With `col_key`: the field id of the number each record holds.
#' @param conn Optional DBI connection; see [shinyformtools::connections]. Without one the
#'   module opens its own for the session.
#' @param user Optional user identifier or function returning it, for the
#'   audit log and the per-user `editable` functions.
#' @param permissions Optional named list: `can_add`, `can_edit`, `can_delete`
#'   (logical or a function returning one; default `TRUE`) and
#'   `editable_fields` (field ids, or a function returning them; default
#'   `NULL`, no restriction), as in [shinyformtools::form_server()].
#' @param conflict_check Logical. When `TRUE` (default), as in
#'   [shinyformtools::form_server()], a changed cell is saved only if it is still stored the
#'   way the grid showed it. `FALSE`: the last save of a cell wins. Emptying a
#'   row is checked either way, so it never deletes what someone else entered.
#' @param poll Seconds between checks for records others saved, or `NULL` for
#'   none. Default 3.
#' @param hint Optional function of the group values returning a matrix of the
#'   grid's shape (or `NULL`) shown greyed inside the cells, for example the
#'   previous day's entries. A hint is never saved.
#' @param empty Function of a row's values (the value columns only) returning
#'   `TRUE` when the row counts as empty. Default: every cell 0 or `NA`.
#' @param sums Logical. Show row, column and total sums.
#' @param language Optional [shinyformtools::language()] object or function / reactive
#'   returning one.
#' @param labels Optional named list overriding the module's labels
#'   (`grid_save`, `grid_saving`, `grid_saved`, `grid_sum`).
#'
#' @return `grid_ui()` returns the UI. `grid_server()` returns a list with
#'   `changed` (a reactive counter incremented on every save that wrote
#'   something; pass it to a [shinyformtools::form_server()]'s `refresh_triggers` to keep a
#'   records table in step, with `table = list(refresh_delay = 1000)` so
#'   the table is rebuilt at most once a second while people type), `saved` (a reactive that fires after every successful save,
#'   also one that wrote nothing: a list with `group`, `rows` (key, label and
#'   action of the rows written), `empty` (no row holds anything), `confirmed`
#'   and `time`), `confirm(empty = FALSE)` (save the grid now and report it as
#'   confirmed, for a "done" button; with `empty = TRUE` every cell is cleared
#'   first, for "nothing found here"; rights and conflict checks apply as for
#'   any save), `value` (the grid's current matrix) and `reload()` (re-read
#'   the group from the database).
#' @examples
#' \dontrun{
#' library(shiny)
#' db <- db_sqlite(tempfile(fileext = ".sqlite"))
#' counts <- form(
#'   form_id = "counts", table_name = "counts", db = db,
#'   fields = list(
#'     form_field(id = "site", label = "Site"),
#'     form_field(id = "species", label = "Species"),
#'     form_field(id = "adults", label = "Adults", input_type = "numericInput"),
#'     form_field(id = "juveniles", label = "Juveniles", input_type = "numericInput")
#'   )
#' )
#' ui <- fluidPage(
#'   selectInput("site", "Site", c("Pond", "Meadow")),
#'   grid_ui("counts")
#' )
#' server <- function(input, output, session) {
#'   grid_server(
#'     "counts", counts,
#'     rows = c("Blackbird", "Robin", "Great tit"), key = "species",
#'     group = reactive(list(site = input$site)), user = "demo"
#'   )
#' }
#' shinyApp(ui, server)
#' }
#' @name grid_module
NULL

#' @rdname grid_module
#' @export
grid_ui <- function(id, label = NULL, autosave = TRUE, width = NULL) {
  ns <- shiny::NS(id)

  shiny::tagList(
    if (!is.null(label)) shiny::tags$label(class = "control-label", label),
    shiny::tags$div(
      style = if (!is.null(width)) paste0("width:", htmltools::validateCssUnit(width), ";"),
      shiny::uiOutput(ns("grid"))
    ),
    shiny::tags$div(
      class = "sgt-grid-bar",
      style = "margin-top: .4rem; display: flex; gap: .75rem; align-items: center;",
      if (!isTRUE(autosave)) shiny::uiOutput(ns("save_button"), inline = TRUE),
      shiny::textOutput(ns("status"), inline = TRUE)
    )
  )
}

#' @rdname grid_module
#' @export
grid_server <- function(id,
                        form,
                        rows,
                        key,
                        group = NULL,
                        cols = NULL,
                        col_key = NULL,
                        value = NULL,
                        conn = NULL,
                        user = NULL,
                        permissions = list(),
                        conflict_check = TRUE,
                        poll = 3,
                        hint = NULL,
                        empty = sgt_grid_empty_row,
                        autosave = TRUE,
                        sums = TRUE,
                        language = NULL,
                        labels = list()) {
  if (!inherits(form, "sft_form")) {
    stop("form must be a form object.", call. = FALSE)
  }

  if (!is.list(permissions)) {
    stop("permissions must be a named list.", call. = FALSE)
  }

  if (!is.null(poll) && (!is.numeric(poll) || length(poll) != 1L || is.na(poll) || poll <= 0)) {
    stop("poll must be NULL or a positive number of seconds.", call. = FALSE)
  }

  key_field <- key_fields(form, key)[[1L]]
  long <- !is.null(col_key)

  if (long) {
    if (is.null(value) || is.null(cols)) {
      stop("A grid with col_key needs `value` (the number field) and `cols` (the columns).", call. = FALSE)
    }
    col_field <- key_fields(form, col_key)[[1L]]
    value_field <- key_fields(form, value)[[1L]]
  }
  # save() has a matrix called `value`; the field id goes by another name.
  value_id <- value

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Like form_server(): a connection that cannot be opened (a server at
    # max_connections) does not end the session; the next access tries again.
    live <- module_connection(session, form$db, conn)

    grid_labels <- function() with_language(language, ui_labels(labels))
    label <- function(name, values = list()) ui_label(name, values = values, labels = grid_labels()) %||% ""
    current_user <- function() if (is.function(user)) user() else user
    permission <- function(name) module_permission(permissions, name)

    group_values <- shiny::reactive({
      values <- sgt_grid_resolve(group)

      if (!is.null(values) && (!is.list(values) || is.null(names(values)))) {
        stop("group must be NULL or a named list of field values.", call. = FALSE)
      }

      values
    })

    row_spec <- shiny::reactive(sgt_grid_row_spec(sgt_grid_resolve(rows)))

    value_fields <- shiny::reactive({
      if (long) {
        list(value_field)
      } else {
        sgt_grid_value_fields(form, cols, exclude = c(key, names(group_values())))
      }
    })

    # The grid's columns: the value fields (wide), or the column key's values
    # (long). A data frame of `value` and `label` either way.
    col_spec <- shiny::reactive({
      if (long) {
        sgt_grid_row_spec(sgt_grid_resolve(cols))
      } else {
        fields <- value_fields()
        data.frame(
          value = vapply(fields, function(field) field$id, character(1)),
          label = vapply(fields, function(field) field$label, character(1)),
          stringsAsFactors = FALSE
        )
      }
    })

    # The stored records of a group as the grid's matrix, in either layout.
    load_stored <- function(conn, group, spec, fields, columns) {
      if (long) {
        sgt_grid_load_long(conn, form, group, spec, key_field, columns, col_field, value_field)
      } else {
        sgt_grid_load(conn, form, group, spec, key_field, fields)
      }
    }

    # Columns the current user may change: the field's own `editable` (a
    # function is resolved for this user) and permissions$editable_fields.
    editable_columns <- function(fields) {
      resolved <- resolve_editable(form, current_user())
      ids <- vapply(fields, function(field) field$id, character(1))
      ok <- vapply(ids, function(id) {
        field <- Filter(function(f) identical(f$id, id), resolved$fields)[[1L]]
        isTRUE(field$editable)
      }, logical(1))
      allowed <- module_editable_fields(permissions)

      if (!is.null(allowed)) {
        ok <- ok & ids %in% allowed
      }

      unname(ok)
    }

    loaded <- shiny::reactiveVal(NULL)
    changed <- shiny::reactiveVal(0L)
    status <- shiny::reactiveVal("")
    structure_tick <- shiny::reactiveVal(0L)
    # Identifies the rendered grid. A value still pending from the previous
    # grid (debounced keystrokes) carries the old token and is not saved into
    # the group now shown.
    token <- 0L
    # Group, rows and columns of the grid as rendered: a save writes what the
    # user saw, even when the group reactive has already moved on.
    rendered <- NULL
    # The grids rendered before, by token, with their baseline: a value typed
    # into a grid just before it was replaced (another group chosen within
    # the debounce) still arrives, and is saved to the group it was typed in.
    earlier <- list()

    load <- function() {
      # Like every read of the package: the schema is ensured (probe-gated).
      conn <- prepare_write(form, live())
      load_stored(conn, group_values(), row_spec(), value_fields(), col_spec())
    }

    output$grid <- shiny::renderUI({
      structure_tick()

      if (!is.null(rendered)) {
        earlier[[as.character(token)]] <<- list(rendered = rendered, loaded = shiny::isolate(loaded()))
        earlier <<- utils::tail(earlier, 5L)
      }

      fields <- value_fields()
      missing <- sgt_grid_missing_mandatory(
        form, c(names(group_values()), key, col_key, vapply(fields, function(field) field$id, character(1)))
      )

      if (length(missing) > 0L) {
        stop(
          "The grid cannot save records of this form: the mandatory field(s) ",
          paste(missing, collapse = ", "), " are not part of the grid. Add them ",
          "to `group` or `cols`, or make them optional.",
          call. = FALSE
        )
      }

      current <- load()
      loaded(current)
      status("")

      duplicates <- attr(current, "duplicates")
      if (length(duplicates) > 0L) {
        shiny::showNotification(
          label("grid_duplicates", list(rows = paste(duplicates, collapse = ", "))),
          type = "warning", duration = NULL
        )
      }

      hint_matrix <- if (!is.null(hint)) {
        h <- if (is.function(hint)) hint(group_values()) else hint
        if (!is.null(h)) sgt_grid_matrix(h, rownames(current), colnames(current), keep_extra = FALSE)
      }

      token <<- token + 1L
      columns <- col_spec()
      rendered <<- list(group = group_values(), spec = row_spec(), fields = fields, cols = columns)
      # Long: every column is the one value field.
      column_fields <- if (long) rep(fields, nrow(columns)) else fields
      locked <- which(!editable_columns(column_fields)) - 1L
      # Each column takes its field's min / max / step; a field without them
      # keeps the grid's defaults (min 0, step 1: whole counts).
      field_arg <- function(name, default) {
        vapply(column_fields, function(field) {
          value <- field$args[[name]]
          if (is.null(value) || length(value) != 1L) default else as.numeric(value)
        }, numeric(1))
      }
      tag <- grid_input(
        ns("cells"),
        value = current,
        hint = hint_matrix,
        min = field_arg("min", 0),
        max = field_arg("max", NA_real_),
        step = field_arg("step", 1),
        sums = sums,
        sum_label = label("grid_sum")
      )

      query <- htmltools::tagQuery(tag)
      query$find(".sgt-grid-input")$addAttrs(`data-token` = token)$resetSelected()
      # Locked columns show their values but take no input.
      query$find("input.sgt-grid-cell")$each(function(cell, i) {
        if (as.integer(cell$attribs[["data-c"]]) %in% locked) {
          cell$attribs$disabled <- NA
        }
      })

      query$allTags()
    })

    refuse <- function(name, rows) {
      shiny::showNotification(label(name, list(rows = paste(rows, collapse = ", "))), type = "error", duration = 10)
      status("")
      structure_tick(shiny::isolate(structure_tick()) + 1L)
      invisible(FALSE)
    }

    saved <- shiny::reactiveVal(NULL)

    # Tell the app what a save did, also when it wrote nothing: the app keeps
    # its own notion of "done" (a site visited and found empty is not a
    # site nobody visited), and only it knows what that means.
    report_saved <- function(value, rows_changed = integer(), actions = character(), confirmed = FALSE,
                             context = rendered) {
      spec <- context$spec
      field_ids <- vapply(context$fields, function(field) field$id, character(1))
      line_empty <- vapply(seq_len(nrow(value)), function(i) {
        line <- unname(value)[i, ]
        if (long) {
          all(vapply(line, function(v) isTRUE(empty(stats::setNames(list(v), field_ids))), logical(1)))
        } else {
          isTRUE(empty(stats::setNames(as.list(line), field_ids)))
        }
      }, logical(1))

      saved(list(
        group = context$group,
        rows = data.frame(
          key = spec$value[rows_changed],
          label = spec$label[rows_changed],
          action = if (length(actions)) actions else character(),
          stringsAsFactors = FALSE
        ),
        empty = all(line_empty),
        confirmed = isTRUE(confirmed),
        time = Sys.time()
      ))
    }

    save <- function(value, confirmed = FALSE) {
      if (is.null(value)) {
        return(invisible(FALSE))
      }

      # The grid the value was typed into: the one shown, or one replaced
      # moments ago (its last keystrokes still arrive), saved against the
      # baseline it had.
      sent_for <- attr(value, "token")
      is_current <- is.null(sent_for) || identical(sent_for, as.character(token))

      if (is_current) {
        context <- rendered
        previous <- loaded()
      } else {
        before <- earlier[[sent_for]]
        if (is.null(before)) {
          return(invisible(FALSE))
        }
        context <- before$rendered
        previous <- before$loaded
      }

      if (is.null(context)) {
        return(invisible(FALSE))
      }

      spec <- context$spec
      fields <- context$fields
      columns <- context$cols

      if (is.null(previous) || !identical(dim(value), c(nrow(spec), nrow(columns)))) {
        return(invisible(FALSE))
      }

      field_ids <- vapply(fields, function(field) field$id, character(1))
      seen_seq <- suppressWarnings(as.integer(attr(value, "seq") %||% NA))
      value <- unname(value)
      shown <- unname(previous)

      # A value read before a later patch reached the browser still has the
      # old numbers in the patched cells; the browser shows the new ones, and
      # so does the baseline. Those cells are taken from the baseline, or
      # the save would write the old numbers back over someone else's.
      if (!is.na(seen_seq) && is_current) {
        for (entry in applied) {
          if (identical(entry$token, token) && entry$seq > seen_seq) {
            for (cell in entry$cells) {
              value[cell[1L], cell[2L]] <- shown[cell[1L], cell[2L]]
            }
          }
        }
      }
      existed <- attr(previous, "exists")

      differs <- function(a, b) !all(sgt_grid_same(a, b))
      changed_cell <- matrix(!sgt_grid_same(value, shown), nrow = nrow(value))
      rows_changed <- which(rowSums(changed_cell) > 0L)

      if (length(rows_changed) == 0L) {
        if (isTRUE(confirmed)) {
          report_saved(value, confirmed = TRUE, context = context)
          status(label("grid_saved", list(time = format(Sys.time(), "%H:%M:%S"))))
        }
        return(invisible(TRUE))
      }

      as_values <- function(line) stats::setNames(as.list(line), field_ids)
      group <- context$group

      # The units a save writes, each one record: a row of the grid (wide), or
      # a cell (long). A wide row the user emptied is sent whole and checked
      # whole: its record is deleted, so nothing anyone else entered in it may
      # go with it. Any other wide row carries only the cells this user changed
      # and is checked cell by cell, so two people typing into different cells
      # of one row do not collide. A long cell is its own record anyway.
      units <- list()
      if (long) {
        for (i in rows_changed) {
          for (j in which(changed_cell[i, ])) {
            units[[length(units) + 1L]] <- list(
              row = i, cols = j, existed = existed[i, j],
              action = sgt_grid_row_action(existed[i, j], isTRUE(empty(as_values(value[i, j])))),
              label = paste(spec$label[i], columns$label[j], sep = " / ")
            )
          }
        }
      } else {
        for (i in rows_changed) {
          action <- sgt_grid_row_action(existed[i, 1L], isTRUE(empty(as_values(value[i, ]))))
          units[[length(units) + 1L]] <- list(
            row = i,
            cols = if (identical(action, "delete")) seq_along(field_ids) else which(changed_cell[i, ]),
            existed = existed[i, 1L], action = action, label = spec$label[i]
          )
        }
      }
      unit_labels <- vapply(units, function(u) u$label, character(1))

      # Permissions, checked here and enforced by refusing the whole save: a
      # grid has no half-saved state the user could make sense of.
      needed <- c(add = "can_add", edit = "can_edit", delete = "can_delete")
      column_fields <- if (long) rep(fields, nrow(columns)) else fields
      columns_ok <- editable_columns(column_fields)
      unit_ok <- vapply(units, function(u) {
        (u$action == "none" || permission(needed[[u$action]])) &&
          all(columns_ok[which(changed_cell[u$row, ])[which(changed_cell[u$row, ]) %in% u$cols]])
      }, logical(1))

      if (!all(unit_ok)) {
        return(refuse("grid_not_allowed", unique(unit_labels[!unit_ok])))
      }

      status(label("grid_saving"))

      entry <- function(u) {
        i <- u$row
        cols <- u$cols
        seen <- if (u$existed) shown[i, cols] else rep(NA_real_, length(cols))
        check <- identical(u$action, "delete") || isTRUE(conflict_check)
        # A user who may add but not edit writes a unit only if it still has
        # no record: someone may have created it since the grid loaded.
        add_only <- identical(u$action, "add") && !permission("can_edit")
        cells <- if (long) {
          c(stats::setNames(list(columns$value[cols]), col_key), stats::setNames(list(value[i, cols]), value_id))
        } else {
          stats::setNames(as.list(value[i, cols]), field_ids[cols])
        }
        seen_values <- if (long) stats::setNames(list(seen), value_id) else stats::setNames(as.list(seen), field_ids[cols])

        list(
          record = c(group, stats::setNames(list(spec$value[i]), key), cells),
          expected = if (add_only) NA else if (check) seen_values,
          empty = identical(u$action, "delete")
        )
      }

      todo <- which(vapply(units, function(u) u$action != "none", logical(1)))
      batch <- lapply(units[todo], entry)
      batch_units <- units[todo]
      conflicted <- character()
      failed <- character()
      failed_units <- list()
      result <- data.frame(row = integer(), action = character(), sft_id = integer())
      upsert_key <- c(names(group), key, col_key)

      # A unit someone else changed in the meantime is taken out and the rest
      # written again, so one collision does not cost the user every other
      # row of this save. Each round removes a unit, so this ends.
      save_conn <- live()
      while (length(batch) > 0L) {
        result <- tryCatch(
          with_language(language, upsert_records(
            # As in form_server(): per-user editable functions resolved, so
            # the update path keeps the fields this user may change.
            resolve_editable(form, current_user()),
            lapply(batch, function(item) item$record),
            key = upsert_key,
            conn = save_conn,
            user = current_user(),
            empty = vapply(batch, function(item) item$empty, logical(1)),
            expected = lapply(batch, function(item) item$expected)
          )),
          error = function(e) e
        )

        if (inherits(result, "sft_edit_conflict") && !is.null(result$row)) {
          conflicted <- c(conflicted, batch_units[[result$row]]$label)
          batch <- batch[-result$row]
          batch_units <- batch_units[-result$row]
          result <- data.frame(row = integer(), action = character(), sft_id = integer())
          next
        }

        # A row the rules refuse (a negative count) is taken out the same way:
        # it stays in the grid with its message, the valid rows are stored.
        # Refusing the whole save lost them without a word once the user
        # switched groups.
        if (inherits(result, "error") && !is.null(result$row) && !is.null(result$row_message)) {
          failed <- c(failed, paste0(batch_units[[result$row]]$label, ": ", result$row_message))
          failed_units <- c(failed_units, batch_units[result$row])
          batch <- batch[-result$row]
          batch_units <- batch_units[-result$row]
          result <- data.frame(row = integer(), action = character(), sft_id = integer())
          next
        }

        break
      }

      if (inherits(result, "error")) {
        status("")
        shiny::showNotification(conditionMessage(result), type = "error", duration = 8)
        return(invisible(FALSE))
      }

      # Refused rows, named in the grid's words instead of upsert_records()'
      # "Row 4 (site = 1, species = ...)".
      if (length(failed) > 0L) {
        shiny::showNotification(paste(failed, collapse = "\n"), type = "error", duration = 8)
      }

      # Inside the caller's with_transaction() the write is stored only when
      # that block commits; until then the grid keeps its old baseline, so a
      # rolled-back or retried block writes the values again.
      stored <- function(fun) after_commit(save_conn, fun)

      if (any(result$action %in% c("insert", "update", "delete"))) {
        stored(function() changed(changed() + 1L))
      }

      if (length(conflicted) > 0L) {
        # The rest is stored; the grid reloads so the user sees the current
        # values of the rows someone else changed.
        return(refuse("grid_conflict", unique(conflicted)))
      }

      # What the grid shows now is what is stored: the new baseline.
      now_exists <- existed
      written <- batch_units[result$row]
      for (k in seq_along(written)) {
        stored_now <- result$action[k] %in% c("insert", "update", "unchanged")
        if (long) {
          now_exists[written[[k]]$row, written[[k]]$cols] <- stored_now
        } else {
          now_exists[written[[k]]$row, ] <- stored_now
        }
      }
      # A unit that only holds empty values (a 0) wrote nothing: its baseline
      # is what is stored, so the next poll does not blank the cell.
      for (u in units[vapply(units, function(u) u$action == "none", logical(1))]) {
        if (long) {
          value[u$row, u$cols] <- NA
        } else {
          value[u$row, ] <- NA
        }
      }
      # A refused row keeps its stored values as baseline: the grid still shows
      # what the user typed, and the next save sends it again.
      for (u in failed_units) {
        if (long) {
          value[u$row, u$cols] <- unname(previous)[u$row, u$cols]
        } else {
          value[u$row, ] <- unname(previous)[u$row, ]
        }
      }
      dimnames(value) <- dimnames(previous)
      attr(value, "exists") <- now_exists
      attr(value, "duplicates") <- character()
      sent <- vapply(written, function(u) u$row, integer(1))
      stored(function() {
        if (is_current) {
          loaded(value)
        } else {
          earlier[[sent_for]]$loaded <<- value
        }
        report_saved(value, rows_changed = sent, actions = result$action,
                     confirmed = confirmed && length(failed) == 0L, context = context)
        if (length(failed) == 0L) {
          status(label("grid_saved", list(time = format(Sys.time(), "%H:%M:%S"))))
        } else {
          status("")
        }
      })
      invisible(length(failed) == 0L)
    }

    shiny::observeEvent(input$cells, {
      if (isTRUE(autosave)) {
        save(input$cells)
      } else {
        status("")
      }
    })

    # Keystrokes a replaced grid still had in its debounce (see
    # unsubscribe() in sgt-grid.js): saved to the group they were typed in.
    shiny::observeEvent(input$cells_late, {
      if (isTRUE(autosave)) {
        save(input$cells_late)
      }
    })

    # Records others saved: poll a fingerprint of the group, and when it
    # changes send the differing cells to the browser. It applies a cell only
    # if it still shows the old value and is not focused, and reports which
    # cells it applied; only those move the baseline, so a cell the user
    # never saw the new value of stays under the conflict check.
    # One patch is outstanding at a time and numbered. The browser answers
    # with the cells it took, and every value it sends carries the number of
    # the last patch it saw. A value that was read before a patch (a
    # keystroke still inside the 500 ms debounce) must not undo that patch:
    # save() puts the patched values back into such a value (see `applied`).
    pending <- NULL
    patch_seq <- 0L
    applied <- list()

    if (!is.null(poll)) {
      # The poll uses the handle it has; only a failing query goes through
      # the probe, which is one more round trip per poll per session.
      poll_conn <- function() live(probe = FALSE)

      stamp <- shiny::reactivePoll(
        poll * 1000, session,
        checkFunc = function() {
          if (is.null(rendered)) {
            return(NULL)
          }
          tryCatch(
            records_stamp(form, rendered$group, poll_conn()),
            error = function(e) {
              tryCatch(records_stamp(form, rendered$group, live()), error = function(e) NULL)
            }
          )
        },
        valueFunc = function() Sys.time()
      )

      shiny::observeEvent(stamp(), ignoreInit = TRUE, {
        previous <- loaded()

        if (is.null(rendered) || is.null(previous)) {
          return()
        }

        # Still waiting for the browser's answer to the last patch; a patch
        # older than 10 s had no answer (the grid was re-rendered) and is
        # dropped.
        if (!is.null(pending) && difftime(Sys.time(), pending$sent, units = "secs") < 10) {
          return()
        }

        stored <- tryCatch(
          load_stored(poll_conn(), rendered$group, rendered$spec, rendered$fields, rendered$cols),
          error = function(e) NULL
        )

        if (is.null(stored)) {
          return()
        }

        patch <- sgt_grid_patch(previous, stored)

        if (length(patch) == 0L) {
          pending <<- NULL
          return()
        }

        patch_seq <<- patch_seq + 1L
        pending <<- list(token = token, seq = patch_seq, stored = stored, patch = patch, sent = Sys.time())
        sgt_grid_send_patch(session, "cells", list(
          patch = patch, token = as.character(token), seq = patch_seq
        ))
      })

      shiny::observeEvent(input$cells_synced, {
        synced <- input$cells_synced
        previous <- loaded()

        # An answer for a grid the group switch has replaced: `loaded()`
        # already belongs to the new group.
        if (is.null(pending) || is.null(previous) ||
            !identical(as.character(synced$token), as.character(token)) ||
            !identical(as.character(synced$token), as.character(pending$token)) ||
            !identical(as.integer(synced$seq), pending$seq)) {
          return()
        }

        cells <- lapply(synced$applied, function(x) as.integer(unlist(x)) + 1L)
        stored <- unname(pending$stored)
        baseline <- previous
        exists <- attr(previous, "exists")
        applied_rows <- integer()

        for (cell in cells) {
          baseline[cell[1L], cell[2L]] <- stored[cell[1L], cell[2L]]
          applied_rows <- c(applied_rows, cell[1L])
        }

        # A long cell is its own record: it counts as stored as soon as it
        # reached the browser. A wide row does once every differing cell of it
        # reached the browser.
        stored_exists <- attr(pending$stored, "exists")
        if (long) {
          for (cell in cells) {
            exists[cell[1L], cell[2L]] <- stored_exists[cell[1L], cell[2L]]
          }
        } else {
          patched_rows <- vapply(pending$patch, function(p) p[[1L]] + 1L, integer(1))
          for (i in unique(applied_rows)) {
            if (sum(patched_rows == i) == sum(applied_rows == i)) {
              exists[i, ] <- stored_exists[i, ]
            }
          }
        }

        attr(baseline, "exists") <- exists
        loaded(baseline)
        applied[[length(applied) + 1L]] <<- list(seq = pending$seq, token = pending$token, cells = cells)
        if (length(applied) > 50L) {
          applied <<- utils::tail(applied, 50L)
        }
        pending <<- NULL
      })
    }

    output$save_button <- shiny::renderUI({
      shiny::actionButton(ns("save"), label("grid_save"), class = "btn-primary btn-sm")
    })

    shiny::observeEvent(input$save, {
      save(shiny::isolate(input$cells))
    })

    output$status <- shiny::renderText(status())

    list(
      changed = shiny::reactive(changed()),
      saved = shiny::reactive(saved()),
      confirm = function(empty = FALSE) {
        value <- if (isTRUE(empty)) {
          previous <- shiny::isolate(loaded())
          if (is.null(previous)) return(invisible(FALSE))
          # Clear the cells in the browser too; the save below writes it.
          sgt_grid_send_patch(session, "cells", list(clear = TRUE))
          out <- matrix(NA_real_, nrow = nrow(previous), ncol = ncol(previous))
          attr(out, "token") <- as.character(token)
          out
        } else {
          shiny::isolate(input$cells) %||% shiny::isolate(loaded())
        }
        shiny::isolate(save(value, confirmed = TRUE))
      },
      value = shiny::reactive(input$cells),
      reload = function() structure_tick(shiny::isolate(structure_tick()) + 1L)
    )
  })
}
