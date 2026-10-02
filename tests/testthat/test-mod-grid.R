counts_form <- function() {
  form(
    form_id = "counts", table_name = "counts",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "day", label = "Day"),
      form_field(id = "site", label = "Site"),
      form_field(id = "species", label = "Species"),
      form_field(id = "adults", label = "Adults", input_type = "numericInput"),
      form_field(id = "juveniles", label = "Juveniles", input_type = "numericInput"),
      form_field(id = "note", label = "Note")
    )
  )
}

test_that("the grid loads a group's records and saves the grid as records", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  insert_record(counts, list(day = "fri", site = "1", species = "B", adults = 3, juveniles = 1), conn = conn)
  insert_record(counts, list(day = "fri", site = "2", species = "A", adults = 9), conn = conn)
  site <- shiny::reactiveVal("1")

  shiny::testServer(
    grid_server,
    args = list(
      id = "g", form = counts, rows = c("Species A" = "A", "Species B" = "B"), key = "species",
      group = function() list(day = "fri", site = site()), conn = conn, user = "ada"
    ),
    {
      session$flushReact()
      html <- as.character(output$grid$html)
      # The value columns are the numeric fields; the text field is not one.
      expect_true(grepl("Adults", html, fixed = TRUE))
      expect_false(grepl("Note", html, fixed = TRUE))
      # Row B of site 1 came from the database; row A is empty.
      expect_true(grepl('value="3"[^>]*data-r="1" data-c="0"', html))

      # The same values again: nothing is written.
      session$setInputs(cells = matrix(c(NA, 3, NA, 1), nrow = 2))
      expect_identical(nrow(fetch_audit_log(counts, conn = conn)), 2L)
      expect_identical(session$returned$changed(), 0L)

      # A change: A gets values, B is emptied.
      session$setInputs(cells = matrix(c(5, 0, 2, 0), nrow = 2))
      stored <- fetch_records(counts, conn = conn)
      expect_identical(paste(stored$site, stored$species), c("2 A", "1 A"))
      expect_identical(stored$adults[stored$site == "1"], 5)
      expect_identical(session$returned$changed(), 1L)
      expect_match(output$status, "^Saved ")
      audit <- fetch_audit_log(counts, conn = conn)
      # fetch_audit_log() orders by record, so count instead of reading the tail.
      expect_identical(sum(audit$action == "delete"), 1L)
      expect_identical(sum(audit$action == "insert"), 3L)
      expect_identical(sort(unique(audit$changed_by)), "ada")

      # Switching the group re-renders the grid with that group's records.
      site("2")
      session$flushReact()
      expect_true(grepl('value="9"[^>]*data-r="0" data-c="0"', as.character(output$grid$html)))
      expect_identical(session$returned$value()[1, 1], 5)
    }
  )
})

test_that("without autosave the grid writes on the Save button only", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)

  shiny::testServer(
    grid_server,
    args = list(
      id = "g", form = counts, rows = c("A", "B"), key = "species",
      group = list(day = "fri", site = "1"), conn = conn, autosave = FALSE,
      language = german()
    ),
    {
      session$flushReact()
      session$setInputs(cells = matrix(c(1, 2, 3, 4), nrow = 2))
      expect_identical(nrow(fetch_records(counts, conn = conn)), 0L)
      expect_true(grepl("Speichern", as.character(output$save_button$html), fixed = TRUE))

      session$setInputs(save = 1)
      expect_identical(nrow(fetch_records(counts, conn = conn)), 2L)
      expect_match(output$status, "^Gespeichert ")
    }
  )
})

test_that("a refused row is reported by its label and the valid row is stored", {
  counts <- form(
    form_id = "counts", table_name = "counts",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "species", label = "Species"),
      form_field(id = "adults", label = "Adults", input_type = "numericInput")
    ),
    validation_rules = list(
      forbid_if("no_negatives", function(values) isTRUE(values$adults < 0), fields = "adults",
                message = "No negative counts.")
    )
  )
  conn <- local_test_conn(counts$db)
  shown <- NULL
  local_mocked_bindings(showNotification = function(ui, ...) shown <<- ui, .package = "shiny")

  shiny::testServer(
    grid_server,
    args = list(id = "g", form = counts, rows = c("A", "B"), key = "species", conn = conn),
    {
      session$flushReact()
      session$setInputs(cells = matrix(c(1, -2), nrow = 2))
      expect_identical(fetch_records(counts, conn = conn)$species, "A")
      # Named by its label, without upsert_records()' "Row 2 (species = B)".
      expect_identical(shown, "B: No negative counts.")
      expect_identical(session$returned$changed(), 1L)
    }
  )
})

test_that("hint, cols and rows as a data frame shape the grid", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)

  shiny::testServer(
    grid_server,
    args = list(
      id = "g", form = counts,
      rows = data.frame(value = c("A", "B"), label = c("Alpha", "Beta")), key = "species",
      group = list(day = "fri", site = "1"), cols = "juveniles", conn = conn,
      hint = function(group) matrix(c(7, 8), nrow = 2)
    ),
    {
      session$flushReact()
      html <- as.character(output$grid$html)
      expect_true(grepl("Alpha", html, fixed = TRUE))
      expect_true(grepl("Juveniles", html, fixed = TRUE))
      expect_false(grepl("Adults", html, fixed = TRUE))
      expect_identical(lengths(regmatches(html, gregexpr("sgt-grid-hint", html))), 2L)
    }
  )

  expect_error(sgt_grid_value_fields(counts, "nope", exclude = "species"), "does not have: nope")
  expect_error(sgt_grid_row_spec(data.frame(x = 1)), "need a `value` column")
})

grid_args <- function(counts, conn, ...) {
  utils::modifyList(
    list(id = "g", form = counts, rows = c("A", "B"), key = "species",
         group = list(day = "fri", site = "1"), conn = conn, user = "ada"),
    list(...),
    keep.null = TRUE
  )
}

test_that("a save writes only the rows the user changed", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  insert_record(counts, list(day = "fri", site = "1", species = "A", adults = 1), conn = conn)
  insert_record(counts, list(day = "fri", site = "1", species = "B", adults = 2), conn = conn)

  shiny::testServer(grid_server, args = grid_args(counts, conn), {
    session$flushReact()
    # Someone else changes B after this grid loaded it; this user edits A only.
    update_record(counts, list(adults = 7), record_id = 2, conn = conn, user = "bob")
    session$setInputs(cells = matrix(c(4, 2, NA, NA), nrow = 2))

    stored <- fetch_records(counts, conn = conn)
    expect_identical(stored$adults, c(4, 7))
    expect_identical(session$returned$changed(), 1L)
  })
})

test_that("a row someone else changed in the meantime is not overwritten", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  insert_record(counts, list(day = "fri", site = "1", species = "A", adults = 1), conn = conn)
  shown <- NULL
  local_mocked_bindings(showNotification = function(ui, ...) shown <<- ui, .package = "shiny")

  shiny::testServer(grid_server, args = grid_args(counts, conn), {
    session$flushReact()
    update_record(counts, list(adults = 7), record_id = 1, conn = conn, user = "bob")
    # Both rows change; A conflicts and keeps bob's value, B is still saved.
    session$setInputs(cells = matrix(c(4, 5, NA, NA), nrow = 2))

    stored <- fetch_records(counts, conn = conn)
    expect_identical(stored$adults, c(7, 5))
    expect_match(shown, "Someone else changed A")
    # The grid reloaded and shows the current value.
    session$flushReact()
    expect_true(grepl('value="7"[^>]*data-r="0" data-c="0"', as.character(output$grid$html)))
  })
})

test_that("a record someone else inserted in the meantime is not overwritten", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  shown <- NULL
  local_mocked_bindings(showNotification = function(ui, ...) shown <<- ui, .package = "shiny")

  shiny::testServer(grid_server, args = grid_args(counts, conn), {
    session$flushReact()
    insert_record(counts, list(day = "fri", site = "1", species = "A", adults = 9), conn = conn)
    session$setInputs(cells = matrix(c(4, NA, NA, NA), nrow = 2))

    expect_identical(fetch_records(counts, conn = conn)$adults, 9)
    expect_match(shown, "Someone else changed A")
  })
})

test_that("permissions and editable decide what the grid may write", {
  counts <- form(
    form_id = "counts", table_name = "counts",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "day", label = "Day"),
      form_field(id = "site", label = "Site"),
      form_field(id = "species", label = "Species"),
      form_field(id = "adults", label = "Adults", input_type = "numericInput"),
      form_field(id = "juveniles", label = "Juveniles", input_type = "numericInput",
                 editable = function(user) identical(user, "admin"))
    )
  )
  conn <- local_test_conn(counts$db)
  insert_record(counts, list(day = "fri", site = "1", species = "A", adults = 1, juveniles = 1), conn = conn)
  shown <- NULL
  local_mocked_bindings(showNotification = function(ui, ...) shown <<- ui, .package = "shiny")

  # can_add = FALSE: filling row B is refused, the whole save with it.
  shiny::testServer(grid_server, args = grid_args(counts, conn, permissions = list(can_add = FALSE)), {
    session$flushReact()
    session$setInputs(cells = matrix(c(2, 3, 1, NA), nrow = 2))
    expect_identical(nrow(fetch_records(counts, conn = conn)), 1L)
    expect_identical(fetch_records(counts, conn = conn)$adults, 1)
    expect_match(shown, "may not change these rows: B")
  })

  # can_delete = FALSE: emptying row A is refused.
  shiny::testServer(grid_server, args = grid_args(counts, conn, permissions = list(can_delete = FALSE)), {
    session$flushReact()
    session$setInputs(cells = matrix(c(0, NA, 0, NA), nrow = 2))
    expect_identical(nrow(fetch_records(counts, conn = conn)), 1L)
  })

  # juveniles is locked for ada: rendered disabled, a changed value is refused.
  shiny::testServer(grid_server, args = grid_args(counts, conn), {
    session$flushReact()
    html <- as.character(output$grid$html)
    expect_true(grepl('data-c="1"[^>]*disabled', html) || grepl('disabled[^>]*data-c="1"', html))
    expect_false(grepl('disabled[^>]*data-c="0"', html) || grepl('data-c="0"[^>]*disabled', html))
    session$setInputs(cells = matrix(c(1, NA, 5, NA), nrow = 2))
    expect_identical(fetch_records(counts, conn = conn)$juveniles, 1)
  })

  # For admin the same change goes through.
  shiny::testServer(grid_server, args = grid_args(counts, conn, user = "admin"), {
    session$flushReact()
    session$setInputs(cells = matrix(c(1, NA, 5, NA), nrow = 2))
    expect_identical(fetch_records(counts, conn = conn)$juveniles, 5)
  })

  # editable_fields restricts the same way.
  shiny::testServer(grid_server, args = grid_args(counts, conn, user = "admin",
                                                  permissions = list(editable_fields = "juveniles")), {
    session$flushReact()
    session$setInputs(cells = matrix(c(8, NA, 5, NA), nrow = 2))
    expect_identical(fetch_records(counts, conn = conn)$adults, 1)
  })
})

test_that("a value from a grid rendered before is saved to its own group, not the current one", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  site <- shiny::reactiveVal("1")

  shiny::testServer(grid_server, args = grid_args(counts, conn, poll = NULL, group = function() list(day = "fri", site = site())), {
    session$flushReact()
    # Typed at site 1; the user picks site 2 before it was sent.
    old <- structure(matrix(c(4, NA, NA, NA), nrow = 2), token = "1")
    site("2")
    session$flushReact()
    session$setInputs(cells = old)
    stored <- fetch_records(counts, conn = conn)
    expect_identical(paste(stored$site, stored$species, stored$adults), "1 A 4")

    current <- structure(matrix(c(7, NA, NA, NA), nrow = 2), token = "2")
    session$setInputs(cells = current)
    stored <- fetch_records(counts, conn = conn)
    expect_setequal(paste(stored$site, stored$species, stored$adults), c("1 A 4", "2 A 7"))

    # A second late value for site 1 is compared with what that grid last
    # saved, not with site 2.
    session$setInputs(cells = structure(matrix(c(5, NA, NA, NA), nrow = 2), token = "1"))
    stored <- fetch_records(counts, conn = conn)
    expect_setequal(paste(stored$site, stored$species, stored$adults), c("1 A 5", "2 A 7"))
  })
})

test_that("a change the size of a rounding error is still saved", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  insert_record(counts, list(day = "fri", site = "1", species = "A", adults = 1e9), conn = conn)

  shiny::testServer(grid_server, args = grid_args(counts, conn), {
    session$flushReact()
    session$setInputs(cells = matrix(c(1e9 + 1, NA, NA, NA), nrow = 2))
    expect_identical(fetch_records(counts, conn = conn)$adults, 1e9 + 1)
  })
})

test_that("a mandatory field outside the grid is reported up front", {
  counts <- form(
    form_id = "counts", table_name = "counts",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "species", label = "Species"),
      form_field(id = "observer", label = "Observer", mandatory = TRUE),
      form_field(id = "adults", label = "Adults", input_type = "numericInput")
    )
  )
  conn <- local_test_conn(counts$db)

  shiny::testServer(grid_server, args = list(id = "g", form = counts, rows = "A", key = "species", conn = conn), {
    session$flushReact()
    expect_error(output$grid, "mandatory field\\(s\\) observer are not part of the grid")
  })
})

test_that("two stored records for one row are reported", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  insert_record(counts, list(day = "fri", site = "1", species = "A", adults = 1), conn = conn)
  insert_record(counts, list(day = "fri", site = "1", species = "A", adults = 2), conn = conn)
  shown <- NULL
  local_mocked_bindings(showNotification = function(ui, ...) shown <<- ui, .package = "shiny")

  shiny::testServer(grid_server, args = grid_args(counts, conn), {
    session$flushReact()
    expect_match(shown, "Several records are stored for A")
    expect_true(grepl('value="1"[^>]*data-r="0" data-c="0"', as.character(output$grid$html)))
  })
})

test_that("grid_ui applies its width", {
  expect_match(as.character(grid_ui("g", width = "400px")), "width:400px;", fixed = TRUE)
})

test_that("zeros typed into a row without a record do not cause a false conflict", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  shown <- NULL
  local_mocked_bindings(showNotification = function(ui, ...) shown <<- ui, .package = "shiny")

  shiny::testServer(grid_server, args = grid_args(counts, conn), {
    session$flushReact()
    session$setInputs(cells = matrix(c(0, NA, NA, NA), nrow = 2))
    expect_identical(nrow(fetch_records(counts, conn = conn)), 0L)
    session$setInputs(cells = matrix(c(0, NA, 7, NA), nrow = 2))
    expect_null(shown)
    expect_identical(fetch_records(counts, conn = conn)$juveniles, 7)
  })
})

test_that("a save writes under the group the grid was rendered for", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  site <- shiny::reactiveVal("1")

  shiny::testServer(grid_server, args = grid_args(counts, conn, group = function() list(day = "fri", site = site())), {
    session$flushReact()
    # The group moves on, but the grid has not been rendered again yet.
    site("2")
    save_value <- structure(matrix(c(4, NA, NA, NA), nrow = 2), token = "1")
    session$setInputs(cells = save_value)
    stored <- fetch_records(counts, conn = conn)
    expect_true(nrow(stored) == 0L || identical(stored$site, "1"))
  })
})

test_that("two people changing different cells of one row do not collide", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  insert_record(counts, list(day = "fri", site = "1", species = "A", adults = 1, juveniles = 1), conn = conn)
  shown <- NULL
  local_mocked_bindings(showNotification = function(ui, ...) shown <<- ui, .package = "shiny")

  shiny::testServer(grid_server, args = grid_args(counts, conn), {
    session$flushReact()
    # bob changes juveniles of A; this user changes adults of A.
    update_record(counts, list(juveniles = 9), record_id = 1, conn = conn, user = "bob")
    session$setInputs(cells = matrix(c(4, NA, 1, NA), nrow = 2))

    stored <- fetch_records(counts, conn = conn)
    expect_identical(c(stored$adults, stored$juveniles), c(4, 9))
    expect_null(shown)
  })
})

test_that("emptying a row that someone else filled further is refused", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  insert_record(counts, list(day = "fri", site = "1", species = "A", adults = 1), conn = conn)
  shown <- NULL
  local_mocked_bindings(showNotification = function(ui, ...) shown <<- ui, .package = "shiny")

  shiny::testServer(grid_server, args = grid_args(counts, conn), {
    session$flushReact()
    update_record(counts, list(juveniles = 9), record_id = 1, conn = conn, user = "bob")
    # This user empties A as they saw it; bob's juveniles would go with it.
    session$setInputs(cells = matrix(c(0, NA, NA, NA), nrow = 2))

    expect_identical(nrow(fetch_records(counts, conn = conn)), 1L)
    expect_match(shown, "Someone else changed A")
  })
})

test_that("the grid shows what others saved, and only cells the browser took count as seen", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  insert_record(counts, list(day = "fri", site = "1", species = "A", adults = 1, juveniles = 1), conn = conn)
  sent <- NULL
  shown <- NULL
  local_mocked_bindings(sgt_grid_send_patch = function(session, input_id, message) sent <<- message)
  local_mocked_bindings(showNotification = function(ui, ...) shown <<- ui, .package = "shiny")

  shiny::testServer(grid_server, args = grid_args(counts, conn), {
    session$flushReact()
    update_record(counts, list(adults = 7, juveniles = 8), record_id = 1, conn = conn, user = "bob")
    session$elapse(3100)

    # Two cells differ: adults 1 -> 7 and juveniles 1 -> 8.
    expect_length(sent$patch, 2L)
    expect_identical(unlist(sent$patch[[1]]), c(0, 0, 1, 7))

    # The browser took adults but not juveniles (the user was in that cell).
    session$setInputs(cells_synced = list(token = sent$token, seq = sent$seq, applied = list(list(0, 0))))

    # Saving adults as shown (7) plus a new value for B: no conflict.
    session$setInputs(cells = matrix(c(7, 3, 1, NA), nrow = 2))
    expect_null(shown)
    expect_identical(fetch_records(counts, conn = conn)$adults, c(7, 3))

    # juveniles was never seen as 8: changing it is a conflict.
    session$setInputs(cells = matrix(c(7, 3, 2, NA), nrow = 2))
    expect_match(shown, "Someone else changed A")
    expect_identical(fetch_records(counts, conn = conn)$juveniles[1], 8)
  })
})

test_that("without conflict_check the last save of a cell wins, but a row is not emptied over others", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  insert_record(counts, list(day = "fri", site = "1", species = "A", adults = 1), conn = conn)
  insert_record(counts, list(day = "fri", site = "1", species = "B", adults = 1), conn = conn)
  shown <- NULL
  local_mocked_bindings(showNotification = function(ui, ...) shown <<- ui, .package = "shiny")

  shiny::testServer(grid_server, args = grid_args(counts, conn, conflict_check = FALSE, poll = NULL), {
    session$flushReact()
    update_record(counts, list(adults = 7), record_id = 1, conn = conn, user = "bob")
    update_record(counts, list(juveniles = 5), record_id = 2, conn = conn, user = "bob")
    # adults of A: the user's 4 wins. B emptied as the user saw it: refused,
    # bob's juveniles would go with it.
    session$setInputs(cells = matrix(c(4, 0, NA, NA), nrow = 2))

    stored <- fetch_records(counts, conn = conn)
    expect_identical(stored$adults, c(4, 1))
    expect_identical(stored$juveniles[2], 5)
    expect_match(shown, "Someone else changed B")
  })
})

test_that("poll = NULL does not look for changes, and poll is checked", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  sent <- NULL
  local_mocked_bindings(sgt_grid_send_patch = function(session, input_id, message) sent <<- message)

  shiny::testServer(grid_server, args = grid_args(counts, conn, poll = NULL), {
    session$flushReact()
    insert_record(counts, list(day = "fri", site = "1", species = "A", adults = 1), conn = conn)
    session$elapse(10000)
    expect_null(sent)
  })

  expect_error(grid_server("g", counts, rows = "A", key = "species", poll = 0), "poll must be")
})

test_that("a user who may only add does not change a record someone created meanwhile", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  shown <- NULL
  local_mocked_bindings(showNotification = function(ui, ...) shown <<- ui, .package = "shiny")

  shiny::testServer(grid_server, args = grid_args(counts, conn, poll = NULL, conflict_check = FALSE,
                                                  permissions = list(can_edit = FALSE)), {
    session$flushReact()
    insert_record(counts, list(day = "fri", site = "1", species = "A", juveniles = 4), conn = conn)
    # The grid still thinks A has no record; the user types adults.
    session$setInputs(cells = matrix(c(6, NA, NA, NA), nrow = 2))
    stored <- fetch_records(counts, conn = conn)
    expect_true(is.na(stored$adults))
    expect_match(shown, "Someone else changed A")
  })
})

test_that("float noise in a stored value does not count as a change", {
  f <- form(
    form_id = "share", table_name = "share", db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "species", label = "Species"),
      form_field(id = "count", label = "Count", input_type = "numericInput"),
      form_field(id = "share", label = "Share", input_type = "numericInput", editable = FALSE)
    )
  )
  conn <- local_test_conn(f$db)
  insert_record(f, list(species = "A", count = 1, share = 0.1 * 3), conn = conn)
  shown <- NULL
  local_mocked_bindings(showNotification = function(ui, ...) shown <<- ui, .package = "shiny")

  shiny::testServer(grid_server, args = list(id = "g", form = f, rows = "A", key = "species", conn = conn, poll = NULL), {
    session$flushReact()
    # The browser shows and sends the share as 0.3.
    session$setInputs(cells = matrix(c(2, 0.3), nrow = 1))
    expect_null(shown)
    stored <- fetch_records(f, conn = conn)
    expect_identical(stored$count, 2)
    expect_identical(stored$share, 0.1 * 3)
  })
})

test_that("each grid column takes min, max and step from its field", {
  f <- form(
    form_id = "steps", table_name = "steps", db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "species", label = "Species"),
      form_field(id = "count", label = "Count", input_type = "numericInput"),
      form_field(id = "price", label = "Price", input_type = "numericInput",
                 args = list(step = 0.001, min = -5, max = 5))
    )
  )
  conn <- local_test_conn(f$db)

  shiny::testServer(grid_server, args = list(id = "g", form = f, rows = "A", key = "species", conn = conn, poll = NULL), {
    session$flushReact()
    html <- as.character(output$grid$html)
    expect_match(html, 'min="0" step="1" data-r="0" data-c="0"', fixed = TRUE)
    expect_match(html, 'min="-5" max="5" step="0.001" data-r="0" data-c="1"', fixed = TRUE)
  })
})

test_that("a value read before a patch does not write the patched cells back", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  insert_record(counts, list(day = "fri", site = "1", species = "A", adults = 1, juveniles = 5), conn = conn)
  sent <- NULL
  local_mocked_bindings(sgt_grid_send_patch = function(session, input_id, message) sent <<- message)

  shiny::testServer(grid_server, args = grid_args(counts, conn), {
    session$flushReact()
    update_record(counts, list(juveniles = 7), record_id = 1, conn = conn, user = "carl")
    session$elapse(3100)
    expect_identical(sent$seq, 1L)
    session$setInputs(cells_synced = list(token = sent$token, seq = sent$seq, applied = list(list(0, 1))))

    # The debounced value was read at the keystroke, before the patch: it
    # still has juveniles 5, and seq 0.
    stale <- structure(matrix(c(12, NA, 5, NA), nrow = 2), token = sent$token, seq = 0L)
    session$setInputs(cells = stale)
    stored <- fetch_records(counts, conn = conn)
    expect_identical(c(stored$adults, stored$juveniles), c(12, 7))
  })
})

test_that("a second patch waits for the answer to the first", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  insert_record(counts, list(day = "fri", site = "1", species = "A", adults = 1), conn = conn)
  sent <- list()
  local_mocked_bindings(sgt_grid_send_patch = function(session, input_id, message) sent[[length(sent) + 1L]] <<- message)

  shiny::testServer(grid_server, args = grid_args(counts, conn), {
    session$flushReact()
    update_record(counts, list(adults = 7), record_id = 1, conn = conn, user = "carl")
    session$elapse(3100)
    update_record(counts, list(adults = 9), record_id = 1, conn = conn, user = "carl")
    session$elapse(3100)
    expect_length(sent, 1L)

    # The answer to patch 1 arrives: now the next change goes out as patch 2.
    session$setInputs(cells_synced = list(token = sent[[1]]$token, seq = 1L, applied = list(list(0, 0))))
    update_record(counts, list(adults = 11), record_id = 1, conn = conn, user = "carl")
    session$elapse(3100)
    expect_length(sent, 2L)
    expect_identical(sent[[2]]$seq, 2L)
    expect_identical(unlist(sent[[2]]$patch[[1]]), c(0, 0, 7, 11))
  })
})

test_that("the change stamp sees an update whose timestamp sorts lower", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  insert_record(counts, list(day = "fri", site = "1", species = "A", adults = 1), conn = conn)
  insert_record(counts, list(day = "fri", site = "1", species = "B", adults = 1), conn = conn)
  DBI::dbExecute(conn, "UPDATE counts SET sft_updated_at = '2026-10-25T02:59:00.000+0200' WHERE species = 'A'")
  DBI::dbExecute(conn, "UPDATE counts SET sft_updated_at = '2026-10-25T01:00:00.000+0200' WHERE species = 'B'")
  before <- records_stamp(counts, list(day = "fri", site = "1"), conn)
  # B is updated after the clock change: later in real time, lower as text
  # than A's, so MAX(sft_updated_at) would not move.
  DBI::dbExecute(conn, "UPDATE counts SET sft_updated_at = '2026-10-25T02:10:00.000+0100' WHERE species = 'B'")
  expect_false(identical(records_stamp(counts, list(day = "fri", site = "1"), conn), before))
})

test_that("saved reports every save, confirm() also one that writes nothing", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  local_mocked_bindings(sgt_grid_send_patch = function(session, input_id, message) NULL)

  shiny::testServer(grid_server, args = grid_args(counts, conn, poll = NULL), {
    session$flushReact()
    expect_null(session$returned$saved())

    session$setInputs(cells = matrix(c(4, NA, NA, NA), nrow = 2))
    s <- session$returned$saved()
    expect_identical(s$rows$key, "A")
    expect_identical(s$rows$action, "insert")
    expect_false(s$confirmed)
    expect_false(s$empty)
    expect_identical(s$group, list(day = "fri", site = "1"))

    # "Done" without a change: reported, nothing written.
    audit_before <- nrow(fetch_audit_log(counts, conn = conn))
    session$returned$confirm()
    s <- session$returned$saved()
    expect_true(s$confirmed)
    expect_identical(nrow(s$rows), 0L)
    expect_identical(nrow(fetch_audit_log(counts, conn = conn)), audit_before)
  })
})

test_that("confirm(empty = TRUE) clears the group, within the user's rights", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  insert_record(counts, list(day = "fri", site = "1", species = "A", adults = 3), conn = conn)
  local_mocked_bindings(sgt_grid_send_patch = function(session, input_id, message) NULL)
  shown <- NULL
  local_mocked_bindings(showNotification = function(ui, ...) shown <<- ui, .package = "shiny")

  # Without can_delete: refused, nothing reported.
  shiny::testServer(grid_server, args = grid_args(counts, conn, poll = NULL, permissions = list(can_delete = FALSE)), {
    session$flushReact()
    session$returned$confirm(empty = TRUE)
    expect_null(session$returned$saved())
    expect_identical(nrow(fetch_records(counts, conn = conn)), 1L)
  })

  # With it: the record goes, and the app learns the site is done and empty.
  shiny::testServer(grid_server, args = grid_args(counts, conn, poll = NULL), {
    session$flushReact()
    session$returned$confirm(empty = TRUE)
    s <- session$returned$saved()
    expect_true(s$confirmed)
    expect_true(s$empty)
    expect_identical(s$rows$action, "delete")
    expect_identical(nrow(fetch_records(counts, conn = conn)), 0L)
  })

  # On a group with nothing stored it still reports: visited, found nothing.
  shiny::testServer(grid_server, args = grid_args(counts, conn, poll = NULL, group = list(day = "fri", site = "9")), {
    session$flushReact()
    session$returned$confirm(empty = TRUE)
    s <- session$returned$saved()
    expect_true(s$confirmed && s$empty)
    expect_identical(s$group$site, "9")
  })
})

long_form <- function() {
  form(
    form_id = "obs", table_name = "obs", db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "day", label = "Day"),
      form_field(id = "site", label = "Site"),
      form_field(id = "species", label = "Species"),
      form_field(id = "zone", label = "Zone"),
      form_field(id = "count", label = "Count", input_type = "numericInput")
    )
  )
}

long_args <- function(f, conn, ...) {
  utils::modifyList(
    list(id = "g", form = f, rows = c("Species A" = "A", "Species B" = "B"), key = "species",
         cols = c("Zone 1" = "1", "Zone 2" = "2", "Zone 3" = "3"), col_key = "zone", value = "count",
         group = list(day = "fri", site = "7"), conn = conn, user = "ada", poll = NULL),
    list(...),
    keep.null = TRUE
  )
}

test_that("a long grid stores one record per cell and deletes emptied cells", {
  f <- long_form()
  conn <- local_test_conn(f$db)
  insert_record(f, list(day = "fri", site = "7", species = "B", zone = "3", count = 5), conn = conn)
  insert_record(f, list(day = "sun", site = "7", species = "A", zone = "1", count = 9), conn = conn)

  shiny::testServer(grid_server, args = long_args(f, conn), {
    session$flushReact()
    html <- as.character(output$grid$html)
    expect_true(grepl("Zone 2", html, fixed = TRUE))
    expect_true(grepl('value="5"[^>]*data-r="1" data-c="2"', html))

    # A1 = 4, A2 = 0 (zero: not stored), B3 emptied (deleted).
    session$setInputs(cells = matrix(c(4, NA, 0, NA, NA, NA), nrow = 2))
    live <- fetch_records(f, conn = conn)
    fri <- live[live$day == "fri", ]
    expect_identical(paste(fri$species, fri$zone, fri$count), "A 1 4")
    expect_identical(nrow(live[live$day == "sun", ]), 1L)
    expect_identical(sum(fetch_audit_log(f, conn = conn)$action == "delete"), 1L)

    s <- session$returned$saved()
    expect_setequal(s$rows$action, c("insert", "delete"))
  })
})

test_that("in a long grid two people on different cells of a row do not collide", {
  f <- long_form()
  conn <- local_test_conn(f$db)
  insert_record(f, list(day = "fri", site = "7", species = "A", zone = "1", count = 1), conn = conn)
  shown <- NULL
  local_mocked_bindings(showNotification = function(ui, ...) shown <<- ui, .package = "shiny")

  shiny::testServer(grid_server, args = long_args(f, conn), {
    session$flushReact()
    # Someone else adds A2 and changes A1 meanwhile.
    insert_record(f, list(day = "fri", site = "7", species = "A", zone = "2", count = 8), conn = conn)
    update_record(f, list(count = 2), record_id = 1, conn = conn, user = "bob")

    # This user types A3 only: no collision.
    session$setInputs(cells = matrix(c(1, NA, NA, NA, 6, NA), nrow = 2))
    expect_null(shown)
    live <- fetch_records(f, conn = conn)
    expect_setequal(paste(live$zone, live$count), c("1 2", "2 8", "3 6"))

    # Now A1, which bob changed after this grid loaded: refused, named by cell.
    session$setInputs(cells = matrix(c(5, NA, NA, NA, 6, NA), nrow = 2))
    expect_match(shown, "Species A / Zone 1")
    expect_identical(fetch_records(f, conn = conn)$count[1], 2)
  })
})

test_that("a long grid needs value and cols", {
  f <- long_form()
  expect_error(grid_server("g", f, rows = "A", key = "species", col_key = "zone"), "needs `value`")
})

test_that("rows and columns given as a named vector show their names", {
  spec <- sgt_grid_row_spec(c("Species A" = "A", "B"))
  expect_identical(spec$value, c("A", "B"))
  expect_identical(spec$label, c("Species A", "B"))
})

test_that("a long grid shows cells others saved", {
  f <- long_form()
  conn <- local_test_conn(f$db)
  sent <- NULL
  local_mocked_bindings(sgt_grid_send_patch = function(session, input_id, message) sent <<- message)

  shiny::testServer(grid_server, args = long_args(f, conn, poll = 3), {
    session$flushReact()
    insert_record(f, list(day = "fri", site = "7", species = "B", zone = "2", count = 3), conn = conn)
    session$elapse(3100)
    expect_identical(unlist(sent$patch[[1]]), c(1, 1, 3))
    session$setInputs(cells_synced = list(token = sent$token, seq = sent$seq, applied = list(list(1, 1))))
    # B2 is now seen as stored: changing it is an update of that record.
    session$setInputs(cells = matrix(c(NA, NA, NA, 4, NA, NA), nrow = 2))
    live <- fetch_records(f, conn = conn)
    expect_identical(nrow(live), 1L)
    expect_identical(live$count, 4)
  })
})

test_that("keystrokes a replaced grid still had arrive as cells_late and are saved to their group", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  site <- shiny::reactiveVal("1")

  shiny::testServer(grid_server, args = grid_args(counts, conn, poll = NULL, group = function() list(day = "fri", site = site())), {
    session$flushReact()
    site("2")
    session$flushReact()
    # The new grid's first value and the old grid's late one in one flush.
    session$setInputs(
      cells = structure(matrix(c(NA, NA, NA, NA), nrow = 2), token = "2"),
      cells_late = structure(matrix(c(3, 1, NA, NA), nrow = 2), token = "1")
    )
    stored <- fetch_records(counts, conn = conn)
    expect_setequal(paste(stored$site, stored$species, stored$adults), c("1 A 3", "1 B 1"))
  })
})

test_that("confirm() inside a with_transaction() that rolls back keeps the values to save", {
  db <- db_sqlite(tempfile(fileext = ".sqlite"))
  counts <- form(form_id = "counts", table_name = "counts", db = db, fields = list(
    form_field(id = "site", label = "Site"), form_field(id = "species", label = "Species"),
    form_field(id = "adults", label = "Adults", input_type = "numericInput")))
  status <- form(form_id = "status", table_name = "status", db = db, fields = list(
    form_field(id = "site", label = "Site")))
  seed <- db_connect(db); init_db(counts, conn = seed); init_db(status, conn = seed); db_disconnect(seed)

  for (case in c("retry", "fail")) {
    attempts <- 0L
    server <- function(input, output, session) {
      g <- grid_server("g", counts, rows = c("A", "B"), key = "species", group = list(site = case),
                       poll = NULL, autosave = FALSE)
      f <- form_server("f", status)
      session$userData$g <- g; session$userData$f <- f
      shiny::observeEvent(input$done, {
        conn <- f$connection()
        attempts <<- 0L
        try(with_transaction(conn, {
          attempts <<- attempts + 1L
          g$confirm()
          insert_record(status, list(site = case), conn = conn)
          if (case == "retry" && attempts == 1L) stop("database is locked")
          if (case == "fail" && input$done == 1) stop("status write failed")
        }), silent = TRUE)
      })
    }

    shiny::testServer(server, {
      session$flushReact()
      conn <- session$userData$f$connection()
      tok <- attr(session$userData$g$value(), "token")
      session$setInputs(`g-cells` = structure(matrix(c(4, 5), nrow = 2), token = tok))
      session$setInputs(done = 1)
      live <- function(f) { r <- fetch_records(f, conn = conn); r[r$site == case, , drop = FALSE] }
      if (case == "retry") {
        expect_identical(attempts, 2L)
        expect_identical(nrow(live(counts)), 2L)
        expect_identical(nrow(live(status)), 1L)
      } else {
        expect_identical(nrow(live(counts)), 0L)
        expect_null(session$userData$g$saved())
        session$setInputs(done = 2)
        expect_identical(nrow(live(counts)), 2L)
        expect_identical(nrow(live(status)), 1L)
      }
    })
  }
})

test_that("a poll answer for the previous group is not applied after a group switch", {
  sent <- list()
  notes <- character()
  local_mocked_bindings(
    sgt_grid_send_patch = function(session, input_id, message) sent[[length(sent) + 1L]] <<- message
  )
  local_mocked_bindings(
    showNotification = function(ui, ...) { notes <<- c(notes, as.character(ui)); invisible(NULL) },
    .package = "shiny"
  )
  counts <- form(form_id = "counts", table_name = "counts", db = db_sqlite(tempfile(fileext = ".sqlite")), fields = list(
    form_field(id = "site", label = "Site"), form_field(id = "species", label = "Species"),
    form_field(id = "adults", label = "Adults", input_type = "numericInput")))
  conn <- local_test_conn(counts$db); init_db(counts, conn = conn)
  site <- shiny::reactiveVal("1")

  shiny::testServer(grid_server, args = list(id = "g", form = counts, rows = c("A", "B"), key = "species",
    group = function() list(site = site()), conn = conn, poll = 1), {
    session$flushReact()
    insert_record(counts, list(site = "1", species = "A", adults = 31), conn = conn, user = "other")
    session$elapse(1500); session$flushReact()
    patch <- sent[[length(sent)]]
    site("2"); session$flushReact()
    # The replaced grid's answer arrives late.
    session$setInputs(cells_synced = list(token = patch$token, seq = patch$seq, applied = list(list(0, 0))))
    token_now <- sub('.*data-token="([0-9]+)".*', "\\1", as.character(output$grid$html))
    session$setInputs(cells = structure(matrix(c(NA, 4), nrow = 2), token = token_now))
    st <- fetch_records(counts, conn = conn)
    expect_length(notes, 0L)
    expect_identical(nrow(st[st$site == "2" & st$species == "B", ]), 1L)
  })
})

test_that("a 0 typed where no record is stays after the next poll", {
  counts <- counts_form()
  conn <- local_test_conn(counts$db)
  init_db(counts, conn = conn)
  sent <- NULL
  local_mocked_bindings(sgt_grid_send_patch = function(session, input_id, message) sent <<- message)

  shiny::testServer(grid_server, args = grid_args(counts, conn), {
    session$flushReact()
    session$setInputs(cells = matrix(c(0, 7, NA, NA), nrow = 2))
    expect_identical(fetch_records(counts, conn = conn)$species, "B")
    # Someone else saves in the group: the poll sends what changed.
    insert_record(counts, list(day = "fri", site = "1", species = "A", juveniles = 2), conn = conn)
    session$elapse(3100)
    patched <- vapply(sent$patch, function(p) paste(p[1:2], collapse = ","), character(1))
    # Juveniles of A (row 0, column 1) arrives; the user's 0 in adults of A is
    # not blanked by a patch to "no record".
    expect_true("0,1" %in% patched)
    adults_a <- Filter(function(p) p[[1]] == 0 && p[[2]] == 0, sent$patch)
    expect_length(adults_a, 0L)
  })
})

test_that("a row the rules refuse stays in the grid, the valid rows are stored", {
  counts <- counts_form()
  counts$validation_rules <- list(forbid_if(
    "neg", function(values) isTRUE(values$adults < 0), fields = "adults",
    message = "No negative counts."
  ))
  conn <- local_test_conn(counts$db)
  init_db(counts, conn = conn)
  shown <- character()
  local_mocked_bindings(showNotification = function(ui, ...) shown <<- c(shown, ui), .package = "shiny")

  shiny::testServer(grid_server, args = grid_args(counts, conn, poll = NULL), {
    session$flushReact()
    session$setInputs(cells = matrix(c(-1, 5, NA, 6), nrow = 2))
    stored <- fetch_records(counts, conn = conn)
    expect_identical(stored$species, "B")
    expect_identical(stored$adults, 5)
    expect_match(shown[length(shown)], "^A: .*No negative counts")
    # The user corrects A; B is unchanged and not written again.
    session$setInputs(cells = matrix(c(2, 5, NA, 6), nrow = 2))
    stored <- fetch_records(counts, conn = conn)
    expect_identical(stored$species, c("B", "A"))
    expect_identical(nrow(fetch_audit_log(counts, conn = conn)), 2L)
  })
})
