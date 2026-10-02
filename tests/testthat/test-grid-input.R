birds <- matrix(c(412, 388, 0, NA), nrow = 2, dimnames = list(c("A", "B"), c("First", "Second")))

test_that("grid_input renders one number cell per row and column", {
  html <- as.character(grid_input("g", "Birds", value = birds, hint = birds * 0 + 1))

  expect_true(grepl('id="g"', html, fixed = TRUE))
  expect_identical(lengths(regmatches(html, gregexpr('type="number"', html))), 4L)
  expect_true(grepl('data-r="1" data-c="0"', html, fixed = TRUE))
  expect_true(grepl('value="412"', html, fixed = TRUE))
  # An empty cell is an empty input; a hint is a span next to it, none for NA.
  expect_true(grepl('value=""', html, fixed = TRUE))
  expect_identical(lengths(regmatches(html, gregexpr("sgt-grid-hint", html))), 3L)
  expect_true(grepl('data-rows="[&quot;A&quot;,&quot;B&quot;]"', html, fixed = TRUE))
  expect_true(grepl("data-sum-col", html, fixed = TRUE))

  deps <- htmltools::findDependencies(grid_input("g", rows = "A", cols = "x"))
  expect_identical(deps[[1]]$name, "sgt-grid")
  expect_true(file.exists(file.path(deps[[1]]$src$file, "sgt-grid.js")))

  expect_false(grepl("data-sum-col", as.character(grid_input("g", rows = "A", cols = "x", sums = FALSE)), fixed = TRUE))
  expect_error(grid_input("g", value = matrix(1)), "rows and cols")
  expect_error(grid_input("g", value = matrix(1), rows = c("A", "B"), cols = "x"), "2 x 1 matrix")
})

test_that("the input handler turns the client's rows into a named matrix", {
  received <- list(rows = list("A", "B"), cols = list("x", "y"), values = list(list(1, NULL), list(2.5, 0)))
  m <- sgt_grid_input_handler(received)

  expect_identical(dim(m), c(2L, 2L))
  expect_identical(dimnames(m), list(c("A", "B"), c("x", "y")))
  expect_identical(m["A", "y"], NA_real_)
  expect_identical(m["B", "x"], 2.5)

  # Shiny may hand simplified vectors instead of lists; same result.
  simplified <- list(rows = c("A", "B"), cols = c("x", "y"), values = list(c(1, NA), c(2.5, 0)))
  expect_identical(sgt_grid_input_handler(simplified), m)
  expect_null(sgt_grid_input_handler(NULL))
  expect_identical(dim(sgt_grid_input_handler(list(rows = list(), cols = list("x"), values = list()))), c(0L, 1L))
})

test_that("a grid round-trips through the stored JSON and shows its totals", {
  stored <- sgt_grid_encode(birds)
  expect_identical(stored, "{\"rows\":[\"A\",\"B\"],\"cols\":[\"First\",\"Second\"],\"values\":[[412,0],[388,null]]}")
  expect_identical(sgt_grid_decode(stored), birds)
  expect_identical(sgt_grid_encode(NULL), NA_character_)
  expect_null(sgt_grid_decode(NA_character_))
  expect_identical(sgt_grid_format(stored), "First 800; Second 0")

  # As a form field: stored in one column, back as a matrix, totals in the table.
  results <- form(
    form_id = "results", table_name = "results",
    db = db_sqlite(tempfile(fileext = ".sqlite")),
    fields = list(
      form_field(id = "site", label = "Site"),
      form_field(id = "birds", label = "Birds", input_type = "grid_input",
                 args = list(rows = c("A", "B"), cols = c("First", "Second")))
    )
  )
  conn <- local_test_conn(results$db)
  insert_record(results, list(site = "1", birds = birds), conn = conn)
  row <- fetch_records(results, conn = conn)
  expect_identical(row$birds, stored)
  expect_identical(sgt_grid_decode(row$birds), birds)
  # The edit dialog of form_server() shows the stored grid with its numbers.
  modal <- NULL
  local_mocked_bindings(showModal = function(ui, ...) modal <<- ui, .package = "shiny")
  shiny::testServer(form_server, args = list(id = "m", form = results, conn = conn,
                                             columns = list(persist = FALSE)), {
    session$flushReact()
    session$setInputs(records_rows_selected = 1L)
    session$setInputs(open_edit = 1)
    html <- as.character(modal)
    expect_match(html, 'value="412"', fixed = TRUE)
    expect_match(html, 'value="388"', fixed = TRUE)
  })
})

test_that("update_grid_input sends values and hints as rows with NA as null", {
  messages <- list()
  session <- list(sendInputMessage = function(inputId, message) messages[[inputId]] <<- message)

  update_grid_input(session, "g", value = birds, hint = birds + 1)
  expect_identical(messages$g$values, list(list(412, 0), list(388, NULL)))
  expect_identical(messages$g$hint[[1]], list(413, 1))

  update_grid_input(session, "h")
  expect_null(messages$h)
})

test_that("a stored grid is placed by name when rows are reordered or added", {
  stored <- matrix(c(1, 2, 3, 4), 2, dimnames = list(c("A", "B"), c("x", "y")))

  reordered <- sgt_grid_matrix(stored, rows = c("B", "A"), cols = c("x", "y"))
  expect_identical(unname(reordered["B", ]), c(2, 4))

  added <- sgt_grid_matrix(stored, rows = c("A", "C", "B"), cols = c("y", "x"))
  expect_identical(unname(added["A", ]), c(3, 1))
  expect_true(all(is.na(added["C", ])))

  # Rows the field no longer lists keep their numbers, appended under their
  # own names, instead of moving under another label or failing.
  renamed <- sgt_grid_matrix(stored, rows = c("A", "C"), cols = c("x", "y"))
  expect_identical(rownames(renamed), c("A", "C", "B"))
  expect_identical(unname(renamed["B", ]), c(2, 4))
  expect_true(all(is.na(renamed["C", ])))

  # A stored row that is empty is not brought back.
  sparse <- stored
  sparse["B", ] <- NA
  expect_identical(rownames(sgt_grid_matrix(sparse, rows = c("A", "C"), cols = c("x", "y"))), c("A", "C"))

  # Hints drop what the grid does not show.
  expect_identical(dim(sgt_grid_matrix(stored, rows = "A", cols = c("x", "y"), keep_extra = FALSE)), c(1L, 2L))

  # Without names: by position, and it has to fit.
  expect_error(sgt_grid_matrix(unname(stored), rows = c("A2", "B2", "C2"), cols = c("x", "y")), "3 x 2")
})

test_that("grid_input shows the extra rows of a stored value", {
  stored <- matrix(c(1, 2), 2, 1, dimnames = list(c("A", "B"), "x"))
  html <- as.character(grid_input("g", value = stored, rows = c("A", "C"), cols = "x"))
  expect_match(html, ">B<", fixed = TRUE)
  expect_match(html, "[&quot;A&quot;,&quot;C&quot;,&quot;B&quot;]", fixed = TRUE)
})

test_that("a matrix without dimnames survives encode and decode", {
  m <- matrix(c(1, 2, 3, 4), 2)
  back <- sgt_grid_decode(sgt_grid_encode(m))
  expect_identical(unname(back), m)
  expect_identical(sgt_grid_format(sgt_grid_encode(m)), "#1 3; #2 7")
})

test_that("text that is not a stored grid does not break decoding", {
  expect_null(sgt_grid_decode("not json"))
  expect_null(sgt_grid_decode("[1, 2]"))
  expect_identical(sgt_grid_format("not json"), "not json")
})

test_that("an empty value clears every cell of a grid", {
  sent <- NULL
  session <- list(sendInputMessage = function(id, message) sent <<- list(id = id, message = message))
  update_grid_input(session, "edit_birds", value = NA)
  expect_identical(sent$id, "edit_birds")
  expect_true(isTRUE(sent$message$clear))
})

test_that("the package registers its input types with shinyformtools", {
  f <- form(form_id = "t", table_name = "t", db = db_sqlite(tempfile(fileext = ".sqlite")), fields = list(
    form_field(id = "g", label = "G", input_type = "grid_input", args = list(rows = "A", cols = "x")),
    form_field(id = "c", label = "C", input_type = "cart_input",
               args = list(items = data.frame(id = "scope", label = "Scope")))
  ))
  html <- as.character(render_form_fields(f, prefix = "add_"))
  expect_match(html, "sgt-grid-input", fixed = TRUE)
  expect_match(html, "sgt-cart-input", fixed = TRUE)
})
