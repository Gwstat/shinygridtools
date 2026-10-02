# Basket field: storage, display, stock check on save and restore, palette.

test_that("a basket is stored as JSON and shown as counts", {
  value <- data.frame(item = c("scope", "compass", "scope"), label = c("Scope", "Compass", "Scope"),
                      icon = c("U", NA, "U"), n = c(2, 1, 1), on_site = c(FALSE, TRUE, FALSE))
  stored <- sgt_cart_encode(value)
  decoded <- sgt_cart_decode(stored)
  expect_identical(decoded$item, c("scope", "compass"))
  expect_identical(decoded$n, c(3L, 1L))
  expect_identical(sgt_cart_encode(decoded), stored)
  expect_identical(sgt_cart_format(stored), "3 × U Scope, 1 × Compass (on site)")
  expect_identical(with_language(german(), sgt_cart_format(stored)),
                   "3 × U Scope, 1 × Compass (vor Ort)")
  expect_true(is.na(sgt_cart_encode(basket(scope = 0))))
  expect_null(sgt_cart_decode("not json"))
  expect_identical(sgt_cart_format("not json"), "not json")
})

test_that("the input handler turns the browser's lines into the data frame", {
  lines <- list(list(item = "scope", label = "Scope", icon = NULL, n = 2, on_site = FALSE))
  out <- sgt_cart_input_handler(lines)
  expect_identical(out$item, "scope")
  expect_identical(out$n, 2L)
  expect_null(sgt_cart_input_handler(list()))
})

test_that("a save that takes more than the stock is refused, on-site lines do not count", {
  f <- cart_forms()
  conn <- local_test_conn(f$db)
  seed_items(f, conn)

  insert_record(f$teams, list(team = "A", equipment = basket(scope = 2, compass = 5)), conn = conn)
  err <- tryCatch(insert_record(f$teams, list(team = "B", equipment = basket(scope = 2)), conn = conn),
                  error = function(e) e)
  expect_s3_class(err, "sft_validation_error")
  expect_match(conditionMessage(err), "only 1 x Scope left")
  expect_identical(err$issues$fields[[1]], "equipment")

  on_site <- data.frame(item = "scope", n = 4, on_site = TRUE)
  insert_record(f$teams, list(team = "C", equipment = on_site), conn = conn)
  insert_record(f$teams, list(team = "D", equipment = basket(scope = 1)), conn = conn)
  expect_identical(nrow(fetch_records(f$teams, conn = conn)), 3L)
})

test_that("an over-allocated record can still be saved unchanged, deleting frees stock, restore checks", {
  f <- cart_forms()
  conn <- local_test_conn(f$db)
  seed_items(f, conn)
  a <- insert_record(f$teams, list(team = "A", equipment = basket(scope = 3)), conn = conn)
  # The stock is lowered below what A holds.
  update_record(f$items, list(stock = 2), record_id = 1L, conn = conn)
  expect_no_error(update_record(f$teams, list(team = "A2"), record_id = a$sft_id, conn = conn))
  expect_error(update_record(f$teams, list(equipment = basket(scope = 4)), record_id = a$sft_id, conn = conn),
               class = "sft_validation_error")

  soft_delete_record(f$teams, record_id = a$sft_id, conn = conn)
  b <- insert_record(f$teams, list(team = "B", equipment = basket(scope = 2)), conn = conn)
  expect_error(restore_record(f$teams, record_id = a$sft_id, conn = conn), "Cannot restore")
  soft_delete_record(f$teams, record_id = b$sft_id, conn = conn)
  update_record(f$items, list(stock = 3), record_id = 1L, conn = conn)
  expect_no_error(restore_record(f$teams, record_id = a$sft_id, conn = conn))
})

test_that("an item no longer in the catalog cannot be added", {
  f <- cart_forms()
  conn <- local_test_conn(f$db)
  seed_items(f, conn)
  soft_delete_record(f$items, record_id = 1L, conn = conn)
  expect_error(insert_record(f$teams, list(team = "A", equipment = basket(scope = 1)), conn = conn),
               "no longer offered")
})

test_that("the palette lists what is still free for this record", {
  f <- cart_forms()
  conn <- local_test_conn(f$db)
  seed_items(f, conn)
  a <- insert_record(f$teams, list(team = "A", equipment = basket(scope = 2)), conn = conn)
  field <- find_field(f$teams, "equipment")

  fresh <- sgt_cart_available_items(conn, f$teams, field)
  expect_identical(fresh$available, c(1, NA))
  own <- sgt_cart_available_items(conn, f$teams, field, record_id = a$sft_id)
  expect_identical(own$available, c(3, NA))
})

test_that("form_server sends the palette when a dialog opens", {
  f <- cart_forms()
  conn <- local_test_conn(f$db)
  seed_items(f, conn)
  insert_record(f$teams, list(team = "A", equipment = basket(scope = 2)), conn = conn)
  sent <- list()
  local_mocked_bindings(update_cart_input = function(session, inputId, value = NULL, items = NULL) {
    sent[[inputId]] <<- items
  })
  local_mocked_bindings(showModal = function(...) NULL, .package = "shiny")

  shiny::testServer(form_server, args = list(
    id = "m", form = f$teams, conn = conn, columns = list(persist = FALSE)
  ), {
    session$flushReact()
    session$setInputs(open_add = 1)
    expect_identical(sent$add_equipment$available, c(1, NA))
    session$setInputs(records_rows_selected = 1L)
    session$setInputs(open_edit = 1)
    expect_identical(sent$edit_equipment$available, c(3, NA))
  })
})

test_that("a fixed palette from args$items renders and limits too", {
  db <- db_sqlite(tempfile(fileext = ".sqlite"))
  f <- form(form_id = "fixed", table_name = "fixed", db = db, fields = list(
    form_field(id = "gear", label = "Gear", input_type = "cart_input",
               args = list(items = data.frame(id = "scope", label = "Scope", stock = 1)))
  ))
  conn <- local_test_conn(db)
  init_db(f, conn = conn)
  html <- as.character(render_form_fields(f, prefix = "add_"))
  expect_match(html, "sgt-cart-input")
  expect_match(html, "&quot;available&quot;:1|\"available\":1")
  insert_record(f, list(gear = basket(scope = 1)), conn = conn)
  expect_error(insert_record(f, list(gear = basket(scope = 1)), conn = conn), class = "sft_validation_error")
})

test_that("a count beyond any basket is refused with a message", {
  expect_error(sgt_cart_normalize(data.frame(item = "scope", n = 3e9)), "whole number up to 1000000")
  expect_error(sgt_cart_normalize(data.frame(item = "scope", n = Inf)), "whole number up to 1000000")
  expect_identical(sgt_cart_normalize(data.frame(item = "scope", n = 5))$n, 5L)
})

test_that("a basket field that asks for its palette gets it, also in the inline layout", {
  f <- cart_forms()
  conn <- local_test_conn(f$db)
  seed_items(f, conn)
  sent <- list()
  local_mocked_bindings(update_cart_input = function(session, inputId, value = NULL, items = NULL) {
    sent[[inputId]] <<- items
  })

  shiny::testServer(form_server, args = list(
    id = "m", form = f$teams, conn = conn, columns = list(persist = FALSE)
  ), {
    session$flushReact()
    session$setInputs(add_equipment_palette = 1)
    expect_identical(sent$add_equipment$available, c(3, NA))
  })
})

test_that("one catalog's stock is shared by every basket field of every form", {
  f <- cart_forms()
  conn <- local_test_conn(f$db)
  seed_items(f, conn)
  catalog <- cart_catalog(f$items, id = "code", label = "name", icon = "emoji", stock = "stock")
  vehicles <- form(form_id = "vehicles", table_name = "vehicles", db = f$db, fields = list(
    form_field(id = "plate", label = "Plate"),
    form_field(id = "load", label = "Load", input_type = "cart_input", args = list(catalog = catalog))
  ))
  shifts <- form(form_id = "shifts", table_name = "shifts", db = f$db, fields = list(
    form_field(id = "am", label = "Morning", input_type = "cart_input", args = list(catalog = catalog)),
    form_field(id = "pm", label = "Afternoon", input_type = "cart_input", args = list(catalog = catalog))
  ))
  init_db(vehicles, conn = conn)
  init_db(shifts, conn = conn)

  insert_record(f$teams, list(team = "A", equipment = basket(scope = 2)), conn = conn)
  expect_error(insert_record(vehicles, list(plate = "X", load = basket(scope = 2)), conn = conn),
               "only 1 x Scope left")
  insert_record(vehicles, list(plate = "X", load = basket(scope = 1)), conn = conn)
  expect_error(insert_record(shifts, list(am = basket(scope = 1)), conn = conn), "only 0 x Scope left")

  soft_delete_record(f$teams, record_id = 1L, conn = conn)
  # Two fields of one record take from the same stock together.
  expect_error(insert_record(shifts, list(am = basket(scope = 1), pm = basket(scope = 2)), conn = conn),
               class = "sft_validation_error")
  expect_no_error(insert_record(shifts, list(am = basket(scope = 1), pm = basket(scope = 1)), conn = conn))
  field <- find_field(f$teams, "equipment")
  expect_identical(sgt_cart_available_items(conn, f$teams, field)$available[1], 0)
})

test_that("restoring an older version of a live record checks the stock too", {
  f <- cart_forms()
  conn <- local_test_conn(f$db)
  seed_items(f, conn)
  insert_record(f$teams, list(team = "A", equipment = basket(scope = 3)), conn = conn)
  update_record(f$teams, list(equipment = NA), record_id = 1L, conn = conn)
  insert_record(f$teams, list(team = "B", equipment = basket(scope = 3)), conn = conn)
  expect_error(restore_record(f$teams, record_id = 1L, version_no = 1L, reactivate = FALSE, conn = conn),
               "Cannot restore")
})

test_that("odd values and catalogs give messages, not raw errors", {
  lines <- rep(list(list(item = "scope", n = 1e6)), 3)
  expect_error(sgt_cart_normalize(lines), "whole number up to 1000000")
  expect_null(sgt_cart_input_handler(list("a", "b")))
  expect_null(sgt_cart_input_handler(rep(list(list(item = "a", n = 1)), 1001)))

  f <- cart_forms()
  conn <- local_test_conn(f$db)
  init_db(f$teams, conn = conn)
  expect_error(insert_record(f$teams, list(team = "A", equipment = basket(scope = 1)), conn = conn),
               "Call init_db\\(\\) on that form")

  seed_items(f, conn)
  # A second "scope", as an import without the unique check could leave it.
  DBI::dbExecute(conn, paste(
    "INSERT INTO items (code, name, stock, sft_is_deleted, sft_unique_slot)",
    "VALUES ('scope', 'Scope 2', 50, 0, 99)"
  ))
  field <- find_field(f$teams, "equipment")
  items <- sgt_cart_available_items(conn, f$teams, field)
  expect_identical(sum(items$id == "scope"), 1L)
  expect_identical(items$stock[items$id == "scope"], 3)
})

test_that("counting 5000 baskets takes well under a second", {
  f <- cart_forms()
  conn <- local_test_conn(f$db)
  seed_items(f, conn)
  stored <- sgt_cart_encode(basket(scope = 1, compass = 2))
  DBI::dbWriteTable(conn, "bulk", data.frame(sft_id = 1:5000, sft_is_deleted = 0L, cart = stored))
  took <- system.time(counts <- sgt_cart_counts(DBI::dbGetQuery(conn, "SELECT cart FROM bulk")$cart))[["elapsed"]]
  expect_identical(sum(counts$n[counts$item == "scope"]), 5000)
  expect_lt(took, 1)
})

test_that("a basket field moved to another catalog counts against the new one", {
  f <- cart_forms()
  conn <- local_test_conn(f$db)
  seed_items(f, conn)
  other <- form(form_id = "other_items", table_name = "other_items", db = f$db, fields = list(
    form_field(id = "code", label = "Code"), form_field(id = "name", label = "Name"),
    form_field(id = "stock", label = "Stock", input_type = "numericInput")
  ))
  init_db(other, conn = conn)
  make_boxes <- function(catalog_form) {
    form(form_id = "boxes", table_name = "boxes", db = f$db, fields = list(
      form_field(id = "load", label = "Box", input_type = "cart_input",
                 args = list(catalog = cart_catalog(catalog_form, id = "code", label = "name", stock = "stock")))
    ))
  }
  init_db(make_boxes(other), conn = conn)
  # The code now points the field at the teams' catalog; no init_db().
  boxes <- make_boxes(f$items)
  insert_record(boxes, list(load = basket(scope = 2)), conn = conn)
  expect_error(insert_record(f$teams, list(team = "A", equipment = basket(scope = 2)), conn = conn),
               "only 1 x Scope left")
})
