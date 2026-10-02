# The basket board: moves from the stock, between records and back.

board_setup <- function(env = parent.frame()) {
  f <- cart_forms()
  conn <- local_test_conn(f$db, .local_envir = env)
  seed_items(f, conn)
  insert_record(f$teams, list(team = "A", equipment = basket(scope = 2)), conn = conn)
  insert_record(f$teams, list(team = "B"), conn = conn)
  c(f, list(conn = conn))
}

held <- function(f, team) {
  rows <- fetch_records(f$teams, conn = f$conn)
  cart <- sgt_cart_decode(rows$equipment[rows$team == team])
  if (is.null(cart)) return(0L)
  sum(cart$n[cart$item == "scope" & !cart$on_site])
}

test_that("one move takes one piece from the stock, between records and back", {
  f <- board_setup()
  field <- find_field(f$teams, "equipment")

  sgt_board_move(f$conn, f$teams, field, from = NULL, to = 2L, item = "scope", user = "u")
  expect_identical(c(held(f, "A"), held(f, "B")), c(2L, 1L))
  sgt_board_move(f$conn, f$teams, field, from = 1L, to = 2L, item = "scope", user = "u")
  expect_identical(c(held(f, "A"), held(f, "B")), c(1L, 2L))
  sgt_board_move(f$conn, f$teams, field, from = 2L, to = NULL, item = "scope", user = "u")
  expect_identical(c(held(f, "A"), held(f, "B")), c(1L, 1L))

  # The stock is 3: A 1 + B 1 leaves one; a second one is refused.
  sgt_board_move(f$conn, f$teams, field, from = NULL, to = 1L, item = "scope", user = "u")
  expect_error(sgt_board_move(f$conn, f$teams, field, from = NULL, to = 1L, item = "scope", user = "u"),
               class = "sft_validation_error")
  # A move between records never needs free stock.
  expect_no_error(sgt_board_move(f$conn, f$teams, field, from = 1L, to = 2L, item = "scope", user = "u"))
  expect_identical(c(held(f, "A"), held(f, "B")), c(1L, 2L))
})

test_that("taking the last piece leaves the record empty", {
  f <- board_setup()
  field <- find_field(f$teams, "equipment")
  sgt_board_move(f$conn, f$teams, field, from = NULL, to = 2L, item = "scope", user = "u")
  sgt_board_move(f$conn, f$teams, field, from = 2L, to = NULL, item = "scope", user = "u")
  rows <- fetch_records(f$teams, conn = f$conn)
  expect_true(is.na(rows$equipment[rows$team == "B"]))
})

test_that("a piece that is not there, or kept on site, does not move", {
  f <- board_setup()
  field <- find_field(f$teams, "equipment")
  update_record(f$teams, list(equipment = data.frame(item = "scope", n = 2, on_site = TRUE)),
                record_id = 1L, conn = f$conn)
  expect_error(sgt_board_move(f$conn, f$teams, field, from = 1L, to = 2L, item = "scope", user = "u"),
               "no longer there")
  expect_identical(held(f, "B"), 0L)
})

test_that("the payload lists records with baskets and what is free", {
  f <- board_setup()
  fields <- sgt_board_fields(f$teams)
  expect_identical(fields$label$id, "team")
  payload <- sgt_board_payload(f$conn, f$teams, fields, fetch_records(f$teams, conn = f$conn), TRUE)
  expect_identical(vapply(payload$records, function(s) s$label, ""), c("A", "B"))
  expect_identical(payload$records[[1]]$cart[[1]]$n, 2L)
  expect_identical(payload$items[[1]]$available, 1)
})

test_that("the module moves on a drop and refuses without the right to edit", {
  f <- board_setup()
  local_mocked_bindings(showNotification = function(...) NULL, .package = "shiny")

  shiny::testServer(form_server, args = list(
    id = "m", form = f$teams, conn = f$conn, columns = list(persist = FALSE)
  ), {
    session$setInputs(board_ready = TRUE)
    session$setInputs(board_move = list(from = NULL, to = 2L, item = "scope", seq = 1))
    expect_identical(session$returned$saved(), 1L)
  })
  expect_identical(held(f, "B"), 1L)

  for (permissions in list(list(can_edit = FALSE), list(editable_fields = "team"))) {
    shiny::testServer(form_server, args = list(
      id = "m", form = f$teams, conn = f$conn, columns = list(persist = FALSE),
      permissions = permissions
    ), {
      session$setInputs(board_ready = TRUE)
      session$setInputs(board_move = list(from = 1L, to = 2L, item = "scope", seq = 1))
    })
    expect_identical(c(held(f, "A"), held(f, "B")), c(2L, 1L))
  }
})

test_that("form_ui draws the board only on request, in the form's language", {
  expect_false(grepl("sgt-board", as.character(form_ui("m"))))
  expect_true(grepl('id="m-board"', as.character(form_ui("m", show_board = TRUE))))
  html <- as.character(form_ui("m", show_board = TRUE, language = german()))
  expect_match(html, "sgt-board", fixed = TRUE)
  expect_match(html, "Lager", fixed = TRUE)
})

test_that("a move needs a board on the page and the right to see the records", {
  f <- board_setup()
  local_mocked_bindings(showNotification = function(...) NULL, .package = "shiny")
  cases <- list(
    list(ready = FALSE, permissions = list()),
    list(ready = TRUE, permissions = list(can_view_table = FALSE)),
    list(ready = TRUE, permissions = list(can_view_record = FALSE))
  )
  for (case in cases) {
    shiny::testServer(form_server, args = list(
      id = "m", form = f$teams, conn = f$conn, columns = list(persist = FALSE),
      permissions = case$permissions
    ), {
      if (case$ready) session$setInputs(board_ready = TRUE)
      session$setInputs(board_move = list(from = 1L, to = 2L, item = "scope", seq = 1))
    })
    expect_identical(c(held(f, "A"), held(f, "B")), c(2L, 0L))
  }
})

test_that("a move recomputes derived fields and refuses a hidden basket", {
  f <- board_setup()
  teams <- f$teams
  teams$fields <- c(teams$fields, list(
    form_field(id = "pieces", label = "Pieces", input_type = "numericInput", editable = FALSE)
  ))
  init_db(teams, conn = f$conn)
  field <- find_field(teams, "equipment")
  count <- dynamic_value("pieces", depends_on = "equipment", value = function(values) {
    cart <- values$equipment
    if (is.character(cart)) cart <- sgt_cart_decode(cart)
    if (is.null(cart)) 0 else sum(cart$n)
  })

  # The rules a module hands over: apply_edit_rules() on a minimal state.
  rules_with <- function(bindings) {
    state <- list(form = teams, input_bindings = bindings, conn = function() f$conn,
                  current_user = function() "u", display_context = function() list(form = teams))
    function(record_id, values) apply_edit_rules(state, record_id, values)
  }
  sgt_board_move(f$conn, teams, field, from = 1L, to = 2L, item = "scope", user = "u",
                 rules = rules_with(list(count)))
  rows <- fetch_records(teams, conn = f$conn)
  expect_identical(rows$pieces, c(1, 1))

  hide_b <- dynamic_visibility("equipment", depends_on = "team",
                               visible = function(values) !identical(values$team, "B"))
  expect_error(
    sgt_board_move(f$conn, teams, field, from = 1L, to = 2L, item = "scope", user = "u",
                   rules = rules_with(list(hide_b))),
    "not shown for this record"
  )
  expect_identical(c(held(f, "A"), held(f, "B")), c(1L, 1L))
})
