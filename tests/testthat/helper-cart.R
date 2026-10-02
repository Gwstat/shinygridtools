# Forms and baskets shared by the basket tests.

cart_forms <- function(db = db_sqlite(tempfile(fileext = ".sqlite"))) {
  items <- form(form_id = "items", table_name = "items", db = db, fields = list(
    form_field(id = "code", label = "Code", unique = TRUE),
    form_field(id = "name", label = "Name"),
    form_field(id = "emoji", label = "Emoji"),
    form_field(id = "stock", label = "Stock", input_type = "numericInput")
  ))
  teams <- form(form_id = "teams", table_name = "teams", db = db, fields = list(
    form_field(id = "team", label = "Team"),
    form_field(id = "equipment", label = "Equipment", input_type = "cart_input",
               args = list(catalog = cart_catalog(items, id = "code", label = "name",
                                                  icon = "emoji", stock = "stock")))
  ))
  list(db = db, items = items, teams = teams)
}

seed_items <- function(f, conn) {
  init_db(f$items, conn = conn)
  init_db(f$teams, conn = conn)
  insert_record(f$items, list(code = "scope", name = "Scope", emoji = "\U0001F52D", stock = 3), conn = conn)
  insert_record(f$items, list(code = "compass", name = "Compass", emoji = "\U0001F9ED"), conn = conn)
}

basket <- function(...) {
  n <- c(...)
  data.frame(item = names(n), n = unname(n), stringsAsFactors = FALSE)
}
