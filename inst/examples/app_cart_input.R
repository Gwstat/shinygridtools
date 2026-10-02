# A basket field: survey teams borrow equipment from a shared store.
#
# Each team gets its kit through a cart_input field: items are dragged from
# the palette into the basket (or clicked), + and - set the count, and a line
# marked "on site" stays with the team for the whole season.
#
#   * The palette comes from a second form, "Equipment", whose records hold
#     name, emoji and total stock (cart_catalog()). Edit the stock there and
#     the next dialog shows it.
#   * Each item shows how many are still free: the stock minus what the other
#     teams hold. Lines kept on site do not take from the stock.
#   * A save that would take more than is left is refused, also when two
#     people take the last one at the same moment, and also from scripts.
#     Deleting a team frees its equipment.
#   * The records table shows the basket as "2 x Spotting scope, 1 x Compass".
#   * form_ui(show_board = TRUE) adds an overview: every team on the left,
#     the store on the right. Drag an item from the store onto a team, from
#     one team to another, or back onto the store; each move is saved at once
#     with the same stock check. Open it in two windows to see the other
#     window's moves appear within a few seconds.
#
# Run with: shinygridtools::run_example("app_cart_input")

library(shiny)
library(shinyformtools)
library(shinygridtools)

db_path <- tempfile(fileext = ".sqlite")

# ---- Describe the items and their stock --------------------------------------
# The code is what a basket stores, so keep it stable; name, emoji and stock
# can change any time.
equipment_form <- form(
  form_id = "equipment",
  form_name = "Equipment",
  table_name = "equipment",
  db = db_sqlite(db_path),
  fields = list(
    form_field(id = "code", label = "Code", mandatory = TRUE, unique = TRUE),
    form_field(id = "name", label = "Name", mandatory = TRUE),
    form_field(id = "emoji", label = "Icon"),
    form_field(id = "stock", label = "Stock", input_type = "numericInput",
               args = list(min = 0, step = 1))
  )
)

# ---- Give each team a basket ----------------------------------------------------
# cart_catalog() names the items form and which fields hold id, label, icon
# and stock.
teams_form <- form(
  form_id = "teams",
  form_name = "Teams",
  table_name = "teams",
  db = db_sqlite(db_path),
  fields = list(
    form_field(id = "team", label = "Team", mandatory = TRUE),
    form_field(id = "area", label = "Survey area"),
    form_field(
      id = "kit", label = "Equipment", input_type = "cart_input",
      args = list(catalog = cart_catalog(
        equipment_form, id = "code", label = "name", icon = "emoji", stock = "stock"
      ))
    )
  )
)

# ---- Fill in sample items and teams ---------------------------------------------
# So the page is not empty: five items with their stock and a few teams, one
# of which already holds some of them (one camera kept on site).
local({
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn))
  init_db(equipment_form, conn = conn)
  init_db(teams_form, conn = conn)
  items <- data.frame(
    code = c("scope", "camera", "compass", "clipboard", "vest"),
    name = c("Spotting scope", "Camera", "Compass", "Clipboard", "Safety vest"),
    emoji = c("\U0001F52D", "\U0001F4F7", "\U0001F9ED", "\U0001F4CB", "\U0001F9BA"),
    stock = c(3, 4, 6, 12, 10)
  )
  for (i in seq_len(nrow(items))) {
    insert_record(equipment_form, as.list(items[i, ]), conn = conn, user = "demo")
  }
  insert_record(teams_form, list(
    team = "Pond team", area = "Mill pond and reeds",
    kit = data.frame(
      item = c("scope", "camera", "camera", "clipboard"),
      label = c("Spotting scope", "Camera", "Camera", "Clipboard"),
      icon = c("\U0001F52D", "\U0001F4F7", "\U0001F4F7", "\U0001F4CB"),
      n = c(1, 1, 1, 2),
      on_site = c(FALSE, FALSE, TRUE, FALSE)
    )
  ), conn = conn, user = "demo")
  for (name in c("Meadow team", "Forest team", "River team", "Town team")) {
    insert_record(teams_form, list(team = name), conn = conn, user = "demo")
  }
})

# ---- Explain the page --------------------------------------------------------
how_to <- function() {
  shiny::tags$div(
    style = paste(
      "margin-bottom: 1rem; padding: 0.75rem 1rem;",
      "border-left: 4px solid #4178be; background: #eef3fb; border-radius: 4px;"
    ),
    shiny::tags$strong("How to use it"),
    shiny::tags$p(
      style = "margin: 0.4rem 0 0;",
      "Add or edit a team: drag items from the palette into the basket, or click them. ",
      "Each item shows how many are still free. \"on site\" marks what stays with the team ",
      "for the season; it does not count against the store. ",
      "The overview above the table is faster: drag from the store on the right onto a team, ",
      "between teams, or back into the store; every move saves at once. ",
      "The stock itself is edited in the Equipment tab."
    )
  )
}

# ---- Wire the two forms ------------------------------------------------------
# The basket reads the items when a dialog opens; nothing else to wire.
server <- function(input, output, session) {
  form_server("teams", teams_form, user = "demo", columns = list(persist = FALSE))
  form_server("equipment", equipment_form, user = "demo", columns = list(persist = FALSE))
}

# ---- Build the UI ------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Equipment for the survey teams"),
  how_to(),
  tabsetPanel(
    tabPanel("Teams", form_ui("teams", title = "Teams", show_board = TRUE)),
    tabPanel("Equipment", form_ui("equipment", title = "Equipment and stock"))
  )
)

shinyApp(ui, server)
