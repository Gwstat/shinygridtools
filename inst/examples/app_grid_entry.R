# Grid entry over several records: a bird survey. Volunteers count the birds
# at a survey site, per species and distance band, on two visits.
#
# One stored record per visit x site x species; the three distance-band
# counts are its numeric fields. grid_server() shows one site's species as a
# grid whose rows are the records and whose columns are the counts:
#
#   * saving is automatic half a second after the last change, in ONE
#     transaction through upsert_records(): only the cells the user changed
#     are written, a new row is inserted, a row with nothing left in it is
#     soft-deleted, so the table holds only what was actually seen;
#   * two people on the same site: each sees the other's entries within a
#     few seconds (`poll`), and a clash in the same cell is refused with a
#     message instead of overwriting (`conflict_check`);
#   * on the second visit the first visit's counts show greyed inside the
#     cells (`hint`), as a reference, never as the value;
#   * Enter and the arrow keys walk the cells, a block from a spreadsheet can
#     be pasted, sums follow the entries;
#   * the records table below is the regular form module on the same table,
#     refreshed by the grid's `changed`, with the export buttons.
#
# Run with: shinygridtools::run_example("app_grid_entry")

library(shiny)
library(shinyformtools)
library(shinygridtools)

db_path <- tempfile(fileext = ".sqlite")

# ---- Describe the records behind the grid ------------------------------------
# `visit` and `site` identify the group the grid shows, `species` the row.
# The three numeric fields become the grid's columns; a text field would stay
# out of the grid.
visits <- c("First visit" = "1", "Second visit" = "2")
sites <- c("Pond" = "pond", "Meadow" = "meadow", "Forest edge" = "forest")
species <- c("Blackbird", "Robin", "Great tit", "Chaffinch", "Other")

counts_form <- form(
  form_id = "bird_counts",
  form_name = "Bird counts",
  table_name = "bird_counts",
  db = db_sqlite(db_path),
  fields = list(
    form_field(id = "visit", label = "Visit", input_type = "selectInput", args = list(choices = visits)),
    form_field(id = "site", label = "Site", input_type = "selectInput", args = list(choices = sites)),
    form_field(id = "species", label = "Species", input_type = "selectInput", args = list(choices = species)),
    form_field(id = "near", label = "Under 25 m", input_type = "numericInput", args = list(value = 0, min = 0)),
    form_field(id = "middle", label = "25 to 100 m", input_type = "numericInput", args = list(value = 0, min = 0)),
    form_field(id = "far", label = "Over 100 m", input_type = "numericInput", args = list(value = 0, min = 0))
  ),
  validation_rules = list(
    forbid_if(
      id = "no_negative_counts",
      condition = function(values) any(unlist(values[c("near", "middle", "far")]) < 0, na.rm = TRUE),
      fields = c("near", "middle", "far"),
      message = "Counts cannot be negative."
    )
  )
)

# ---- Seed the first visit at the pond -----------------------------------------
local({
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)
  init_db(counts_form, conn = conn, user = "demo")

  if (nrow(fetch_records(counts_form, conn = conn)) == 0L) {
    upsert_records(
      counts_form,
      data.frame(
        visit = "1", site = "pond", species = c("Blackbird", "Robin", "Great tit"),
        near = c(2, 0, 1), middle = c(4, 3, 0), far = c(1, 2, 0)
      ),
      key = c("visit", "site", "species"), conn = conn, user = "demo"
    )
  }
})

# ---- The first visit as a hint on the second ----------------------------------
# `hint` gets the group and returns a matrix of the grid's shape or NULL.
# Here it reads the other visit's records of the same site.
first_visit_as_hint <- function(group) {
  if (!identical(group$visit, "2")) {
    return(NULL)
  }
  conn <- db_connect(db_sqlite(db_path))
  on.exit(db_disconnect(conn), add = TRUE)
  stored <- fetch_records(counts_form, conn = conn)
  first <- stored[stored$visit == "1" & stored$site == group$site, , drop = FALSE]
  m <- matrix(NA_real_, nrow = length(species), ncol = 3)
  i <- match(first$species, species)
  m[i, ] <- as.matrix(first[, c("near", "middle", "far")])
  m
}

how_to <- function() {
  shiny::div(
    style = paste(
      "margin-bottom: 1rem; padding: 0.75rem 1rem;",
      "border-left: 4px solid #4178be; background: #eef3fb; border-radius: 4px;"
    ),
    shiny::tags$strong("How to use it"),
    shiny::tags$p(
      style = "margin: 0.4rem 0 0;",
      "Pick a visit and a site, then type the counts into the grid: Enter moves to the next species, ",
      "the arrow keys change the distance band, and a block copied from a spreadsheet can be pasted. ",
      "Everything saves by itself; each row is one record in the table below. ",
      "On the second visit, the first visit's counts show greyed inside the cells."
    )
  )
}

# ---- Wire the grid and the records table --------------------------------------
# `group` is a reactive over the two selectors, so choosing another site
# re-renders the grid with that site's records. The form module below shows
# the same table and refreshes on the grid's `changed`.
server <- function(input, output, session) {
  grid <- grid_server(
    "grid", counts_form,
    rows = species, key = "species",
    group = reactive(list(visit = input$visit, site = input$site)),
    hint = first_visit_as_hint,
    user = "demo"
  )

  form_server(
    "records", counts_form, user = "demo",
    columns = list(visible = c("visit", "site", "species", "near", "middle", "far"), persist = FALSE),
    refresh_triggers = grid$changed,
    # The grid saves as people type; the table below is rebuilt at most
    # once a second instead of after every save.
    table = list(refresh_delay = 1000)
  )
}

# ---- Build the UI -------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Bird survey"),
  how_to(),
  fluidRow(
    column(3, selectInput("visit", "Visit", choices = visits)),
    column(4, selectInput("site", "Site", choices = sites))
  ),
  grid_ui("grid", label = "Birds per species and distance"),
  hr(),
  form_ui("records", title = "Stored records", show_export = TRUE)
)

shinyApp(ui, server)
