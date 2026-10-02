#' @keywords internal
"_PACKAGE"

# The package-level help page (?shinygridtools) is generated from the
# DESCRIPTION by roxygen2. See README.md and the bundled example app
# (run_example("app_grid_entry"), run_example("app_cart_input")) for an overview.
#
# Everything the grid and the basket need from shinyformtools comes through its extension
# API (?shinyformtools::db_helpers, ?shinyformtools::module_helpers, ...);
# nothing internal is touched, and test-extension-api.R keeps it that way.
#' @importFrom shinyformtools form_fields key_fields resolve_editable
#' @importFrom shinyformtools module_connection prepare_write find_records_by_key
#'   records_stamp after_commit upsert_records
#' @importFrom shinyformtools with_language ui_labels ui_label
#' @importFrom shinyformtools module_permission module_editable_fields apply_edit_rules
#' @importFrom shinyformtools find_field record_value form_message validation_issue
#' @importFrom shinyformtools db_backend in_transaction table_exists after_transaction
#'   update_record with_transaction
NULL

`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}
