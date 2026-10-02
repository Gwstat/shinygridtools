# A SQLite connection for one test, disconnected automatically when the
# calling test_that() block finishes. withr is an imported dependency of
# testthat, so it is always available here. shinyformtools itself is attached
# with the package (Depends), so the tests use its functions unqualified.
local_test_conn <- function(path = tempfile(fileext = ".sqlite"),
                            .local_envir = parent.frame()) {
  conn <- shinyformtools::db_connect(path)
  withr::defer(shinyformtools::db_disconnect(conn), envir = .local_envir)
  conn
}
