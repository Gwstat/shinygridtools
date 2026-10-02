#' The bundled example apps
#'
#' `list_examples()` names the example apps shipped in `inst/examples`,
#' `example_path()` gives the path of one, `run_example()` runs it.
#'
#' @param example Example name, with or without `.R`.
#' @param ... Passed to [shiny::runApp()].
#'
#' @return `list_examples()`: a character vector of names. `example_path()`:
#'   the path of the file. `run_example()` runs the app.
#' @examples
#' list_examples()
#' \dontrun{
#' run_example("app_grid_entry")
#' }
#' @name examples
NULL

sgt_examples_dir <- function() {
  system.file("examples", package = "shinygridtools", mustWork = TRUE)
}

#' @rdname examples
#' @export
list_examples <- function() {
  sub("\\.R$", "", list.files(sgt_examples_dir(), pattern = "^[^_].*\\.R$"))
}

#' @rdname examples
#' @export
example_path <- function(example) {
  if (!is.character(example) || length(example) != 1L || is.na(example) || !nzchar(example)) {
    stop("example must be a non-empty character scalar.", call. = FALSE)
  }

  filename <- if (grepl("\\.R$", example)) example else paste0(example, ".R")
  path <- file.path(sgt_examples_dir(), filename)

  if (!file.exists(path)) {
    stop(
      "Example not found: ", filename, ". Available examples: ",
      paste(list_examples(), collapse = ", "), call. = FALSE
    )
  }

  path
}

#' @rdname examples
#' @export
run_example <- function(example, ...) {
  shiny::runApp(appDir = example_path(example), ...)
}
