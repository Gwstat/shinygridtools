# The grid lives outside shinyformtools and reaches it through the extension
# API only: nothing with its internal prefix is called from here, and every
# shinyformtools function the sources use is exported by it.

test_that("the sources call no shinyformtools internals", {
  dir <- testthat::test_path("..", "..", "R")
  skip_if(!dir.exists(dir), "sources not available")

  code <- unlist(lapply(list.files(dir, full.names = TRUE), readLines, warn = FALSE))
  called <- unique(sub("\\($", "", unlist(regmatches(code, gregexpr("\\bsft_[A-Za-z0-9_]+\\(", code)))))
  expect_identical(called, character())
  expect_false(any(grepl("shinyformtools:::", code, fixed = TRUE)))
})

test_that("every shinyformtools function the package imports is exported there", {
  imports <- getNamespaceImports("shinygridtools")$shinyformtools
  expect_true(length(imports) > 0L)
  expect_true(all(imports %in% getNamespaceExports("shinyformtools")))
})

test_that("the module's labels follow the language", {
  expect_identical(ui_label("grid_saved", list(time = "12:00")), "Saved 12:00")
  expect_identical(with_language(german(), ui_label("grid_sum")), "Summe")
  expect_identical(language(labels = list(grid_save = "Go"))$labels$grid_save, "Go")
})
