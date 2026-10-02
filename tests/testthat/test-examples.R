# The example app is not launched here, but it must be found and parse, so a
# broken example is caught before a user runs run_example().

test_that("the example apps are listed and parse", {
  expect_true("app_grid_entry" %in% list_examples())

  for (example in list_examples()) {
    path <- example_path(example)
    expect_true(file.exists(path))
    expect_no_error(parse(file = path))
  }

  expect_error(example_path("nope"), "Example not found")
  expect_error(example_path(""), "non-empty")
})
