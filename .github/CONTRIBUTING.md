# Contributing to shinygridtools

Thanks for taking the time to contribute! Bug reports, feature ideas, and pull
requests are all welcome.

## Reporting bugs and requesting features

Open an issue at <https://github.com/Gwstat/shinygridtools/issues>. For bugs,
please include:

- a minimal reproducible example (a small `form()` and the `grid_server()` or
  `cart_input` call that fails),
- the backend you used (SQLite / DuckDB / MariaDB),
- your R version, the versions of shinygridtools and shinyformtools, and the
  output of `sessionInfo()`.

A problem with forms in general (dialogs, the records table, schema,
validation) belongs to [shinyformtools](https://github.com/Gwstat/shinyformtools/issues).

## Development setup

```r
# from a fork/clone of the repo
install.packages("devtools")
devtools::install_dev_deps()   # Imports + Suggests, shinyformtools from GitHub
```

## Everyday workflow

```r
devtools::load_all()   # reload the package during development
devtools::document()   # regenerate man/ and NAMESPACE after roxygen changes
devtools::test()       # run the testthat suite
devtools::check()      # full R CMD check before opening a PR
```

`R CMD check` must stay clean: **0 errors, 0 warnings, 0 notes**.

## Code conventions

- Comments and roxygen are written in **English**; names are **snake_case**.
- Exported functions use bare names (`grid_input()`, `grid_server()`,
  `cart_input()`); internal helpers carry the `sgt_` prefix. Names the browser
  sees are `sgt-grid*`, `sgt-cart*`, `sgt-board*` (CSS classes, HTML
  dependencies) and `shinygridtools.grid` / `shinygridtools.cart` (input
  bindings).
- shinyformtools is used **only through its exported extension API**: no
  `sft_*` call and no `:::`. `tests/testthat/test-extension-api.R` checks
  both. If something is missing, it belongs in the shinyformtools API first.
- The dependency goes one way: shinygridtools depends on shinyformtools,
  never the reverse. Field types, texts and the board are registered from
  this package in `.onLoad()`.
- **All SQL uses parameterized queries**; quote identifiers with
  `DBI::dbQuoteIdentifier()`.
- Keep R source **ASCII** (escape non-ASCII as `\uXXXX`).

## Pull requests

1. Branch from `main` and keep each PR focused on one change.
2. Add or update tests for the behaviour you change.
3. Run `devtools::document()` and `devtools::check()` (expect 0/0/0).
4. Add a bullet to `NEWS.md` for any user-facing change.

## License

By contributing, you agree that your contributions are licensed under the
project's [MIT license](../LICENSE).
