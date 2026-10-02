# shinygridtools 0.3.2

* New example data, in English: `app_grid_entry` is a bird survey (counts per
  site, species and distance band on two visits), `app_cart_input` lends
  equipment to survey teams from a shared store. README, vignette, help
  examples and screenshots follow.
* The board's CSS classes are `sgt-board-records` and `sgt-board-record`
  (were `sgt-board-stations` and `sgt-board-station`); custom CSS against the
  old names needs the new ones. Behaviour is unchanged.

# shinygridtools 0.3.1

* Documentation: a README with an introduction, screenshots and examples, a
  vignette (`vignette("shinygridtools")`) that walks through the grid and the
  basket with what they store, a hex logo and a contributing guide. No change
  in behaviour.

# shinygridtools 0.3.0

* Needs shinyformtools 0.10.0, which no longer knows this package. The board
  of `form_ui(show_board = TRUE)` is registered there with
  `register_ui_part()` when this package loads.
* Apps attach this package with `library(shinygridtools)`; shinyformtools
  does not load it on its own any more.

# shinygridtools 0.2.0

* Needs shinyformtools 0.9.0, which no longer has the grid and the basket
  itself. The `grid_input` and `cart_input` types are now always this
  package's (before, a shinyformtools 0.8.x with its own built-ins served
  form fields of these types).

# shinygridtools 0.1.0

The grid and the basket of shinyformtools 0.8.0 as a package of their own,
built on the extension API of shinyformtools (`?shinyformtools::db_helpers`
and friends).

* `grid_input()` / `update_grid_input()`: a grid of number cells as a Shiny
  input, and as the `input_type = "grid_input"` of
  `shinyformtools::form_field()`.
* `grid_ui()` / `grid_server()`: grid entry over the records of a group, wide
  (one record per row) or long (one record per cell, `col_key`), with
  autosave, per-cell conflict checks and other users' entries polled in.
* `cart_input()` / `update_cart_input()` / `cart_catalog()`: a basket field
  whose palette comes from a fixed item list or from another form, with a
  stock shared by every basket that uses it and checked on every save.
* `cart_board_ui()`: the basket board (every record as a drop target, the
  stock beside it, one drag moves one piece). `form_ui(show_board = TRUE)` of
  shinyformtools renders the same board.
* Behaviour and arguments are those of shinyformtools 0.8.0. What changed are
  the names Shiny and the browser see: the input handlers and bindings are
  `shinygridtools.grid` and `shinygridtools.cart`, the HTML dependencies
  `sgt-grid`, `sgt-cart` and `sgt-board`, the CSS classes `sgt-grid-*`,
  `sgt-cart-*` and `sgt-board-*`. Custom CSS written against `sft-*` needs
  the new class names.
* Fixed (carried over from shinyformtools 0.8.0): `grid_server(permissions =
  list(editable_fields = ...))` failed with "$ operator is invalid for atomic
  vectors".
* While shinyformtools still ships its own `grid_input` and `cart_input`
  types (up to 0.8.x), a form field of either type renders and saves with
  shinyformtools' copy; `grid_server()` is unaffected. Once shinyformtools
  drops them, this package's registrations take over.
* Example apps `app_grid_entry` and `app_cart_input` (`run_example()`).
