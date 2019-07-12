# brickr 0.1.0.0000

**Mosaics**

* New rendering of mosaics in ggplot2. Includes embossed text with custom labels.
* Updated brick collection algorithm to allow for custom brick input.

**3D Models**

* `brick_res` input options to render models in higher definition ('sd', 'hd', 'uhd')
* Updated brick collection algorithm staggers bricks over layer, though still prioritizes larger bricks.

**ggplot Extension**

* `geom_brick_col` for bar charts in the shape of bricks.
* `coord_brick` to prevent chart brick distortion. `coord_brick_flip` for horizontal bars.

### TO DO

* negative bars (knobs at 0)
* negative bars (partial bricks lowest)
* LEGO color themes
* DOCUMENTATION
* Website
* Check()
* CRAN!

----

# brickr 0.0.0.9200

* Added `bricks_from_excel()` as a way to decrease the starting cost of using brickr. 
* See [https://github.com/ryantimpe/brickr_toybox](https://github.com/ryantimpe/brickr_toybox)

# brickr 0.0.0.9150

* Added `bricks_from_coords()` function to convert a long data frame with x, y, z, and Color columns into input for `display_bricks()`.
* Aesthetic updates to the README and logo.
* Added a `NEWS.md` file to track changes to the package.
