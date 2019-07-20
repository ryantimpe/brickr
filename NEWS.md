# brickr 0.1.0.0000

* **Breaking:** Data "lego_colors.rda" has been updated with more accurate RGB values and new `brickrID` numbers. This will impact previously created mosaics and 3D models.
* **Breaking:** Pretty much every function name.

**Mosaics**

* New rendering of mosaics in ggplot2. Includes embossed text with custom labels.
* Updated brick collection algorithm to allow for custom brick input.

**3D Models**

* `brick_res` input options to render models in higher definition ('sd', 'hd', 'uhd')
* Updated brick collection algorithm staggers bricks over layer, though still prioritizes larger bricks.

**ggplot Extension**

* `geom_brick_col` for bar charts in the shape of bricks. Negative values are fine, but `position = stack` is not available.
* `coord_brick` to prevent chart brick distortion. `coord_brick_flip` for horizontal bars.

### TO DO

* LEGO color themes
* Negative bricks are "underside"
* bricks_from_models
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
