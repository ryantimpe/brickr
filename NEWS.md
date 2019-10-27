# brickr 0.2.1

## Documentation

* Issues and bugs are now actively tracked on [GitHub Issues](https://github.com/ryantimpe/brickr/issues).

## 3D Models

* `build_bricks()` now renders models in {rgl}, rather than {rayshader}. Most options for the rendering have changed. Use `build_brick_rayshader()` for previous output.

----

# brickr 0.1.1

* **Breaking:** Pretty much *EVERY* function. Seriously, check out the README and start fresh.

* **Breaking:** Data "lego_colors.rda" has been updated with more accurate RGB values and new `brickrID` numbers. This will impact previously created mosaics and 3D models.

## Documentation

* pkgdown site
* Vignettes

## Mosaics

* New rendering of mosaics in ggplot2. Includes embossed text with custom labels.
* Color_palette allows option to choose brick colors by rarity. 'universal' (most common), 'generic', and 'special' (least common).
* New color matching options to convert image to available brick colors. Previous option still available, but results will look different due to changed RGB values.
* Color [dithering](https://en.wikipedia.org/wiki/Floyd%E2%80%93Steinberg_dithering) option for large, photo-realistic mosaics.
* Updated brick collection algorithm to allow for custom brick input.
* 3D mosaics have been rewritten as 3D models using plates with `bricks_from_mosaic()`.

## 3D Models

* `brick_res` input options to render models in higher definition ('sd', 'hd', 'uhd')
* `bricks_from_rayshader()` to render LEGO models from rayshader plot_3d() input.
* Option to use plates rather than bricks. Combining the two involves some hacking.
* Updated brick collection algorithm to allow for custom brick input.
* Updated brick collection algorithm staggers bricks over layer, though still prioritizes larger bricks.
* `build_instructions` generates building instructions for 3D models, as well as mosaics.

## ggplot Extension

* `geom_brick_col` for bar charts in the shape of bricks. Negative values are fine, but `position = stack` is not available.
* `coord_brick` to prevent chart brick distortion. `coord_brick_flip` for horizontal bars.
* `scale_fill_brick` and `theme_brick` for different LEGO color options.

----

# brickr 0.0.0.9200

* Added `bricks_from_excel()` as a way to decrease the starting cost of using brickr. 
* See [https://github.com/ryantimpe/brickr_toybox](https://github.com/ryantimpe/brickr_toybox)

----

# brickr 0.0.0.9150

* Added `bricks_from_coords()` function to convert a long data frame with x, y, z, and Color columns into input for `display_bricks()`.
* Aesthetic updates to the README and logo.
* Added a `NEWS.md` file to track changes to the package.
