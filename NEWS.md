# brickr 0.1.1.0000

* **Breaking:** Pretty much *EVERY* function. Seriously, check out the README and start fresh.

* **Breaking:** Data "lego_colors.rda" has been updated with more accurate RGB values and new `brickrID` numbers. This will impact previously created mosaics and 3D models.

**Mosaics**

* New rendering of mosaics in ggplot2. Includes embossed text with custom labels.
* Color_palette allows option to choose brick colors by rarity. 'universal' (most common), 'generic', and 'special' (least common).
* New color matching options to convert image to available brick colors. Previous option still available, but results will look different due to changed RGB values.
* Updated brick collection algorithm to allow for custom brick input.
* 3D mosaics have been rewritten as 3D models using plates with `bricks_from_mosaic()`.

**3D Models**

* `brick_res` input options to render models in higher definition ('sd', 'hd', 'uhd')
* Option to use plates rather than bricks. Combining the two involves some hacking.
* Updated brick collection algorithm staggers bricks over layer, though still prioritizes larger bricks.

**ggplot Extension**

* `geom_brick_col` for bar charts in the shape of bricks. Negative values are fine, but `position = stack` is not available.
* `coord_brick` to prevent chart brick distortion. `coord_brick_flip` for horizontal bars.
* `scale_fill_brick` and `theme_brick` for different LEGO color options.

### TO DO

* ggplot - continuous scale
* Mosaic [dithering](https://en.wikipedia.org/wiki/Floyd%E2%80%93Steinberg_dithering)
* 3D model instructions... level by level
* Vignettes
    - Mosaics
    - 3D models from tables
    - 3D models from coords
    - ggplot
    - IRL
* Website
* Check() breaks at the size check
* CRAN!

### TO DO for a future update 

* Negative bricks are "underside"
* bricks_from_models

----

# brickr 0.0.0.9200

* Added `bricks_from_excel()` as a way to decrease the starting cost of using brickr. 
* See [https://github.com/ryantimpe/brickr_toybox](https://github.com/ryantimpe/brickr_toybox)

----

# brickr 0.0.0.9150

* Added `bricks_from_coords()` function to convert a long data frame with x, y, z, and Color columns into input for `display_bricks()`.
* Aesthetic updates to the README and logo.
* Added a `NEWS.md` file to track changes to the package.
