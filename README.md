LEGO Mosaics in R
================

brickr <img src='man/figures/logo.png' align="right" height="138" />
====================================================================

<!-- <!-- badges: start -->
--&gt; <!-- [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) --> <!-- <!-- badges: end --> --&gt;

Overview
--------

**brickr** is a package for creating LEGO-esque 2D and 3D models using the R [tidyverse](https://www.tidyverse.org/) and [Tyler Morgan-Wall](https://twitter.com/tylermorganwall)'s [rayshader](https://github.com/tylermorganwall/rayshader) package.

The package has two key uses: - Converting image files in to 2D and 3D LEGO mosaics - Building 3D LEGO models from simple data frames

Installation
------------

``` r
# To install the latest version from Github:
# install.packages("devtools")
devtools::install_github("ryantimpe/brickr")
```

A full explanation can be found on [this blog post](http://www.ryantimpe.com/post/lego-mosaic1/), this [follow-up post](http://www.ryantimpe.com/post/lego-mosaic2/),and this [third post](http://www.ryantimpe.com/post/lego-mosaic3/).

<img src='Images/goldengirls.JPG'  height="180" />

![](README_files/figure-markdown_github/m1_set-1.png)

This process is competed in a few distinct steps:

-   `scale_image()` reduces the image to a number of brick "pixels". Providing a single value, such as `48`, crops the image to a square. Inputting a 2-element array, `c(56, 48)`, will output a rectangular image of `c(width, height)`. Use `brightness` to adjust the light of the image. Values greater than 1 will lighten the image, while value less than 1 will darken it.

-   `legoize()` converts every brick-sized pixel in the scaled image to an official LEGO brick color. Those colors are stored in `Colors/Lego_Colors.csv`. By default, the functions look at only currently produced, non-transparent colors. Use `theme = 'bw'`to create a 4-color grayscale mosaic.

-   `collect_bricks()` looks for adjacent groups of the same color to replace single 1 x 1 bricks with larger bricks.

-   `display_set()` renders the LEGO mosaic as a plot for viewing, creating the image above.

``` r
mosaic1 <- readJPEG("Images/goldengirls.jpg") %>% 
  scale_image(48) %>%
  legoize() %>% 
  collect_bricks() 

mosaic1 %>% display_set()
```

3D Mosaics
----------

Two additional functions can convert the `collect_bricks()` output into a 3D mosaic using the [rayshader](https://github.com/tylermorganwall/rayshader) package by [Tyler Morgan-Wall](https://twitter.com/tylermorganwall).

-   `collect_3d()` translates the 2D LEGO mosaic into two matrices - one for the color image and one for the elevation of each point on the image. By default, the produced image has the height of 6 LEGO plates (2 LEGO bricks) with lighter colors having a higher elevation. Use `mosaic_height` to change the height of the mosaic and set `highest_el = 'dark'` to set the darker colors as the tallest bricks.

-   `display_3d()` simply calls `rayshader::plot_3d()`, but pulls both the hillshade and elevation matrices from the output of `collect_3d()` and fixes the `zscale` value. See `?rayshader::plot_3d()` for more information.

``` r
library(rayshader)

mosaic1 %>% 
  collect_3d() %>% 
  display_3d(fov=0,theta=-20,phi=30,windowsize=c(1000,800),zoom=0.7)

render_snapshot()
```

![](README_files/figure-markdown_github/m1_3d-1.png)

LEGO Mosaics IRL
----------------

Additional functions assist in the translation from the LEGO mosaic image into a real LEGO set.

### Instructions

Use `generate_instructions()` to break the LEGO mosaic image into easier-to-read steps for building the set. This defaults to 6 steps, but passing any integer value will generate that many steps.

``` r
mosaic1 %>% generate_instructions(9)
```

![](README_files/figure-markdown_github/m1_instructions-1.png)

### Piece list and count

Use `display_pieces()` to generate a graphic and count of all required plates or bricks (for stacked mosaics). These are sorted by color and size for easy purchase on LEGO.com's [Pick-a-Brick](https://shop.lego.com/en-US/Pick-a-Brick) section using the advanced search option. Alternatively, use `table_pieces()` to produce a data frame table of all required bricks.

``` r
mosaic1 %>% display_pieces()
```

![](README_files/figure-markdown_github/m1_pieces-1.png)

Stacked mosaics
---------------

The default produces instructions for a flat LEGO mosaic, with all bricks placed "stud-up" on a plate. Alternatively, specifying `mosaic_type = "stacked"` in the `collect_bricks()` function will generate a mosaic where all bricks are stack on top of each other, resulting in the mosaic image being visible from the side.

A 1 x 1 LEGO brick is taller than it is wide by a ratio of 6/5, so it's recommended to use a wider image.

``` r
m2_lego <- readJPEG("Images/goldengirls2.jpg") %>% 
  scale_image(c(56, 48)) %>% #c(Width, Height) for rectangle
  legoize() %>% 
  collect_bricks("stacked") 
```

![](README_files/figure-markdown_github/m2_set-1.png)

``` r
mosaic2 %>% display_pieces()
```

![](README_files/figure-markdown_github/m2_pieces-1.png)

Color options
-------------

For now, you can limit the available LEGO brick colors by filtering rows in the included `Colors/Lego_Colors.csv` file.

You can also use `legoize(theme='bw')` to create a 4-color grayscale mosaic. Set the `contrast` input to be less than or greater than 1 to adjust the allocation of colors.

``` r
readJPEG("Images/goldengirls.jpg") %>% 
  scale_image(48) %>%
  legoize(theme='bw', contrast = 1.25) %>% 
  collect_bricks() %>% 
  display_set()
```

![](README_files/figure-markdown_github/m1_bw-1.png)
