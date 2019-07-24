
#' Convert image raster array to a LEGO-esque mosaic. Wrapper function.
#'
#' @param image_list List output from scale_image(). Contains an element  \code{Img_scaled}.
#' @param img_size Size of output image in pixel, where one pixel = one 'brick'. Use a single value (e.g. \code{48}) for a square image with 48 pixels on each side. 
#' Use an array of two values for a rectangular image \code{c(width, height)}.
#' @param color_table Defaults to \code{lego_colors}. Data frame of brick colors to map onto image. Must contain Name and R, G, B channels. See attached data  \code{lego_colors} as examples.
#' @param use_bricks Array of brick sizes to use in mosaic. Defaults to \code{c('4x2', '2x2', '3x1', '2x1', '1x1')}`.
#' @param mosaic_type DEPRECATED. All mosaics are "flat" or knobs-up.
#' @param brightness A value >1 will increase the brightness of the image while a positive value <1 will decrease the brightness.
#' @param warhol Array of values \code{c(1, 2, 3)} associated with R, G, B color channels. Swap values in array to swap color channels for a fun visual effect.
#' @param brick_theme Theme of brick colors to use. Set to \code{"bw"} for grayscale mosaics.
#' @param contrast For \code{theme = "bw"}. A value >1 will increase the contrast of the image while a positive value <1 will decrease the contrast.
#' @return A list with element \code{Img_lego} containing a data frame of the x- & y-coordinates, R, G, B channels, and mapped color of each brick (pixel).
#' @export 
#'
image_to_bricks <- function(img, img_size = 48, color_table = lego_colors, 
                            mosaic_type = NULL, use_bricks = NULL, 
                            brightness = 1, warhol = 1:3, brick_theme = "default", contrast = 1){
  
  in_list <- img %>% 
    image_to_scaled(img_size = img_size, brightness = brightness, warhol = warhol) %>% 
    scaled_to_colors(color_table = color_table, theme = brick_theme, contrast = contrast) %>% 
    collect_bricks(use_bricks = use_bricks, mosaic_type = mosaic_type)
  
  return(in_list)
}