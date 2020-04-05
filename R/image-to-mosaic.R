#' Create a 2D LEGO mosaic from an image array
#' 
#' Generate brick mosaics from an image or matrix with customization options.
#'
#' @param img Image matrix to convert into mosaic. Usually from \code{\link[jpeg]{readJPEG}} or \code{\link[png]{readPNG}}.
#' @param img_size Size of output image in pixel, where one pixel = one 'brick'. Use a single value (e.g. \code{48}) for a square image with 48 pixels on each side. 
#' Use an array of two values for a rectangular image \code{c(width, height)}.
#' @param method The method to use for comparison. Options are 'euclidean', 'cie1976', 'cie94', 'cie2000', or 'cmc'. 
#' See \code{\link[farver]{compare_colour}}.
#' @param color_table Defaults to \code{lego_colors}. Data frame of brick colors to map onto image. Must contain Name and R, G, B channels. 
#' See attached data  \code{lego_colors} as examples.
#' @param color_palette Brick color rarity to use. Defaults to all colors: 'universal' (most common), 'generic', and 'special' (least common). This is useful when trying to build the mosaic out of real bricks.
#' Use "bw" for only grayscale bricks. Ignored if a \code{color_table} is supplied.
#' @param use_bricks Array of brick sizes to use in mosaic. Defaults to \code{c('4x2', '2x2', '3x1', '2x1', '1x1')}`.
#' @param dithering Improves color of large, photo-realistic mosaics. 
#' @param brightness A value >1 will increase the brightness of the image while a positive value <1 will decrease the brightness.
#' @param warhol Array of values \code{c(1, 2, 3)} associated with R, G, B color channels. Swap values in array to swap color channels for a fun visual effect.
#' @param contrast For \code{theme = "bw"}. A value >1 will increase the contrast of the image while a positive value <1 will decrease the contrast.
#' @return A list with element \code{Img_lego} containing a data frame of the x- & y-coordinates, R, G, B channels, and mapped color of each brick (pixel).
#' @family Mosaics
#' @export 
#' @examples 
#' # Import a jpeg or png
#'  demo_file <- system.file("extdata", "demo_img.jpg", 
#'                           package = "brickr", mustWork = TRUE)
#'  demo_image <- jpeg::readJPEG(demo_file)
#'  
#'  #Create a 24x24 mosaic
#'  \donttest{
#'  demo_image %>% 
#'    image_to_mosaic(img_size = 24) %>% 
#'    build_mosaic()
#'  }
#' 
#'  #Only use the two more common tiers of colors
#'  \donttest{
#'  demo_image %>% 
#'   image_to_mosaic(img_size = 24, 
#'                   color_palette = c("universal", "generic")) %>% 
#'    build_mosaic()
#'  }
#'  
#'  #Be more prescriptive with colors using 'color_table'.
#'  # Here, we prevent all blues from being used
#'  lego_colors_no_blue = lego_colors %>% 
#'     dplyr::filter(!grepl("[Bb]lue|[Aa]zur", Color))
#'  \donttest{
#'  demo_image %>% 
#'   image_to_mosaic(img_size = 24, 
#'                   color_table = lego_colors_no_blue) %>% 
#'    build_mosaic()
#'  }
#'  
#'  #Color matching is done with the 'farver' package. There are different method.
#'  # Change the method to euclidean (RGB distance matching)
#'  \donttest{
#'  demo_image %>% 
#'   image_to_mosaic(img_size = 24, 
#'                   method = "euclidean") %>% 
#'    build_mosaic()
#'  }
#'  
#'  #Change the default size of bricks to use.
#'  # In this extreme example, use 4x6s for the background, 1x1 everywhere else
#'  \donttest{
#'  demo_image %>% 
#'   image_to_mosaic(img_size = 24, 
#'                   use_bricks = c("4x6")) %>% 
#'    build_mosaic()
#'  }
#'  
#'
image_to_mosaic <- function(img, img_size = 48, color_table = NULL,
                            method = "cie94", 
                            color_palette = c("universal", "generic", "special"), 
                            dithering = FALSE, contrast = 1, 
                            use_bricks = NULL, 
                            brightness = 1, warhol = 1:3){
  
  in_list <- img %>% 
    image_to_scaled(img_size = img_size, brightness = brightness, warhol = warhol) %>% 
    scaled_to_colors(method = method, 
                     color_table = color_table, color_palette = color_palette, 
                     dithering = dithering, contrast = contrast, default_piece_type = "p") %>% 
    collect_bricks(use_bricks = use_bricks)
  
  return(in_list)
}