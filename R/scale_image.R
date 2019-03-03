#' Scale an image raster array to a small number of pixels. Process into a data frame.
#'
#' @param image A raster array from an image.
#' @param img_size Size of output image in pixel, where one pixel = one 'brick'. Use a single value (e.g. \code{48}) for a square image with 48 pixels on each side. 
#' Use an array of two values for a rectangular image \code{c(width, height)}.
#' @param brightness A value >1 will increase the brightness of the image while a positive value <1 will decrease the brightness.
#' @param warhol Array of values \code{c(1, 2, 3)} associated with R, G, B color channels. Swap values in array to swap color channels for a fun visual effect.
#' @return A list with element \code{Img_scaled} containing a data frame of the x- & y-coordinates, R, G, B channels, and hex color of each brick (pixel).
#' @export 
#' 
scale_image <- function(image, img_size, brightness = 1, warhol = 1:3){
  
  #Adjust brightness. Max channel value is 1
  if(brightness < 0 ){stop("brightness should be a positive value. Use 1 for no change, >1 for lighter, <1 for darker.")}
  image_b <- image*brightness
  image_b[image_b>1] <- 1
  
  #Only whole values for image size
  img_size <- round(img_size, 0)
  
  #RGB channel order as specified with the `warhol` input
  col_chan <- order(warhol[1:3])
  
  #Convert image to a data frame with RGB values
  img <- dplyr::bind_rows(
    list(
      (as.data.frame(image_b[, , col_chan[1]]) %>% 
         dplyr::mutate(y=dplyr::row_number(), channel = "R")),
      (as.data.frame(image_b[, , col_chan[2]]) %>% 
         dplyr::mutate(y=dplyr::row_number(), channel = "G")),
      (as.data.frame(image_b[, , col_chan[3]]) %>% 
         dplyr::mutate(y=dplyr::row_number(), channel = "B"))
    )
  ) %>% 
    tidyr::gather(x, value, -y, -channel) %>% 
    dplyr::mutate(x = as.numeric(gsub("V", "", x))) %>% 
    tidyr::spread(channel, value)
  
  #Wide or tall image? Shortest side should be `img_size` pixels
  if(max(img$x) > max(img$y)){
    img_scale_x <-  max(img$x) / max(img$y)
    img_scale_y <- 1
  } else {
    img_scale_x <- 1
    img_scale_y <-  max(img$y) / max(img$x)
  }
  
  #If only 1 img_size value, create a square image
  if(length(img_size) == 1){
    img_size2 <- c(img_size, img_size)
  } else {
    img_size2 <- img_size[1:2]
    img_scale_x <- 1
    img_scale_y <- 1
  }
  
  #Rescale the image
  img2 <- img %>% 
    dplyr::mutate(y_scaled = (y - min(y))/(max(y)-min(y))*img_size2[2]*img_scale_y + 1,
                  x_scaled = (x - min(x))/(max(x)-min(x))*img_size2[1]*img_scale_x + 1) %>% 
    dplyr::select(-x, -y) %>% 
    dplyr::group_by(y = ceiling(y_scaled), x = ceiling(x_scaled)) %>% 
    #Get average R, G, B and convert it to hexcolor
    dplyr::summarize_at(dplyr::vars(R, G, B), dplyr::funs(mean(.))) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(color = rgb(R, G, B)) %>% 
    dplyr::ungroup() %>% 
    #Center the image
    dplyr::filter(x <= median(x) + img_size2[1]/2, x > median(x) - img_size2[1]/2,
                  y <= median(y) + img_size2[2]/2, y > median(y) - img_size2[2]/2) %>%
    #Flip y
    dplyr::mutate(y = (max(y) - y) + 1)
  
  out_list <- list()
  out_list[["Img_scaled"]] <- img2
  
  return(out_list)
  
}