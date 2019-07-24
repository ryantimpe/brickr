#' Scale an image raster array to a small number of pixels. Process into a data frame.
#'
#' @param image A raster array from an image.
#' @param img_size Size of output image in pixel, where one pixel = one 'brick'. Use a single value (e.g. \code{48}) for a square image with 48 pixels on each side. 
#' Use an array of two values for a rectangular image \code{c(width, height)}.
#' @param brightness A value >1 will increase the brightness of the image while a positive value <1 will decrease the brightness.
#' @param warhol Array of values \code{c(1, 2, 3)} associated with R, G, B color channels. Swap values in array to swap color channels for a fun visual effect.
#' @format NULL
#' @usage NULL
#' @return A list with element \code{Img_scaled} containing a data frame of the x- & y-coordinates, R, G, B channels, and hex color of each brick (pixel).
#' @export
#' 
image_to_scaled <- function(image, img_size = 48, brightness = 1, warhol = 1:3){
  
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

#' Convert image output from scale_image() to bricks
#'
#' @param image_list List output from scale_image(). Contains an element  \code{Img_scaled}.
#' @param method The method to use for comparison. Either 'brickr_classic', 'euclidean', 'cie1976', 'cie94', 'cie2000', or 'cmc'. 
#' 'brickr_classic' is an explicit euclidean distance formula, but yield different results than 'euclidean' in {farver}. 
#' See \code{farver::compare_colour}.
#' @param color_table Defaults to \code{lego_colors}. Data frame of brick colors to map onto image. Must contain Name and R, G, B channels. 
#' See attached data  \code{lego_colors} as examples.
#' @param color_palette Brick color rarity to use. Defaults to all colors: 'universal' (most common), 'generic', and 'special' (least common). This is useful when trying to build the mosaic out of real bricks.
#' Use "bw" for only grayscale bricks. Ignored if a \code{color_table} is supplied.
#' @param contrast For \code{color_palette = "bw"}. A value >1 will increase the contrast of the image while a positive value <1 will decrease the contrast.
#' @format NULL
#' @usage NULL
#' @return A list with element \code{Img_lego} containing a data frame of the x- & y-coordinates, R, G, B channels, and mapped color of each brick (pixel).
#' @export
scaled_to_colors <- function(image_list, method = "cie94", 
                             color_table = NULL,
                             color_palette = c("universal", "generic", "special"), 
                             contrast = 1){
  in_list <- image_list
  
  #Brick colors to use ----
  if(is.null(color_table)) {
    brick_table <- lego_colors
  } else{
    brick_table <- color_table
  }
  
  #Standard bricks ----
  # Two condition... not-supplied color_table & standard palette - or - a supplied color_table
  if((is.null(color_table) & any(c("universal", "generic", "special") %in% color_palette)) |
     !is.null(color_table)){
    
    brick_table <- brick_table %>% 
      dplyr::filter(tolower(Palette) %in% color_palette)
      
    #Speed up calc by round pixel to nearest 1/20 & only calculating unique
    mosaic_base <- in_list$Img_scaled %>% 
      dplyr::mutate_at(dplyr::vars(R, G, B), list(~round(.*20)/20)) %>% 
      dplyr::select(R, G, B) %>% 
      dplyr::distinct()
    
    #I don't like the compare_color results for "euclidean"... use manual way ----
    if(method == "brickr_classic"){
      mosaic_colors <- mosaic_base %>% 
        dplyr::mutate(lego = purrr::pmap(list(R, G, B), convert_to_match_color, brick_table)) %>% 
        tidyr::unnest(lego)
      
    } else { #Farver ----
      mosaic_colors <- mosaic_base %>% 
        dplyr::mutate(rgb = purrr::pmap(list(R, G, B), function(R, G, B){
          cc <- matrix(c(R, G, B), ncol = 3)
          dstncs <- farver::compare_colour(from=cc, to=brick_table[, c('R_lego', 'G_lego', 'B_lego')], 
                                           from_space='rgb', to_space = 'rgb', method=method)
          
          sel_color <- as.character(brick_table[which.min(dstncs), "Color"])[1]
          
          brick_table %>% 
            dplyr::filter(Color == sel_color) %>% 
            dplyr::mutate(Lego_color = grDevices::rgb(R_lego, G_lego, B_lego)) %>% 
            dplyr::select(Lego_name = Color, Lego_color)
        })) %>% 
        tidyr::unnest(rgb)
      
    }
    
    img <- in_list$Img_scaled %>% 
      dplyr::mutate_at(dplyr::vars(R, G, B), list(~round(.*20)/20)) %>%
      dplyr::left_join(mosaic_colors, by = c("R", "G", "B"))
    
  } else if ("bw" %in% color_palette){
    #Black and white is simpler... cut the colors into 4 groups, then assign lightest = white, darkest = black
    bw_colors <- lego_colors  %>% 
      dplyr::filter(Color %in% c("White", "Black", "Medium stone grey", "Dark stone grey")) %>% 
      dplyr::arrange((R_lego + G_lego + B_lego)) %>% 
      dplyr::mutate(Lego_color = grDevices::rgb(R_lego, G_lego, B_lego))
    
    img <- in_list$Img_scaled %>% 
      dplyr::mutate(shade = (0.299*R+0.587*G+0.114*B)/3,
                    shade = shade ^ contrast) %>% 
      dplyr::mutate(shade_bw = as.numeric(as.factor(cut(shade, 4)))) %>% 
      dplyr::mutate(Lego_name = bw_colors$Color[shade_bw],
                    Lego_color = bw_colors$Lego_color[shade_bw]) %>% 
      dplyr::select(-dplyr::starts_with("shade"))
    
  } else {
    stop("Either you must supply a color_table or use an accepted color_palette: 
       a combination of ('universal', 'generic', and/or 'special') - or- 'bw'")
  }
  
  in_list[["Img_lego"]] <- img %>% 
    dplyr::mutate(Level = 1)
  
  in_list[["brickr_object"]] <- "mosaic"
  
  return(in_list)
  
}

convert_to_match_color <- function(R, G, B, dat_color){
  dat_color %>% 
    dplyr::mutate(dist = ((R_lego - R)^2 + (G_lego - G)^2 + (B_lego - B)^2)^(1/2)) %>% 
    dplyr::top_n(-1, dist) %>% 
    dplyr::mutate(Lego_color = grDevices::rgb(R_lego, G_lego, B_lego)) %>% 
    dplyr::select(Lego_name = Color, Lego_color)
}