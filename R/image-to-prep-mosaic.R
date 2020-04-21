#' Scale an image raster array to a small number of pixels
#' 
#' Decrease the size of an image, in pixel. Process into a data frame. Internal function.
#'
#' @param image A raster array from an image.
#' @param img_size Size of output image in pixel, where one pixel = one 'brick'. Use a single value (e.g. \code{48}) for a square image with 48 pixels on each side. 
#' Use an array of two values for a rectangular image \code{c(width, height)}.
#' @param brightness A value >1 will increase the brightness of the image while a positive value <1 will decrease the brightness.
#' @param warhol Array of values \code{c(1, 2, 3)} associated with R, G, B color channels. Swap values in array to swap color channels for a fun visual effect.
#' @format NULL
#' @usage NULL
#' @return A list with element \code{Img_scaled} containing a data frame of the x- & y-coordinates, R, G, B channels, and hex color of each brick (pixel).
#' @keywords internal

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
  
  # If png, drop the transparent bricks
  if(dim(image_b)[3] == 4){
    transparent <- as.data.frame(image_b[, , 4]) %>%
      dplyr::mutate(y=dplyr::row_number(), channel = "bg_transparent") %>%
      tidyr::gather(x, value, -y, -channel) %>%
      dplyr::mutate(x = as.numeric(gsub("V", "", x))) %>%
      tidyr::spread(channel, value) %>%
      dplyr::filter(bg_transparent < 1) %>% 
      dplyr::mutate(bg_transparent = TRUE)

    img <- img %>%
      dplyr::left_join(transparent, by = c("y", "x")) %>% 
      tidyr::replace_na(list(bg_transparent = FALSE))
  }
  
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
    dplyr::summarize_at(dplyr::vars(R, G, B, bg_transparent), mean) %>% 
    dplyr::mutate(bg_transparent = as.logical(round(bg_transparent))) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(color = rgb(R, G, B)) %>% 
    dplyr::ungroup() %>% 
    #Center the image
    dplyr::filter(x <= stats::median(x) + img_size2[1]/2, x > stats::median(x) - img_size2[1]/2,
                  y <= stats::median(y) + img_size2[2]/2, y > stats::median(y) - img_size2[2]/2) %>%
    #Flip y
    dplyr::mutate(y = (max(y) - y) + 1)
  
  out_list <- list()
  out_list[["Img_scaled"]] <- img2
  out_list[["dims"]] <- img_size2
  
  return(out_list)
  
}

#' Convert image output from scale_image() to bricks
#' 
#' Match raw color channel values to a smaller subset of colors.
#'
#' @param image_list List output from scale_image(). Contains an element  \code{Img_scaled}.
#' @param method Default 'cie94'. The method to use for comparison. Either 'brickr_classic', 'euclidean', 'cie1976', 'cie94', 'cie2000', or 'cmc'. 
#' 'brickr_classic' is an explicit euclidean distance formula, but yield different results than 'euclidean' in {farver}. 
#' See \code{farver::compare_colour}.
#' @param color_table Defaults to \code{lego_colors}. Data frame of brick colors to map onto image. Must contain Name and R, G, B channels. 
#' See attached data  \code{lego_colors} as examples.
#' @param color_palette Brick color rarity to use. Defaults to all colors: 'universal' (most common), 'generic', and 'special' (least common). 
#' This is useful when trying to build the mosaic out of real bricks.
#' Use "bw" for only grayscale bricks. Ignored if a \code{color_table} is supplied.
#' @param trans_bg If \code{img} is a png has a transparent background, name of color to replace the background. 
#' @param dithering Improves color of large, photo-realistic mosaics. 
#' @param contrast For \code{color_palette = "bw"}. A value >1 will increase the contrast of the image while a positive value <1 will decrease the contrast.
#' @param default_piece_type Piece type to use in absence of piece_type column.
#' @format NULL
#' @usage NULL
#' @keywords internal
#' @return A list with element \code{Img_lego} containing a data frame of the x- & y-coordinates, R, G, B channels, and mapped color of each brick (pixel).
scaled_to_colors <- function(image_list, method = "cie94", 
                             color_table = NULL,
                             color_palette = c("universal", "generic", "special"), 
                             trans_bg = "White",
                             dithering = FALSE,
                             contrast = 1, default_piece_type = "b"){
  in_list <- image_list
  
  #Brick colors to use ----
  if(is.null(color_table)) {
    brick_table <- brickr::lego_colors %>% 
      #No transparent colors in mosaics
      dplyr::filter(!Trans_lego)
  } else{
    # if(any(stringr::str_detect(color_table$Color, stringr::fixed("Tr. ")))){
    #   warning("Transparent bricks cannot be used in mosaics. Removing from color_table.")
    # }
    
    brick_table <- color_table %>% 
      #No transparent colors in mosaics
      dplyr::filter(!stringr::str_detect(Color, stringr::fixed("Tr. ")))
  }
  
  #Set up color palette... used standard way or with Dithering
  if(any(c("universal", "generic", "special") %in% color_palette)){
    brick_table <- brick_table %>% 
      #No transparent colors in mosaics
      dplyr::filter(!Trans_lego) %>% 
      dplyr::filter(tolower(Palette) %in% color_palette)
  } else {
    #Black and white is simpler... cut the colors into 4 groups, then assign lightest = white, darkest = black
    brick_table <- brickr::lego_colors  %>% 
      dplyr::filter(Color %in% c("White", "Black", "Medium stone grey", "Dark stone grey")) %>% 
      dplyr::arrange((R_lego + G_lego + B_lego)) %>% 
      dplyr::mutate(Lego_color = grDevices::rgb(R_lego, G_lego, B_lego))
  }
  
  #Standard or dithering
  if(!dithering){
    img <- convert_color_to_brick_standard(in_list$Img_scaled, color_table, brick_table, 
                                           color_palette, method, contrast)
  } else {
    img <- convert_color_to_brick_dithering(in_list$Img_scaled, color_table, brick_table, 
                                            color_palette, method)
  }
  
  #Replace the transparent background with a LEGO color
    if(!(trans_bg %in% lego_colors$Color)){
      stop("trans_bg must be an official color. See 'build_colors()'")
    }
    
    bg_color <- brickr::lego_colors %>% 
      dplyr::filter(Color == trans_bg)
    
    img <- img %>% 
      dplyr::mutate(color = ifelse(bg_transparent, bg_color$hex[1], color),
                    Lego_name = ifelse(bg_transparent, bg_color$Color[1], Lego_name),
                    Lego_color = ifelse(bg_transparent, bg_color$hex[1], Lego_color)) %>% 
      dplyr::select(-bg_transparent)
  
  #Return output....
  in_list[["Img_lego"]] <- img %>% 
    dplyr::mutate(Level = 1, piece_type = default_piece_type)
  
  in_list[["brickr_object"]] <- "mosaic"
  
  return(in_list)
  
}

convert_color_to_brick_standard <- function(img_object, color_table, brick_table, color_palette, method, contrast){
  #Standard bricks ----
  # Two condition... not-supplied color_table & standard palette - or - a supplied color_table
  if((is.null(color_table) & any(c("universal", "generic", "special") %in% color_palette)) |
     !is.null(color_table)){
    
    #Speed up calc by round pixel to nearest 1/20 & only calculating unique
    mosaic_base <- img_object %>% 
      dplyr::mutate_at(dplyr::vars(R, G, B), list(~round(.*20)/20)) %>% 
      dplyr::select(R, G, B) %>% 
      dplyr::distinct()
    
    #Match colors either the old way or with Farver ----
    if(method == "brickr_classic"){
      mosaic_colors <- mosaic_base %>% 
        dplyr::mutate(lego = purrr::pmap(list(R, G, B), convert_to_match_color, brick_table)) %>% 
        tidyr::unnest(lego)
      
    } else { #Farver ----
      mosaic_colors <- mosaic_base %>% 
        dplyr::mutate(rgb = purrr::pmap(list(R, G, B), function(R, G, B){
          cc <- matrix(c(R, G, B), ncol = 3)*255
          dstncs <- farver::compare_colour(from=cc, 
                                           to=brick_table[, c('R_lego', 'G_lego', 'B_lego')]*255, 
                                           from_space='rgb', to_space = 'rgb', method=method)
          
          sel_color <- as.character(brick_table[which.min(dstncs), "Color"])[1]
          
          brick_table %>% 
            dplyr::filter(Color == sel_color) %>% 
            dplyr::mutate(Lego_color = grDevices::rgb(R_lego, G_lego, B_lego)) %>% 
            dplyr::select(Lego_name = Color, Lego_color)
        })) %>% 
        tidyr::unnest(rgb)
    }
    
    img <- img_object %>% 
      dplyr::mutate_at(dplyr::vars(R, G, B), list(~round(.*20)/20)) %>%
      dplyr::left_join(mosaic_colors, by = c("R", "G", "B"))
    
    return(img)
    
  } else if ("bw" %in% color_palette){
    
    img <- img_object %>% 
      dplyr::mutate(shade = (0.299*R+0.587*G+0.114*B)/3,
                    shade = shade ^ contrast) %>% 
      dplyr::mutate(shade_bw = as.numeric(as.factor(cut(shade, 4)))) %>% 
      dplyr::mutate(Lego_name = brick_table$Color[shade_bw],
                    Lego_color = brick_table$Lego_color[shade_bw]) %>% 
      dplyr::select(-dplyr::starts_with("shade"))
    
    return(img)
    
  } else {
    stop("Either you must supply a color_table or use an accepted color_palette: 
       a combination of ('universal', 'generic', and/or 'special') - or- 'bw'")
  }
}

convert_color_to_brick_dithering <- function(img_object, color_table, brick_table, color_palette, method){
  #Standard bricks ----
  # Two condition... not-supplied color_table & standard palette - or - a supplied color_table
  if((is.null(color_table) & any(c("universal", "generic", "special", "bw") %in% color_palette)) |
     !is.null(color_table)){
    
    #Speed up calc by round pixel to nearest 1/20 & only calculating unique
    mosaic_base <- img_object
    
    #Ignore brickr_classic for now
    if(method == "brickr_classic") {
      method <- "cie94"
    }
    
    mosaic_base$Lego_color <- NA
    mosaic_base$Lego_name  <- NA
    
    for(yy in unique(mosaic_base$y)){
      for(xx in unique(mosaic_base$x)){
        dstncs <- mosaic_base[mosaic_base$x == xx & mosaic_base$y == yy, c("R", "G", "B")] %>% 
          as.matrix(ncol = 3) %>% 
          farver::compare_colour(to=brick_table[, c('R_lego', 'G_lego', 'B_lego')], 
                                 from_space='rgb', to_space = 'rgb', method=method)
        
        #Assign LEGO color for this cell
        if(!is.character(brick_table$hex[which.min(dstncs)])){next}
        
        mosaic_base[mosaic_base$x == xx & mosaic_base$y == yy, c("Lego_name", "Lego_color")] <- 
          as.character(brick_table[which.min(dstncs),  c("Color", "hex")][1,])
        
        #Difference in color
        dith_diff <- mosaic_base[mosaic_base$x == xx & mosaic_base$y == yy,  c("R", "G", "B")] - 
          as.numeric(brick_table[which.min(dstncs),  c("R_lego", "G_lego", "B_lego")][1,])
        
        # dith_diff <- dith_diff*c(0.299, 0.587, 0.114)
        
        #Update color of surrounding pixels.. if pixel exists
        if(xx < max(mosaic_base$x)){
          xs <- 1; ys <- 0;
          mosaic_base[(mosaic_base$x == (xx+xs)) & (mosaic_base$y == (yy+ys)),  c("R", "G", "B")] <-
            mosaic_base[(mosaic_base$x == (xx+xs)) & (mosaic_base$y == (yy+ys)),  c("R", "G", "B")] + (7/16)*dith_diff
        }
        
        if(yy > min(mosaic_base$y)){
          xs <- 0; ys <- -1;
          mosaic_base[(mosaic_base$x == (xx+xs)) & (mosaic_base$y == (yy+ys)),  c("R", "G", "B")] <-
            mosaic_base[(mosaic_base$x == (xx+xs)) & (mosaic_base$y == (yy+ys)),  c("R", "G", "B")] + (5/16)*dith_diff
        }
        
        if(xx > min(mosaic_base$x) & yy > min(mosaic_base$y)){
          xs <- -1; ys <- -1;
          mosaic_base[(mosaic_base$x == (xx+xs)) & (mosaic_base$y == (yy+ys)),  c("R", "G", "B")] <-
            mosaic_base[(mosaic_base$x == (xx+xs)) & (mosaic_base$y == (yy+ys)),  c("R", "G", "B")] + (3/16)*dith_diff
        }
        
        if(xx < max(mosaic_base$x) & yy > min(mosaic_base$y)){
          xs <- 1; ys <- -1;
          mosaic_base[(mosaic_base$x == (xx+xs)) & (mosaic_base$y == (yy+ys)),  c("R", "G", "B")] <-
            mosaic_base[(mosaic_base$x == (xx+xs)) & (mosaic_base$y == (yy+ys)),  c("R", "G", "B")] + (1/16)*dith_diff
        }
        
        #Ensure RGBs [0,1]
        mosaic_base[mosaic_base$R > 1, "R"] <- 1
        mosaic_base[mosaic_base$G > 1, "G"] <- 1
        mosaic_base[mosaic_base$B > 1, "B"] <- 1
        
        mosaic_base[mosaic_base$R < 0, "R"] <- 0
        mosaic_base[mosaic_base$G < 0, "G"] <- 0
        mosaic_base[mosaic_base$B < 0, "B"] <- 0
      }
    }
    
    return(mosaic_base)
    
  } else {
    stop("Either you must supply a color_table or use an accepted color_palette: 
       a combination of ('universal', 'generic', and/or 'special') - or- 'bw'")
  }
}

convert_to_match_color <- function(R, G, B, dat_color){
  dat_color %>% 
    dplyr::mutate(dist = ((R_lego - R)^2 + (G_lego - G)^2 + (B_lego - B)^2)^(1/2)) %>% 
    dplyr::top_n(-1, dist) %>% 
    dplyr::mutate(Lego_color = grDevices::rgb(R_lego, G_lego, B_lego)) %>% 
    dplyr::select(Lego_name = Color, Lego_color)
}
