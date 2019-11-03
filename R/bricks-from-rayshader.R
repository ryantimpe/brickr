#' Build a brickr 3D object from rayshader hillshade & heightmap matrices
#'
#' @param hillshade Same as \code{rayshader::plot_3d()}. Hillshade/image to be added to 3D surface map.
#' @param heightmap Same as \code{rayshader::plot_3d()}. A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#' @param img_size Size of output image in pixel, where one pixel = one 'brick'. Use a single value (e.g. \code{48}) for a square image with 48 pixels on each side. 
#' Use an array of two values for a rectangular image \code{c(width, height)}.
#' @param max_height Maximum height of plot in LEGO bricks or plates. 
#' @return A list with elements \code{threed_elevation} and \code{threed_hillshade} to created 3D mosiacs with the \code{rayshader} package.
#' @family 3D Models
#' @export 
#'
bricks_from_rayshader <- function(hillshade, heightmap, max_height = 12, img_size = 48){

  #Convert RGB matrix into a mosaic. Nothing special here.
  #Users can do the same to make 2D rayshader map
  in_list <- hillshade %>% 
    image_to_mosaic(img_size)
  
  BrickIDs <- in_list$ID_bricks
  img_lego <- in_list$Img_lego
  
  #Bad hack to scale heightmap the same way
  hghtmp_list <- (array(c(heightmap, heightmap, heightmap), dim = c(nrow(heightmap), ncol(heightmap), 3)) / max(heightmap)) %>% 
    image_to_scaled(img_size)
  
  hghtmp <- hghtmp_list[[1]] %>% 
    dplyr::select(x, y, height = R) %>% 
    dplyr::mutate(Level = round(height * (max_height))+1) %>% 
    dplyr::select(-height) %>% 
    dplyr::mutate(ww = max(x) - x, 
                  x = max(y) - y, 
                  y = ww) %>% 
    dplyr::select(-ww)

  #New levels for the image
  img_sorted_by_lum <- in_list$Img_lego %>% 
    dplyr::select(-Level) %>% 
    dplyr::left_join(hghtmp, by = c("x", "y")) %>% 
    dplyr::group_by(y) %>% 
    tidyr::fill(Level) %>% 
    tidyr::fill(Level, .direction = "up") %>% 
    dplyr::ungroup() %>% dplyr::group_by(x) %>%
    tidyr::fill(Level) %>% 
    tidyr::fill(Level, .direction = "up") %>% 
    dplyr::ungroup()
  
  #For each Level, create a full based mosaic Level
  img_all_levels <- 1:max_height %>% 
    purrr::map_df(function(lvl){
      dat <- img_sorted_by_lum %>% 
        #Only get colors at or above the current level
        dplyr::filter(Level >= lvl & Level <= (lvl + 2)) %>% 
        #Replace any higher levels with colors in this level
        dplyr::mutate(Lego_name = ifelse(Level > lvl, NA_character_, Lego_name),
                      Lego_color = ifelse(is.na(Lego_name), NA_character_, Lego_color),
                      Level = lvl)
      
      most_common_color <- dat %>% 
        dplyr::filter(!is.na(Lego_name)) %>% 
        dplyr::count(Lego_name, Lego_color, sort = TRUE)
      
      dat %>% 
        dplyr::mutate(Lego_name = ifelse(is.na(Lego_name), as.character(most_common_color[1, "Lego_name"]), Lego_name),
                      Lego_color = ifelse(is.na(Lego_color), as.character(most_common_color[1, "Lego_color"]), Lego_color)) %>% 
        dplyr::select(-color) %>% 
        dplyr::rename(color = Lego_name, z = Level)
    })
  
  return(img_all_levels %>% bricks_from_coords() )
  
}

