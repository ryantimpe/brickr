#' Helper function to convert a level from a 3D model into a rayshader-friendly object.
#'
#' @param brick_list List output from table_to_bricks(). Contains an element \code{Img_lego}.
#' @param brick_type Type of brick to use. Default is 'brick'. Other option is 'plate', which is 1/3 the height of a brick.
#' @param lev z-level of 3D model
#' @param brick_res Resolution, expressed at number of pixels on one side of a 1x1 brick. Defaults to 'sd' (15px). Use 'hd' for 30px per brick, and 'uhd' for 60px. 
#' Enter a value for a custom resolution. High resolutions take longer to render.
#' @return A list with elements \code{threed_elevation} and \code{threed_hillshade} to created 3D mosiacs with the \code{rayshader} package.

layer_from_bricks <- function(brick_list, brick_type = "brick", lev=1, brick_res = "sd"){
  #Get previous data
  in_list <- brick_list
  
  BrickIDs <- in_list$ID_bricks%>% 
    dplyr::filter(Level == lev) 
  
  img_lego <- in_list$Img_lego %>% 
    dplyr::filter(Level == lev)
  
  if(brick_type == 'plate'){
    brick_depth = 1L
  } else {
    brick_depth = 3L
  }
  
  #Increment elevation - a brick is 3 plates tall
  up_el = (lev-1)*brick_depth
  
  #Number of 'pixels' on a side of a single-stud brick. Set by brick_res.
  if(is.numeric(brick_res)){
    if(brick_res > 100) warning("brick_res capped at 100px per brick.")
    ex_size <- min(100, abs(round(brick_res)))
  } else {
    if(!(brick_res %in% c('sd', 'hd', 'uhd'))) stop("brick_res must be 'sd', 'hd', 'uhd', or a number.")
    ex_size <- switch(brick_res,
                      sd = 15,
                      hd = 30,
                      uhd = 60)
  }

  
  #Use below is edge calculation
  # Optimized color only in HD bricks >20 pixels
  if(ex_size >= 20){
    edge_offset <- 0:1
  } else {
    edge_offset <- 0
  }
  
  #Increase data frame into the correct resolution
  lego_expand <- img_lego %>%
    dplyr::select(Level, x, y, Lego_name, Lego_color) %>% 
    dplyr::mutate(stud_id = dplyr::row_number()) 
  
  lego_expand2 <- expand.grid(x = (min(lego_expand$x)*ex_size):(max(lego_expand$x+1)*ex_size),
                              y = (min(lego_expand$y)*ex_size):(max(lego_expand$y+1)*ex_size)) %>% 
    dplyr::mutate(x_comp = x %/% ex_size,
                  y_comp = y %/% ex_size) %>% 
    dplyr::left_join(lego_expand %>% dplyr::rename(x_comp = x, y_comp = y), 
                     by = c("x_comp", "y_comp")) %>% 
    dplyr::left_join(BrickIDs %>% dplyr::select(brick_name, x_comp = x, y_comp = y), 
                     by = c("x_comp", "y_comp")) %>% 
    dplyr::select(-x_comp, -y_comp) %>% 
    dplyr::left_join(lego_colors %>% dplyr::select(Lego_name = Color, R_lego, G_lego, B_lego), 
                     by = "Lego_name") %>% 
    #Round elevation to nearest 1/height
    dplyr::mutate(elevation = ifelse(is.na(brick_name),NA, brick_depth + up_el),
                  elevation = ifelse(is.na(Lego_name),NA, elevation)) %>% 
    #Create the edges of bricks... Brick base begins at 0.01 to avoid complete overlap with previous brick
    dplyr::group_by(brick_name) %>% 
    dplyr::mutate(elevation = dplyr::case_when(
      x %in% (min(x) + edge_offset) ~ 0.01+up_el,
      x %in% (max(x) - edge_offset) ~ 0.01+up_el,
      y %in% (min(y) + edge_offset) ~ 0.01+up_el,
      y %in% (max(y) - edge_offset) ~ 0.01+up_el,
      TRUE ~ elevation
    )) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(y = max(y)-y) %>% 
    #Calculate stud placement... radius of 5/8 * (1/2) and height of 0.5 plate
    dplyr::group_by(stud_id) %>% 
    dplyr::mutate(x_mid = median(x), y_mid = median(y),
                  stud = ((x-x_mid)^2 + (y-y_mid)^2)^(1/2) <= (ex_size * (5/8 * (1/2))),
                  stud_color = dplyr::between(((x-x_mid)^2 + (y-y_mid)^2)^(1/2),
                  (ex_size * (5/8 * (1/2))) - 1,
                  (ex_size * (5/8 * (1/2)))
                  )) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(elevation = ifelse(stud, elevation+0.5, elevation)) %>% 
    dplyr::mutate_at(dplyr::vars(R_lego, G_lego, B_lego), list(~ifelse(stud_color, .-0.1, .))) %>% 
    dplyr::mutate_at(dplyr::vars(R_lego, G_lego, B_lego), list(~ifelse(. < 0, 0, .)))
  
  #Elevation Matrix
  lego_elmat <- lego_expand2 %>% 
    dplyr::mutate(elevation = ifelse(is.na(Lego_name), NA, elevation)) %>% 
    dplyr::select(x, y, elevation) %>% 
    tidyr::spread(y, elevation) %>% 
    dplyr::select(-x) %>% 
    as.matrix()
  
  #Hillshade matrix
  lego_hillshade_m <- array(dim = c(length(unique(lego_expand2$y)), 
                                    length(unique(lego_expand2$x)), 
                                    3))
  
  lego_expand_color <- lego_expand2 %>% 
    dplyr::group_by(brick_name) %>% 
    #This darkens the edge of each brick, to look like they are separated
    # The higher the resolution, the dark this should be
    dplyr::mutate_at(dplyr::vars(R_lego, G_lego, B_lego), 
                     list(~ifelse((x == min(x) | y == min(y) | x == max(x) | y == max(y)), 
                                  . - 0.1, .))) %>% 
    dplyr::mutate_at(dplyr::vars(R_lego, G_lego, B_lego), 
                     list(~ifelse(. < 0, 0, .))) %>% 
    dplyr::ungroup()
  
  lego_hillshade_m[,,1] <- lego_expand_color %>% 
    dplyr::select(x, y, R_lego) %>% 
    tidyr::spread(x, R_lego) %>% 
    dplyr::select(-y) %>% 
    as.matrix()
  
  lego_hillshade_m[,,2] <- lego_expand_color %>% 
    dplyr::select(x, y, G_lego) %>% 
    tidyr::spread(x, G_lego) %>% 
    dplyr::select(-y) %>% 
    as.matrix()
  
  lego_hillshade_m[,,3] <- lego_expand_color %>% 
    dplyr::select(x, y, B_lego) %>% 
    tidyr::spread(x, B_lego) %>% 
    dplyr::select(-y) %>% 
    as.matrix()
  
  #Return
  in_list[["threed_elevation"]] <- lego_elmat
  in_list[["threed_hillshade"]] <- lego_hillshade_m
  in_list[["brick_resolution"]] <- ex_size
  
  return(in_list)
  
}


#' Build 3D brick model with rayshader.
#'
#' @param brick_list List output from collect_bricks(). Contains an element \code{Img_lego}.
#' @param brick_type Type of brick to use. Default is 'brick'. Other option is 'plate', which is 1/3 the height of a brick.
#' @param view_levels Numeric array of Levels/z values to display. Leave as \code{NULL} to include all.
#' @param brick_res Resolution, expressed at number of pixels on one side of a 1x1 brick. Defaults to 'sd' (15px). Use 'hd' for 30px per brick, and 'uhd' for 60px. 
#' Enter a value for a custom resolution. High resolutions take longer to render.
#' @param solidcolor Hex color of mosaic base. Only renders on bottom.
#' @param ... All other inputs from rayshader::plot_3d() EXCEPT \code{hillshade}, \code{soliddepth}, \code{zscale}, and \code{shadow}.
#' @return 3D brick model rendered in the 'rgl' package.
#' @export 
#'
build_bricks <- function(brick_list, brick_type = "brick", brick_res = "sd",
                           view_levels = NULL, solidcolor = "#a3a2a4", ...){
  #Requires Rayshader
  if (!requireNamespace("rayshader", quietly = TRUE)) {
    stop("Package \"rayshader\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  #Get previous data
  in_list <- brick_list
  
  BrickIDs <- in_list$ID_bricks
  img_lego <- in_list$Img_lego 
  
  if(is.null(view_levels)){
    view_levels <- unique(img_lego$Level)
  }

  for(ii in view_levels){
    brick_layer <- brick_list %>% layer_from_bricks(ii, brick_type = brick_type, brick_res = brick_res)
    
    brick_layer$`threed_hillshade`%>%
      rayshader::plot_3d(brick_layer$`threed_elevation`, zscale=0.167*(15/brick_layer$`brick_resolution`), 
                         solid = FALSE,
                         solidcolor=solidcolor, shadow = FALSE, ...)
  }

}