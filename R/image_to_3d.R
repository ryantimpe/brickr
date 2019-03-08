#' Convert image output from scale_image() to bricks
#'
#' @param image_list List output from collect_bricks() or image_to_bricks(). Contains an element \code{Img_lego}.
#' @param mosaic_height Maximum height of 3D mosiacs in LEGO plates. 3 plates = 1 brick. This is also the maximum # of distinct elevations.
#' @param highest_el Brick height is determined by brightness of color. Use \code{highest_el = 'dark'} for darkest bricks to have \code{mosaic_height}.
#' @return A list with elements \code{threed_elevation} and \code{threed_hillshade} to created 3D mosiacs with the \code{rayshader} package.
#' @export 
#'
collect_3d <- function(image_list, mosaic_height = 6, highest_el = "light"){

  #Get previous data
  in_list <- image_list
  
  if(in_list$mosaic_type != "flat")stop("3D mosaics can only be generated with 'flat' mosaics. Set this input in the 'collect_bricks' function.")
  
  BrickIDs <- in_list$ID_bricks
  img_lego <- in_list$Img_lego
  
  #Number of 'pixels' on a side of a single-stud brick. I think this should be fixed for now
  ex_size <- 15
  
  lego_expand <- img_lego %>%
    dplyr::select(x, y, Lego_name, Lego_color) %>% 
    dplyr::mutate(stud_id = dplyr::row_number()) 
  
  lego_expand2 <- expand.grid(x = (min(lego_expand$x)*ex_size):(max(lego_expand$x+1)*ex_size),
                              y = (min(lego_expand$y)*ex_size):(max(lego_expand$y+1)*ex_size)) %>% 
    dplyr::mutate(x_comp = x %/% ex_size,
                  y_comp = y %/% ex_size) %>% 
    dplyr::left_join(lego_expand %>% dplyr::rename(x_comp = x, y_comp = y), 
                     by = c("x_comp", "y_comp")) %>% 
    dplyr::left_join(BrickIDs %>% dplyr::select(brick_id, x_comp = x, y_comp = y), 
                     by = c("x_comp", "y_comp")) %>% 
    dplyr::select(-x_comp, -y_comp) %>% 
    dplyr::left_join(lego_colors %>% dplyr::select(Lego_name = Color, R_lego, G_lego, B_lego), 
                    by = "Lego_name") %>% 
    dplyr::do(
      if(highest_el == "dark"){
        dplyr::mutate(., elevation = (1-((R_lego + G_lego + B_lego )/3)) * 1000)
      } else {
        dplyr::mutate(., elevation = (  ((R_lego + G_lego + B_lego )/3)) * 1000)
      }
    ) %>% 
    #Round elevation to nearest 1/height
    dplyr::mutate(elevation = as.numeric(as.factor(cut(elevation, mosaic_height)))) %>% 
    dplyr::mutate(y = max(y)-y) %>% 
    dplyr::filter(!is.na(elevation)) %>% 
    #Calculate stud placement... radius of 1/3 and height of 0.5 plate
    dplyr::group_by(stud_id) %>% 
    dplyr::mutate(x_mid = median(x), y_mid = median(y),
                  stud = ((x-x_mid)^2 + (y-y_mid)^2)^(1/2) < ex_size/3) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(elevation = ifelse(stud, elevation+0.5, elevation)) %>% 
    dplyr::mutate_at(dplyr::vars(R_lego, G_lego, B_lego), dplyr::funs(ifelse(stud, .-0.1, .))) %>% 
    dplyr::mutate_at(dplyr::vars(R_lego, G_lego, B_lego), dplyr::funs(ifelse(. < 0, 0, .)))
  
  edges <- dplyr::bind_rows(list(
    lego_expand2 %>% dplyr::filter(x == min(x)) %>% dplyr::mutate(x = x-1),
    lego_expand2 %>% dplyr::filter(x == max(x)) %>% dplyr::mutate(x = x+1),
    lego_expand2 %>% dplyr::filter(y == min(y)) %>% dplyr::mutate(y = y-1),
    lego_expand2 %>% dplyr::filter(y == max(y)) %>% dplyr::mutate(y = y+1)
  )) %>% 
    dplyr:: mutate(R_lego = 1, G_lego = 1, B_lego = 1, 
                   elevation = 0,
                   brick_id = NA)
  
  #Elevation Matrix
  lego_elmat <- lego_expand2 %>% 
    dplyr::bind_rows(edges) %>% 
    dplyr::select(x, y, elevation) %>% 
    tidyr::spread(y, elevation) %>% 
    dplyr::select(-x) %>% 
    as.matrix()
  
  #Hillshade matrix
  lego_hillshade_m <- array(dim = c(length(unique(lego_expand2$y)), 
                                    length(unique(lego_expand2$x)), 
                                    3))
  
  lego_expand_color <- lego_expand2 %>% 
    dplyr::group_by(brick_id) %>% 
    #This darkens the edge of each brick, to look like they are separated
    dplyr::mutate_at(dplyr::vars(R_lego, G_lego, B_lego), 
                     dplyr::funs(ifelse((x == min(x) | y == min(y) | x == max(x) | y == max(y)), .*0.75, .))) %>% 
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
  
  return(in_list)
  
}

#' brickr wrapper for rayshader::plot_3d() to display 3D mosaics. Requires rayshader.
#'
#' @param image_list List output from collect_3d(). Contains element \code{threed_elevation} and \code{threed_hillshade}.
#' @param solidcolor Hex color of mosaic base. Only renders on bottom.
#' @param ... All other inputs from rayshader::plot_3d() EXCEPT \code{hillshade}, \code{soliddepth}, and \code{zscale}.
#' @return 3D mosaic rendered in the 'rgl' package.
#' @export 

display_3d <- function(image_list, solidcolor = "#a3a2a4", ...){
  #Requires Rayshader
  if (!requireNamespace("rayshader", quietly = TRUE)) {
    stop("Package \"rayshader\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  image_list$`threed_hillshade`%>%
    rayshader::plot_3d(image_list$`threed_elevation`, zscale=0.125, 
                       solidcolor=solidcolor, ...)
}
