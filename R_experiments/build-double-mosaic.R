#' Build 3D brick model with rgl
#'
#' @param brick_list List output from collect_bricks(). Contains an element \code{Img_lego}.
#' @param brick_type Type of brick to use. Default is 'brick'. Other option is 'plate', which is 1/3 the height of a brick.
#' @param background_color Default 'white'. Color of the background.
#' @param rgl_lit Default 'TRUE'. Include RGL lighting features in rendering.
#' @param outline_bricks Default 'FALSE'. Include black outlines around brick edges. 
#' Set to 'TRUE' and rgl_lit='FALSE' for cartoon-looking bricks.
#' @param trans_alpha Default 0.5. Alpha level for transparent bricks.
#' @param view_levels Numeric array of Levels/z values to display. Leave as 'NULL' to include all.
#' @examples \dontrun{
#' #This is a brick
#'brick <- data.frame(
#'  Level="A",
#'  X1 = rep(3,4), #The number 3 is the brickrID for 'bright red'
#'  X2 = rep(3,4)
#')
#'
#'brick %>% 
#'  bricks_from_table() %>% 
#'  build_bricks()
#' }
#' @return 3D brick model rendered in the 'rgl' package.
#' @family 3D Models
#' @export 
#'
build_double_mosaic <- function(brick_list, brick_list2,
                         background_color = "white", rgl_lit = TRUE,
                         outline_bricks = FALSE,
                         trans_alpha = 0.5,
                         view_levels = NULL){
  #Mosaic 1 ----
  in_list <- brick_list

  img_bricks <- in_list$Img_bricks%>% 
    tidyr::drop_na()%>% 
    dplyr::left_join(lego_colors %>% 
                       dplyr::select(Lego_name = Color, Trans_lego, lum),
                     by = c("Lego_name"))
  
  if(is.null(view_levels)){
    view_levels <- unique(img_bricks$Level)
  }
  
  #Mosaic 2 ----
  in_list2 <- brick_list2
  
  img_bricks2 <- in_list2$Img_bricks%>% 
    tidyr::drop_na()%>% 
    dplyr::left_join(lego_colors %>% 
                       dplyr::select(Lego_name = Color, Trans_lego, lum),
                     by = c("Lego_name"))
  
  if(is.null(view_levels)){
    view_levels2 <- unique(img_bricks2$Level)
  }
  
  #SET PARAMETERS ----
  # For use inside brick drawing functions below
  nudge = 0.01 #Space between bricks
  scale = 1 #Reduce to unit size
  # trans_alpha = 0.5 #Alpha of transparent bricks
  # height_scale = 9.6/7.8 * (2/3)
  
  height_scale = 1
  
  color_outline = "black"
  color_outline_trans = "white"
  
  contrast_knobs = TRUE
  contrast_lum = 0.2
  
  knob_diameter = 5/8
  
  outline_bricks = outline_bricks
  
  suppress_knobs = TRUE
  
  #For now, use the current collect_bricks output. 
  #This was designed for rayshader, and I don't want to drop rayshader just yet.
  
  # MOSAIC 1 ----
  
  max_x = max(img_bricks$xmax) - 0.5
  
  rgl_bricks_base <- list(
    # x & y are the CENTERS of bricks. rgl scales shapes from center
    x = (img_bricks$xmin + 0.5 + (img_bricks$xmax - img_bricks$xmin)/2) * 2,
    z = img_bricks$ymin + 0.5 + (img_bricks$ymax - img_bricks$ymin)/2 ,
    y = img_bricks$Level,
    color = img_bricks$Lego_color,
    trans = img_bricks$Trans_lego,
    lum = img_bricks$lum,
    #Grab brick size from brick type id
    width = as.numeric(substr(img_bricks$brick_type, 2, 2)),
    length = as.numeric(substr(img_bricks$brick_type, 4, 4))
  ) %>% 
    purrr::transpose()
  
  rgl_bricks_base_list <- rgl_bricks_base %>% 
    purrr::map(function(this_brick){
      
      #Solid brick ----
      brk_fill <- rgl::cube3d(col = this_brick$color,
                              alpha = if(this_brick$trans){trans_alpha}else{1})
      
      # brk_fill$vb[, c(5, 7)] <- brk_fill$vb[, c(1, 3)] * 2/3
      brk_fill$vb[, c(1, 5)] <- brk_fill$vb[, c(3, 7)] * 2/3
      
      brk_fill$vb[4,] <- brk_fill$vb[4,]/scale*2 + nudge
      
      brk_fill2 <- brk_fill %>% 
        rgl::scale3d(this_brick$width, this_brick$length, height_scale) %>% #Increase height
        rgl::translate3d(this_brick$x, this_brick$y, this_brick$z * height_scale) 
      
      if(outline_bricks){
      } else {
        brk_out2 <- NULL
        out_list <- list(brk_fill2, brk_out2)
      }
      
      #Save ----
      return(out_list)
      
    }) %>% 
    purrr::transpose()
  
  # MOSAIC 2 ----
  
  max_x = max(img_bricks2$xmax) - 0.5
  
  rgl_bricks_base2 <- list(
    # x & y are the CENTERS of bricks. rgl scales shapes from center
    x = (img_bricks2$xmin + 0.5 + (img_bricks2$xmax - img_bricks2$xmin)/2) * 2 -7,
    z = img_bricks2$ymin + 0.5 + (img_bricks$ymax - img_bricks2$ymin)/2 ,
    y = img_bricks2$Level,
    color = img_bricks2$Lego_color,
    trans = img_bricks2$Trans_lego,
    lum = img_bricks2$lum,
    #Grab brick size from brick type id
    width = as.numeric(substr(img_bricks2$brick_type, 2, 2)),
    length = as.numeric(substr(img_bricks2$brick_type, 4, 4))
  ) %>% 
    purrr::transpose()
  
  rgl_bricks_base_list2 <- rgl_bricks_base2 %>% 
    purrr::map(function(this_brick){
      
      #Solid brick ----
      brk_fill <- rgl::cube3d(col = this_brick$color,
                              alpha = if(this_brick$trans){trans_alpha}else{1})
      
      # brk_fill$vb[, c(6, 8)] <- brk_fill$vb[, c(2, 4)] * 2/3
      brk_fill$vb[, c(2, 6)] <- brk_fill$vb[, c(4, 8)] * 2/3
      
      brk_fill$vb[4,] <- brk_fill$vb[4,]/scale*2 + nudge
      
      brk_fill2 <- brk_fill %>% 
        rgl::scale3d(this_brick$width, this_brick$length, height_scale) %>% #Increase height
        rgl::translate3d(this_brick$x, this_brick$y, this_brick$z * height_scale) 
      
      if(outline_bricks){
      } else {
        brk_out2 <- NULL
        out_list <- list(brk_fill2, brk_out2)
      }
      
      #Save ----
      return(out_list)
      
    }) %>% 
    purrr::transpose()
  
  
  #Draw
  shapelist <- c(rgl_bricks_base_list[[1]]
                 , rgl_bricks_base_list[[2]]
                 , rgl_bricks_base_list2[[1]]
                 , rgl_bricks_base_list2[[2]]
  )
  
  shapelist[sapply(shapelist, is.null)] <- NULL
  
  shapelist %>% 
    rgl::shapelist3d(lit=rgl_lit, shininess = 100, specular = "black")
  
  rgl::bg3d(color = background_color)
  
  rgl::rgl.viewpoint(userMatrix =  rgl::rotate3d(rgl::par3d("userMatrix"), 0, 0, 0 ,1) ,
                     fov=0) #All bricks, regardless of Z, are perceived as same size
  
}