#' Build 3D brick model with rgl
#'
#' @param brick_list List output from collect_bricks(). Contains an element \code{Img_lego}.
#' @param brick_type Type of brick to use. Default is 'brick'. Other option is 'plate', which is 1/3 the height of a brick.
#' @param view_levels Numeric array of Levels/z values to display. Leave as \code{NULL} to include all.
#' @param solidcolor Hex color of mosaic base. Only renders on bottom.
#' @param water Default 'FALSE'. If 'TRUE', a water layer is rendered.
#' @param waterdepth Default '0'. Water level.
#' @param ... All other inputs from rayshader::plot_3d() EXCEPT \code{hillshade}, \code{soliddepth}, \code{zscale}, and \code{shadow}.
#' @return 3D brick model rendered in the 'rgl' package.
#' @family 3D Models
#' @export 
#'
build_bricks_rgl <- function(brick_list,
                             background_color = "white", rgl_lit = FALSE,
                             trans_alpha = 0.5,
                             view_levels = NULL){
  #Get previous data
  in_list <- brick_list
  
  img_lego <- in_list$Img_lego %>% 
    tidyr::drop_na() %>% 
    dplyr::select(-dplyr::contains("lum")) %>% 
    dplyr::left_join(lego_colors %>%
                       dplyr::select(Lego_name = Color, lum),
                     by = c("Lego_name"))
  
  img_bricks <- in_list$Img_bricks %>% 
    tidyr::drop_na()%>% 
    dplyr::left_join(lego_colors %>% 
                       dplyr::select(Lego_name = Color, Trans_lego, lum),
                     by = c("Lego_name"))
  
  if(is.null(view_levels)){
    view_levels <- unique(img_lego$Level)
  }
  
  #SET PARAMETERS ----
  # For use inside brick drawing functions below
  nudge = 0.01 #Space between bricks
  scale = 1 #Reduce to unit size
  # trans_alpha = 0.5 #Alpha of transparent bricks
  height_scale = 9.6/7.8
  
  color_outline = "black"
  color_outline_trans = "white"
  
  contrast_knobs = TRUE
  contrast_lum = 0.2
  
  knob_diameter = 5/8
  
  brick_outlines = TRUE
  
  # rgl_lit = FALSE
  
  #For now, use the current collect_bricks output. 
  #This was designed for rayshader, and I don't want to drop rayshader just yet.
  
  #Bricks without knobs ----
  
  rgl_bricks_base <- list(
    # x & y are the CENTERS of bricks. rgl scales shapes from center
    x = img_bricks$xmin + 0.5 + (img_bricks$xmax - img_bricks$xmin)/2 ,
    y = img_bricks$ymin + 0.5 + (img_bricks$ymax - img_bricks$ymin)/2 ,
    z = img_bricks$Level,
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
      
      brk_fill$vb[4,] <- brk_fill$vb[4,]/scale*2 + nudge
      
      brk_fill2 <- brk_fill %>% 
        rgl::scale3d(this_brick$width, this_brick$length, height_scale) %>% #Increase height
        rgl::translate3d(this_brick$x, this_brick$y, this_brick$z * height_scale) 
      
      # Brick Outline ----
      brk_out <- rgl::cube3d(col = if(this_brick$trans){colorspace::lighten(this_brick$color)}
                             else{color_outline})
      
      brk_out$vb[4,] <- brk_out$vb[4,]/scale*2 + nudge
      
      brk_out$material$lwd <- 1
      brk_out$material$front <- 'line'
      brk_out$material$back <- 'line'
      
      brk_out2 <- brk_out %>% 
        rgl::scale3d(this_brick$width, this_brick$length, height_scale) %>% #Increase height
        rgl::translate3d(this_brick$x, this_brick$y, this_brick$z * height_scale) 
      
      #Save ----
      return(list(brk_fill2, brk_out2))
      
    }) %>% 
    purrr::transpose()
  
  #Bricks knobs ----
  rgl_bricks_knobs <- list(
    x = img_lego$x,
    y = img_lego$y,
    z = img_lego$Level,
    color = img_lego$Lego_color,
    trans = img_lego$Trans_lego,
    lum = img_lego$lum
  ) %>% 
    purrr::transpose()
  
  rgl_bricks_knobs_list <- rgl_bricks_knobs %>% 
    purrr::map(function(this_brick){
      
      # Brick knob ----
      brk_knob <- rgl::cylinder3d(matrix(c(rep(1, 3), rep(1, 3))/2, ncol=2, byrow = TRUE),
                                  sides = 32,
                                  radius = knob_diameter,
                                  closed = -2) 
      
      brk_knob$vb[4,] <- brk_knob$vb[4,]/scale*2 + nudge
      
      #Knob side color
      if(contrast_knobs & !this_brick$trans){
        if(this_brick$lum <= contrast_lum){
          brk_knob$material$color <- colorspace::lighten(this_brick$color)
        } else {
          brk_knob$material$color <- colorspace::darken(this_brick$color)
        }
      } else{
        brk_knob$material$color <- this_brick$color
      }
      brk_knob$material$alpha <- if(this_brick$trans){trans_alpha}else{1}
      
      this_brick$x <- this_brick$x + 0.5
      this_brick$y <- this_brick$y + 0.5
      
      brk_knob2 <- brk_knob %>% 
        rgl::rotate3d(pi/2, 0, 1, 0) %>% 
        rgl::scale3d(1, 1, height_scale + 1.7/9.6) %>% 
        rgl::translate3d(0.25, -0.25, -height_scale-0.02) %>% 
        rgl::translate3d(this_brick$x, this_brick$y, this_brick$z * height_scale)
      
      # Brick knob outlines ----
      # These are 2-dimensional cylinders
      
      brk_knob_ot_prep <- rgl::cylinder3d(matrix(c(rep(1, 3), rep(1, 3))/2, ncol=2, byrow = TRUE),
                                          sides = 32,
                                          radius = knob_diameter*1.015) 
      brk_knob_ot_prep$vb[4,] <- brk_knob_ot_prep$vb[4,]/scale*2 + nudge
      
      brk_knob_ot_prep$material$color <- if(this_brick$trans){colorspace::lighten(this_brick$color)}
      else{color_outline}
      
      #Base of the knob
      brk_knob_ot <- brk_knob_ot_prep %>% 
        rgl::rotate3d(pi/2, 0, 1, 0) %>%
        rgl::scale3d(1, 1, 0.01) %>% #Make the height super short
        rgl::translate3d(0.25, -0.25, 0.62) %>% 
        rgl::translate3d(this_brick$x, this_brick$y, this_brick$z * height_scale)
      
      #Top of the knob
      brk_knob_ot2 <- brk_knob_ot %>% 
        rgl::translate3d(0, 0, 0.22)
      
      #Brick knob cap ----
      # This uses the bricks color if the knob has contrasting sides
      # Only use if the knob side is contrasted
      if(contrast_knobs){
        brk_knob_top <- rgl::cylinder3d(matrix(c(rep(1, 3), rep(1, 3))/2, ncol=2, byrow = TRUE),
                                        sides = 32,
                                        radius = knob_diameter*.99,
                                        closed = -2)
        
        brk_knob_top$vb[4,] <- brk_knob_top$vb[4,]/scale*2 + nudge
        brk_knob_top$material$color <- this_brick$color
        
        brk_knob_top$material$alpha <- if(this_brick$trans){trans_alpha}else{1}
        
        #A 2-dimensional filled circle on the top the knob
        brk_knob_top2 <- brk_knob_top %>% 
          rgl::rotate3d(pi/2, 0, 1, 0) %>%
          rgl::scale3d(1, 1, 0.01) %>% 
          rgl::translate3d(0.25, -0.25, 0.62+0.22+0.01) %>% 
          rgl::translate3d(this_brick$x, this_brick$y, this_brick$z * height_scale)
        
        out_list <- list(brk_knob2, brk_knob_ot, brk_knob_ot2, brk_knob_top2)
      } else{
        brk_knob_top2 <- NULL
        out_list <- list(brk_knob2, brk_knob_ot, brk_knob_ot2)
      }
      
      #Save ----
      return(out_list)
      
    }) %>% 
    purrr::transpose()
  
  #Draw
  shapelist <- c(rgl_bricks_base_list[[1]]
                 , rgl_bricks_base_list[[2]]
                 , purrr::flatten(rgl_bricks_knobs_list)
  )
  
  shapelist %>% 
    rgl::shapelist3d(lit=rgl_lit, shininess = 100, specular = "black")
  
  rgl::bg3d(color = background_color)
  
}