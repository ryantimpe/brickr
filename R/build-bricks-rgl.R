#' Build 3D brick model with rgl
#'
#' @param brick_list List output from collect_bricks(). Contains an element \code{Img_lego}.
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
build_bricks <- function(brick_list,
                         background_color = "white", rgl_lit = TRUE,
                         outline_bricks = FALSE,
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
  height_scale = 9.6/7.8
  
  color_outline = "black"
  color_outline_trans = "white"
  
  contrast_knobs = TRUE
  contrast_lum = 0.2
  
  knob_diameter = 5/8
  brick_diameter = 96/100
  
  outline_bricks = outline_bricks
  
  suppress_knobs = TRUE #this won't draw 'hidden' knobs
  
  pieces_knobbed = c("B", "P")
  pieces_knobbed = c(pieces_knobbed, tolower(pieces_knobbed))
  
  #For now, use the current collect_bricks output. 
  #This was designed for rayshader, and I don't want to drop rayshader just yet.
  
  #Bricks & pieces without knobs ----
  
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
    length = as.numeric(substr(img_bricks$brick_type, 4, 4)),
    piece = tolower(img_bricks$piece_type)
  ) %>% 
    purrr::transpose()
  
  rgl_bricks_base_list <- rgl_bricks_base %>% 
    purrr::map(function(this_brick){
      if(!(this_brick$piece %in% pieces_knobbed)){return(NULL)}
      
      this_height = switch(this_brick$piece,
        b = 1,
        p = 1/3
      )
      
      z_drop = switch(this_brick$piece,
        b = 0,
        p = -1/3
      )
      
      #Solid brick ----
      brk_fill <- rgl::cube3d(col = this_brick$color,
                              alpha = if(this_brick$trans){trans_alpha}else{1})
      
      brk_fill$vb[4,] <- brk_fill$vb[4,]/scale*2 + nudge
      
      brk_fill2 <- brk_fill %>% 
        rgl::scale3d(this_brick$width, this_brick$length, height_scale*this_height) %>% #Increase height
        rgl::translate3d(this_brick$x, this_brick$y, this_brick$z * height_scale + z_drop*height_scale) 
      
      if(outline_bricks){
        # Brick Outline ----
        brk_out <- rgl::cube3d(col = if(this_brick$trans){colorspace::lighten(this_brick$color)}
                               else{color_outline})
        
        brk_out$vb[4,] <- brk_out$vb[4,]/scale*2 + nudge
        
        brk_out$material$lwd <- 1
        brk_out$material$front <- 'line'
        brk_out$material$back <- 'line'
        
        brk_out2 <- brk_out %>% 
          rgl::scale3d(this_brick$width, this_brick$length, height_scale*this_height) %>% #Increase height
          rgl::translate3d(this_brick$x, this_brick$y, this_brick$z * height_scale + z_drop*height_scale) 
        
        out_list <- list(brk_fill2, brk_out2)
      } else {
        brk_out2 <- NULL
        out_list <- list(brk_fill2, brk_out2)
      }
      
      #Save ----
      return(out_list)
      
    }) %>% 
    purrr::discard(is.null) %>% 
    purrr::transpose()
  
  rgl_bricks_wedge_list <- rgl_bricks_base %>% 
    purrr::map(function(this_brick){
      if(!(this_brick$piece %in% paste0("w", 1:4))){return(NULL)}
      
      #Solid brick ----
      brk_fill <- rgl::cube3d(col = this_brick$color,
                              alpha = if(this_brick$trans){trans_alpha}else{1})
      
      #Turn it into a wedge
      w_lhs <- switch(this_brick$piece,
                      w1 = c(7, 8),
                      w2 = c(6, 8),
                      w3 = c(5, 6),
                      w4 = c(5, 7))
      
      w_rhs <- switch(this_brick$piece,
                      w1 = c(3, 4),
                      w2 = c(2, 4),
                      w3 = c(1, 2),
                      w4 = c(1, 3))
      
      w_ratio = 1.2/4
      
      brk_fill$vb[, w_lhs] <- brk_fill$vb[, w_rhs] * (1-w_ratio) + brk_fill$vb[, w_lhs] * w_ratio
      
      brk_fill$vb[4,] <- brk_fill$vb[4,]/scale*2 + nudge
      
      brk_fill2 <- brk_fill %>% 
        rgl::scale3d(this_brick$width, this_brick$length, height_scale * 2/3) %>% #Increase height
        rgl::translate3d(this_brick$x, this_brick$y, 
                         this_brick$z * height_scale - height_scale*(1-2/3)/2) 
      
      if(outline_bricks){
        # Brick Outline ----
        brk_out <- rgl::cube3d(col = if(this_brick$trans){colorspace::lighten(this_brick$color)}
                               else{color_outline})
        
        #Turn it into a wedge
        brk_out$vb[, w_lhs] <- brk_out$vb[, w_rhs] * (1-w_ratio) + brk_out$vb[, w_lhs] * w_ratio
        
        brk_out$vb[4,] <- brk_out$vb[4,]/scale*2 + nudge
        
        brk_out$material$lwd <- 1
        brk_out$material$front <- 'line'
        brk_out$material$back <- 'line'
        
        brk_out2 <- brk_out %>% 
          rgl::scale3d(this_brick$width, this_brick$length, height_scale * 2/3) %>% #Increase height
          rgl::translate3d(this_brick$x, this_brick$y, 
                          this_brick$z * height_scale - height_scale*(1-2/3)/2)  
        
        out_list <- list(brk_fill2, brk_out2)
      } else {
        brk_out2 <- NULL
        out_list <- list(brk_fill2, brk_out2)
      }
      
      #Save ----
      return(out_list)
      
    }) %>% 
    purrr::discard(is.null) %>% 
    purrr::transpose()
  
  rgl_bricks_cyln_list <- rgl_bricks_base %>% 
    purrr::map(function(this_brick){
      if(!(this_brick$piece %in% c("c", paste0("c", 1:2)))){return(NULL)}
      this_piece = tolower(this_brick$piece)
    
      bottom_diameter = 12/16
      bottom_gap =  height_scale/6
      
      cyl_scale = 2
      
      #Solid brick ----
      # Base
      # Between c/c1 (cylinder) and c2 (cone), only base is different
      cyl_base_diameter = switch(
        this_piece,
        c  = brick_diameter, 
        c1 = brick_diameter,
        c2 = c(brick_diameter, (brick_diameter+knob_diameter)/2, knob_diameter)
      )
      
      cyl_base <-  rgl::cylinder3d(matrix(c(rep(1, 3), rep(1, 3))/cyl_scale, ncol=2, byrow = TRUE),
                                   sides = 32,
                                   radius = cyl_base_diameter,
                                   closed = -2)
      
      cyl_base$material$color <- this_brick$color
      cyl_base$material$alpha <- if(this_brick$trans){trans_alpha}else{1}
      
      cyl_base$vb[4,] <- cyl_base$vb[4,]/scale*2 + nudge
      
      cyl_base2 <- cyl_base %>% 
        rgl::rotate3d(pi/2, 0, 1, 0) %>% 
        rgl::scale3d(1, 1, height_scale - bottom_gap) %>% 
        rgl::translate3d(0.25, -0.25, -height_scale + bottom_gap*1.5)  %>% 
        rgl::translate3d(this_brick$x, this_brick$y, 
                         this_brick$z * height_scale) 
      
      # Knob
      cyl_knob <-  rgl::cylinder3d(matrix(c(rep(1, 3), rep(1, 3))/cyl_scale, ncol=2, byrow = TRUE),
                                   sides = 32,
                                   radius = knob_diameter,
                                   closed = -2)
      
      cyl_knob$material$color <- this_brick$color
      cyl_knob$material$alpha <- if(this_brick$trans){trans_alpha}else{1}
      
      cyl_knob$vb[4,] <- cyl_knob$vb[4,]/scale*2 + nudge
      
      cyl_knob2 <- cyl_knob %>% 
        rgl::rotate3d(pi/2, 0, 1, 0) %>% 
        rgl::scale3d(1, 1, height_scale) %>% 
        rgl::translate3d(0.25, -0.25, -height_scale + bottom_gap - 0.02 + (1.7/9.6)/2 - 0.02) %>% 
        rgl::translate3d(this_brick$x, this_brick$y, this_brick$z * height_scale) 
      
      # Bottom
      cyl_bttm <-  rgl::cylinder3d(matrix(c(rep(1, 3), rep(1, 3))/cyl_scale, ncol=2, byrow = TRUE),
                                   sides = 32,
                                   radius = bottom_diameter,
                                   closed = -2)
      
      cyl_bttm$material$color <- this_brick$color
      cyl_bttm$material$alpha <- if(this_brick$trans){trans_alpha}else{1}
      
      cyl_bttm$vb[4,] <- cyl_bttm$vb[4,]/scale*2 + nudge
      
      cyl_bttm2 <- cyl_bttm %>% 
        rgl::rotate3d(pi/2, 0, 1, 0) %>% 
        rgl::scale3d(1, 1, bottom_gap) %>%
        rgl::translate3d(0.25, -0.25, -bottom_gap*3.5)  %>% 
        rgl::translate3d(this_brick$x, this_brick$y, this_brick$z * height_scale)
      
      #Outlines ----
      if(outline_bricks){
        cyl_ot_diameter = switch(
          this_piece,
          c  = brick_diameter, 
          c1 = brick_diameter,
          c2 = knob_diameter
        )
        
        # These are 2-dimensional cylinders
          #Base, top ----
          cyl_base_ot_prep <- rgl::cylinder3d(matrix(c(rep(1, 3), rep(1, 3))/2, ncol=2, byrow = TRUE),
                                              sides = 32,
                                              radius = cyl_ot_diameter*1.015) #Conditional on cone or cylinder
          cyl_base_ot_prep$vb[4,] <- cyl_base_ot_prep$vb[4,]/scale*2 + nudge
          
          cyl_base_ot_prep$material$color <- if(this_brick$trans){colorspace::lighten(this_brick$color)}
          else{color_outline}
          
          cyl_base_ot <- cyl_base_ot_prep %>% 
            rgl::rotate3d(pi/2, 0, 1, 0) %>%
            rgl::scale3d(1, 1, 0.01) %>% #Make the height super short
            rgl::translate3d(0.25, -0.25, height_scale/2 - 0.02) %>% 
            rgl::translate3d(this_brick$x, this_brick$y, this_brick$z * height_scale)
          
          #Base, bottom ---
          cyl_bttm_ot_prep <- rgl::cylinder3d(matrix(c(rep(1, 3), rep(1, 3))/2, ncol=2, byrow = TRUE),
                                              sides = 32,
                                              radius = brick_diameter*1.015) 
          cyl_bttm_ot_prep$vb[4,] <- cyl_bttm_ot_prep$vb[4,]/scale*2 + nudge
          
          cyl_bttm_ot_prep$material$color <- if(this_brick$trans){colorspace::lighten(this_brick$color)}
          else{color_outline}
          
          cyl_bttm_ot <- cyl_bttm_ot_prep %>% 
            rgl::rotate3d(pi/2, 0, 1, 0) %>%
            rgl::scale3d(1, 1, 0.01) %>% #Make the height super short
            rgl::translate3d(0.25, -0.25, height_scale/2) %>% 
            rgl::translate3d(this_brick$x, this_brick$y, this_brick$z * height_scale) %>% 
            rgl::translate3d(0, 0, -1*(height_scale - bottom_gap))
          
          #Knob ----
          cyl_knob_ot_prep <- rgl::cylinder3d(matrix(c(rep(1, 3), rep(1, 3))/2, ncol=2, byrow = TRUE),
                                              sides = 32,
                                              radius = knob_diameter*1.015) 
          cyl_knob_ot_prep$vb[4,] <- cyl_knob_ot_prep$vb[4,]/scale*2 + nudge
          
          cyl_knob_ot_prep$material$color <- if(this_brick$trans){colorspace::lighten(this_brick$color)}
          else{color_outline}
          
          cyl_knob_ot <- cyl_knob_ot_prep %>% 
            rgl::rotate3d(pi/2, 0, 1, 0) %>%
            rgl::scale3d(1, 1, 0.01) %>% #Make the height super short
            rgl::translate3d(0.25, -0.25, height_scale/2) %>%
            rgl::translate3d(this_brick$x, this_brick$y, this_brick$z * height_scale) %>% 
            rgl::translate3d(0, 0, 0.22)
        } else {
          cyl_base_ot  <- NULL
          cyl_bttm_ot <- NULL
          cyl_knob_ot <- NULL
        }
       
      out_list <- list(cyl_base2, cyl_bttm2, cyl_knob2, 
                       cyl_base_ot, cyl_bttm_ot, cyl_knob_ot)
      
      #Save ----
      return(out_list)
      
    }) %>% 
    purrr::discard(is.null) %>% 
    purrr::transpose()
  
  #Bricks knobs ----
  
  if(suppress_knobs){
    img_lego <- img_lego %>% 
      dplyr::group_by(x, y) %>% 
      dplyr::filter(
        #Keep knobs when next level is not right above it
        (dplyr::lead(Level, order_by = Level) != Level + 1) |
          #Or next level is na
          is.na(dplyr::lead(Level, order_by = Level)) |
          # Or this or next level is transparent
          dplyr::lead(Trans_lego, order_by = Level) | Trans_lego
      ) %>% 
      dplyr::ungroup()
  }
  
  rgl_bricks_knobs <- list(
    x = img_lego$x,
    y = img_lego$y,
    z = img_lego$Level,
    color = img_lego$Lego_color,
    trans = img_lego$Trans_lego,
    lum = img_lego$lum,
    piece = tolower(img_lego$piece_type)
  ) %>% 
    purrr::transpose()
  
  rgl_bricks_knobs_list <- rgl_bricks_knobs %>% 
    purrr::map(function(this_brick){
      if(!(this_brick$piece %in% pieces_knobbed)){return(NULL)}
      
      adj_height = switch(this_brick$piece,
                           b = 1,
                           p = 1/3
      )
      
      z_drop = switch(this_brick$piece,
                      b = 0,
                      p = 1/3
      )
      
      cap_drop = switch(this_brick$piece,
                      b = 0,
                      p = -2/3
      )
      
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
        rgl::scale3d(1, 1, (height_scale*adj_height) + 1.7/9.6) %>% 
        rgl::translate3d(0.25, -0.25, -height_scale-0.02) %>% 
        rgl::translate3d(this_brick$x, this_brick$y, this_brick$z * height_scale + z_drop*height_scale)
      
      # Brick knob outlines ----
      # These are 2-dimensional cylinders
      if(outline_bricks){
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
          rgl::translate3d(0.25, -0.25, height_scale/2) %>% 
          rgl::translate3d(this_brick$x, this_brick$y, this_brick$z * height_scale + cap_drop*height_scale)
        
        #Top of the knob
        brk_knob_ot2 <- brk_knob_ot %>% 
          rgl::translate3d(0, 0, 0.22)
        
        out_list <- list(brk_knob2, brk_knob_ot, brk_knob_ot2)
      } else {
        brk_knob_ot  <- NULL
        brk_knob_ot2 <- NULL
        out_list <- list(brk_knob2, brk_knob_ot, brk_knob_ot2)
      }
      
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
          rgl::translate3d(0.25, -0.25, height_scale/2+0.22+0.01) %>%
          rgl::translate3d(this_brick$x, this_brick$y, this_brick$z * height_scale + cap_drop*height_scale)
        
        out_list[[4]] <- brk_knob_top2
      } else{
        brk_knob_top2 <- NULL
        out_list[[4]] <- brk_knob_top2
      }
      
      #Save ----
      return(out_list)
      
    }) %>% 
    purrr::discard(is.null) %>% 
    purrr::transpose()
  
  #Draw
  shapelist <- c(  purrr::flatten(rgl_bricks_base_list)
                 , purrr::flatten(rgl_bricks_wedge_list)
                 , purrr::flatten(rgl_bricks_cyln_list)
                 , purrr::flatten(rgl_bricks_knobs_list)
  )
  
  shapelist[sapply(shapelist, is.null)] <- NULL
  
  shapelist %>% 
    rgl::shapelist3d(lit=rgl_lit, shininess = 100, specular = "black")
  
  rgl::bg3d(color = background_color)
  
  rgl::rgl.viewpoint(userMatrix =  rgl::rotate3d(rgl::par3d("userMatrix"), 0, 0, 0 ,1) ,
                     fov=0) #All bricks, regardless of Z, are perceived as same size
  
}