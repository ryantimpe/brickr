#' Bar charts with bricks - ggplot2 extension
#' 
#' `geom_brick_col()` is the \code{brickr} version of `ggplot2::geom_col()`. 
#' Bar height is determined by values in the data using the \code{y} aesthetic. With the exception of \code{fill}, aesthetics available in `ggplot2::geom_col()` are generally not enabled here.
#
#' @inheritParams ggplot2::geom_col
#' @param label Character string to include as embossed text inside brick knobs. Maximum 6 characters.
#' @param label_scale Scale text size of label as a percentage. 
#' Sizing for the embossed text can be off. A best attempt at the text size is calculated from the device size. Zooming a plot in the RStudio window will not update the text size. 
#' If the automated size doesn't look correct after rendering the chart, use this scale.
#' @param two_knob Logical. Each bar is two knobs / studs wide. When rendering many \code{x} values, set to \code{FALSE}.
#' @param split_bricks Logical. For simpler bars, do not split into individual bricks. Knobs still render.
#' @param min_radius_for_text Knob radius as a percentage of view port. If the calculated radius is lower than this value, embossed label will not appear in knobs.
#' @param position It it not recommended to use \code{position = "stack"}.
#' @examples
#' #geom_brick_col should be used in conjunction with other brickr charting 
#' #functions, especially coord_brick.
#' df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
#' ggplot(df, aes(trt, outcome)) +
#'   geom_brick_col() +
#'   coord_brick()
#'   
#' #For official LEGO colors, use with scale_fill_brick and theme_brick.
#' ggplot(df, aes(trt, outcome)) +
#'   geom_brick_col(aes(fill = trt)) +
#'   scale_fill_brick() +
#'   coord_brick() +
#'   theme_brick()
#' @importFrom stats median
#' @family Graphs
#' @export

geom_brick_col <- function(mapping = NULL, data = NULL,
                           position = "dodge", two_knob = TRUE, split_bricks = TRUE,
                           min_radius_for_text = 0.02, label = "brickr", label_scale = 1,
                           ...,
                           width = NULL,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  
  layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomBrickCol,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      label = label,
      two_knob = two_knob, 
      split_bricks = split_bricks,
      min_radius_for_text = min_radius_for_text,
      label_scale = label_scale,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname brickr-ggproto
#' @format NULL
#' @usage NULL

GeomBrickCol <- ggproto("GeomBrickCol", GeomRect,
                        default_aes = aes(colour = "#333333", fill = "#B40000", size = 0.25, linetype = 1,
                                          alpha = NA, label = "brickr",
                                          angle = 0, family = "", fontface = 1, lineheight = 1.2),
                        required_aes = c("x", "y"),
                        
                        # These aes columns are created by setup_data(). They need to be listed here so
                        # that GeomRect$handle_na() properly removes any bars that fall outside the defined
                        # limits, not just those for which x and y are outside the limits
                        non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),
                        
                        setup_data = function(data, params) {
                          data$width <- data$width %||%
                            params$width %||% (resolution(data$x, FALSE) * 0.9)
                          
                          data$sign = sign(data$y)
                          
                          transform(data,
                                    ymin = pmin(y, 0), ymax = pmax(y, 0),
                                    xmin = x - width / 2, xmax = x + width / 2, width = NULL
                          )
                        },
                        
                        draw_panel = function(self, data, panel_params, coord, linejoin = "mitre", 
                                              min_radius_for_text = 0.02, width=NULL, label = "brickr",
                                              two_knob = TRUE, split_bricks = TRUE, label_scale =1) {
                          
                          #This happens to EACH panel
                          
                            #Parameters ----
                            
                            if(two_knob) n_knob <- 2 else n_knob <- 1

                            #Brick border ----
                            
                            coords_rect <- coord$transform(data, panel_params) %>% 
                              dplyr::mutate(size = data$size[1], linetype = data$linetype[1], 
                                            colour = data$colour[1], alpha = data$alpha[1])
                            
                            #Reverse calc for flipped

                            if(!is.null(coord$is_flipped) && coord$is_flipped()){
                              coords_rect <- flip_coords(coords_rect)
                            }
                            
                            # test_coords_rect <<- coords_rect
                            
                            # Split the bricks into 4-knob long bricks. This can be turned on and off
                            if(split_bricks){
                              brick_dims <- coords_rect %>%
                                dplyr::mutate( brick_width   = abs(xmax - xmin) / n_knob,
                                               num_of_1xs    = abs(ymax - ymin + 0.001) %/% brick_width,
                                               num_of_plates = abs(ymax - ymin + 0.001) %/% (brick_width*4) + 1) %>%  #Always at least 1 plate
                                #For negative bars, temporarily make them positive
                                dplyr::mutate(y_swap = ymin,
                                              ymin = ifelse(sign == -1, ymax, ymin),
                                              ymax = ifelse(sign == -1, ymax + (ymax - y_swap), ymax)) %>% 
                                dplyr::select(-y_swap)
                              
                              coords_rect_complete_bricks <- 1:max(brick_dims$num_of_plates) %>%
                                purrr::map_dfr(function(kk){
                                  brick_dims %>%
                                    dplyr::filter(num_of_plates >= kk) %>%
                                    dplyr::mutate(ymin_orig = ymin, ymax_orig = ymax) %>%
                                    dplyr::rowwise() %>%
                                    dplyr::mutate(ystart_ideal = ymin_orig + (kk-1)*4*brick_width,
                                                  yend_ideal = min(ymax_orig, ystart_ideal + 4*brick_width),
                                                  num_of_knobs_in_this_brick = (abs(ystart_ideal - yend_ideal) + 0.001) %/% brick_width,
                                                  ymin = ymin_orig + (kk-1)*4*brick_width,
                                                  ymax = min(ymax_orig, ymin + num_of_knobs_in_this_brick*brick_width)) %>%
                                    dplyr::ungroup()
                                })
                              
                              # test_coords_rect2 <<- coords_rect_complete_bricks
                              
                              coords_rect_unflipped <- dplyr::bind_rows(
                                #Knobbed-bricks
                                coords_rect_complete_bricks %>%
                                  dplyr::filter(num_of_knobs_in_this_brick > 0),
                                #Unknobbed caps
                                coords_rect_complete_bricks %>%
                                  dplyr::group_by(PANEL, group) %>%
                                  dplyr::filter((dplyr::n() > 1 && num_of_knobs_in_this_brick > 0) |
                                                  dplyr::n() == 1) %>%
                                  dplyr::filter(ymax == max(ymax)) %>%
                                  dplyr::ungroup() %>%
                                  dplyr::mutate(ymin = ymax,
                                                ymax = ymax_orig)
                              ) 
                              
                              coords_rect <- coords_rect_unflipped %>% 
                                #Now need to unswap negative bars
                                dplyr::mutate(
                                  y_swap = ymax,
                                  ymax = ifelse(sign == 1, ymax,
                                                ymin_orig - (ymin - ymin_orig)),
                                  ymin = ifelse(sign == 1, ymin,
                                                ymin_orig - (y_swap - ymin_orig))
                                ) %>% 
                                dplyr::select(-y_swap)
                              
                            } #End split_bricks

                            # test_coords_rect3 <<- coords_rect_unflipped
                            # test_coords_rect4 <<- coords_rect
                            
                            #Brighter colors for darker bricks
                            #Calculate brightness of color
                              # https://stackoverflow.com/questions/596216/formula-to-determine-brightness-of-rgb-color
                            color_lum <- as.data.frame(t(col2rgb(coords_rect$fill)/255))
                            coords_rect$color_intensity <- 0.299*color_lum$red + 0.587*color_lum$green + 0.114*color_lum$blue

                            coords_rect$outline_col <- ifelse(coords_rect$color_intensity <= thres_brick_lum(), "#CCCCCC", "#333333")
                            
                            #Un-Reverse calc for flipped coords
                            if(!is.null(coord$is_flipped) && coord$is_flipped()){
                              coords_rect <- flip_coords(coords_rect)
                            }
                            
                            gm_brick <- grid::rectGrob(
                              coords_rect$xmin, coords_rect$ymax,
                              width = coords_rect$xmax - coords_rect$xmin,
                              height = coords_rect$ymax - coords_rect$ymin,
                              default.units = "native",
                              just = c("left", "top"),
                              gp = grid::gpar(
                                col = alpha(coords_rect$outline_col, 0.3),
                                fill = alpha(coords_rect$fill, coords_rect$alpha),
                                lwd = coords_rect$size * .pt,
                                lty = coords_rect$linetype,
                                linejoin = linejoin,
                                lineend = if (identical(linejoin, "round")) "round" else "square"
                              )
                            )
                            
                            # Knobs ----
                            
                            coords <- coord$transform(data, panel_params)
                            # coords_knobs0 <<- coords
                            
                            #Reverse calc for flipped
                            if(!is.null(coord$is_flipped) && coord$is_flipped()){
                              coords <- flip_coords(coords)
                            }
                            
                            knobs_dims <- coords %>% 
                              dplyr::mutate(brick_width = abs(xmax - xmin)/n_knob,
                                            num_of_1x1s = (ymax-ymin) %/% brick_width,
                                            knob_radius = brick_width * (5/8) * (1/2) )
                            
                            # coords_knobs0a <<- knobs_dims
                            
                            coords_knobs <- 1:max(knobs_dims$num_of_1x1s) %>% 
                              purrr::map_dfr(function(kk){
                                dat <- knobs_dims %>% 
                                  dplyr::filter(num_of_1x1s >= kk) %>% 
                                  dplyr::mutate(y = ifelse(sign == 1,
                                                           ymin + (kk * brick_width) - brick_width/2,
                                                           ymax - (kk * brick_width) + brick_width/2))
                                
                                if(two_knob){
                                  dplyr::bind_rows(
                                    dat %>% dplyr::mutate(x = xmin + brick_width/2),
                                    dat %>% dplyr::mutate(x = xmax - brick_width/2)
                                  )
                                } else {
                                  dat %>% dplyr::mutate(x = xmin + brick_width/2)
                                }
                                
                              })
                            
                            # coords_knobs1 <<- coords_knobs
                            
                            #Outline and text for dark colors
                            color_lum <- as.data.frame(t(col2rgb(coords_knobs$fill)/255))
                            coords_knobs$color_intensity <- 0.299*color_lum$red + 0.587*color_lum$green + 0.114*color_lum$blue
                            
                            coords_knobs$text_alpha <- ifelse(coords_knobs$color_intensity <= thres_brick_lum(), 0.3, 0.3)
                            coords_knobs$text_col <- ifelse(coords_knobs$color_intensity <= thres_brick_lum(), "#CCCCCC", "#333333")
                            
                            #Un-Reverse calc for flipped
                            if(!is.null(coord$is_flipped) && coord$is_flipped()){
                              coords_knobs <- flip_coords_xy(coords_knobs)
                            }
                            
                            gm_knob_shadow <- grid::circleGrob(
                              coords_knobs$x + (1/4)*coords_knobs$knob_radius,
                              coords_knobs$y - (1/4)*coords_knobs$knob_radius,
                              r= coords_knobs$knob_radius,
                              default.units = "native",
                              gp = grid::gpar(
                                col = NA,
                                fill = alpha("#333333", 0.3),
                                size = coords_knobs$size * .pt,
                                lty = coords_knobs$linetype
                              )
                            )
                            
                            gm_knob_base <- grid::circleGrob(
                              coords_knobs$x, coords_knobs$y,
                              r= coords_knobs$knob_radius,
                              default.units = "native",
                              gp = grid::gpar(
                                col = alpha(coords_knobs$text_col, coords_knobs$text_alpha),
                                fill = alpha(coords_knobs$fill, coords_knobs$alpha),
                                size = coords_knobs$size * .pt,
                                lty = coords_knobs$linetype
                              )
                            )
                            
                            #Text ----
                            #Don't draw if there are more knobs than threshold size
                            n <- nrow(coords_knobs)
                            if (coords_knobs$knob_radius[1] < min_radius_for_text ) {
                              gm_knob_text <- grid::nullGrob()
                            } else {
                              lab <- data$label
                              if(any(nchar(lab) > 6)){
                                warning("aes `label` is too long and will be truncated. Please limit to 6 characters or less.")
                                lab <- substr(lab, 1, 6)
                              }
                              
                              label_num <- nchar(lab)[1]
                              
                              #Get view port size for initial text drawing...
                              vp_width = grid::convertWidth(unit(1, "snpc"), "mm", valueOnly=TRUE)
                              fs <- scales::rescale(vp_width, to=c(20, 7), from=c(120, 20))
                              
                              gm_knob_text <- grid::textGrob(
                                lab,
                                coords_knobs$x, coords_knobs$y,
                                default.units = "native",
                                # hjust = data$hjust, vjust = data$vjust,
                                # rot = data$angle,
                                gp = grid::gpar(
                                  col = alpha(coords_knobs$text_col, coords_knobs$text_alpha),
                                  fontsize = fs,
                                  cex = (3/8) * 0.75 * ((coords_knobs$knob_radius / 0.03)^(1/2)) * label_scale, 
                                  # fontfamily = data$family,
                                  fontface = "bold"#,
                                  # lineheight = data$lineheight
                                )
                              )
                            }
                            
                            # Combine ----
                            ggplot2:::ggname("geom_brick_rect", 
                                             grid::grobTree(gm_brick,
                                                            gm_knob_shadow, 
                                                            gm_knob_base,
                                                            gm_knob_text
                                             ))
                        },
                        
                        draw_key = draw_key_brick
)

flip_coords <- function(dat){
  dat$zmin <- dat$xmin
  dat$zmax <- dat$xmax
  dat$xmin <- dat$ymin
  dat$xmax <- dat$ymax
  dat$ymin <- dat$zmin
  dat$ymax <- dat$zmax
  dat$zmin <- NULL
  dat$zmax <- NULL
  
  return(dat)
}

flip_coords_xy <- function(dat){
  dat$z <- dat$x
  dat$x <- dat$y
  dat$y <- dat$z
  dat$z <- NULL
  
  return(dat)
}

thres_brick_lum <- function(){
  return(0.4)
}
