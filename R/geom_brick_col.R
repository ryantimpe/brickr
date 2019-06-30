#' @export
#' @rdname geom_brick_rect
geom_brick_col <- function(mapping = NULL, data = NULL,
                     position = "stack",
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
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname brickr-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom_brick.R
GeomBrickCol <- ggproto("GeomCol", GeomBrick,
                   required_aes = c("x", "y"),
                   
                   # These aes columns are created by setup_data(). They need to be listed here so
                   # that GeomRect$handle_na() properly removes any bars that fall outside the defined
                   # limits, not just those for which x and y are outside the limits
                   non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),
                   
                   setup_data = function(data, params) {
                     data$width <- data$width %||%
                       params$width %||% (resolution(data$x, FALSE) * 0.9)
                     
                     transform(data,
                               ymin = pmin(y, 0), ymax = pmax(y, 0),
                               xmin = x - width / 2, xmax = x + width / 2, width = NULL
                     )
                   },
                   
                   draw_panel = function(self, data, panel_params, coord, linejoin = "mitre", 
                                         simplified_threshold = 24*24, width=NULL) {
                     
                     #This happens to EACH panel
                     if (!coord$is_linear()) {
                       stop("geom_brick_rect must be used with linear coordinates")
                     } else {
                       
                       #Brick border ----

                       coords_rect <- coord$transform(data, panel_params) %>% 
                         dplyr::mutate(size = data$size[1], linetype = data$linetype[1], 
                                       colour = data$colour[1], alpha = data$alpha[1])
                       
                       gm_brick <- grid::rectGrob(
                         coords_rect$xmin, coords_rect$ymax,
                         width = coords_rect$xmax - coords_rect$xmin,
                         height = coords_rect$ymax - coords_rect$ymin,
                         default.units = "native",
                         just = c("left", "top"),
                         gp = grid::gpar(
                           col = alpha(coords_rect$colour, 0.2),
                           fill = alpha(coords_rect$fill, coords_rect$alpha),
                           lwd = coords_rect$size * .pt,
                           lty = coords_rect$linetype,
                           linejoin = linejoin,
                           # `lineend` is a workaround for Windows and intentionally kept unexposed
                           # as an argument. (c.f. https://github.com/tidyverse/ggplot2/issues/3037#issuecomment-457504667)
                           lineend = if (identical(linejoin, "round")) "round" else "square"
                         )
                       )
                       
                       # Knob ----
                       
                       coords <- coord$transform(data, panel_params)
                       
                       #Calculations to add 2 knobs
                       n_knob <- 2
                       
                       hmm <- coords %>% 
                         dplyr::mutate(brick_width = abs(xmax - xmin)/n_knob,
                                num_of_1x1s = (ymax-ymin) %/% brick_width,
                                knob_radius = brick_width * (5/8) /2 )
                       
                       coords_knobs <- 1:max(hmm$num_of_1x1s) %>% 
                         purrr::map_dfr(function(kk){
                           dat <- hmm %>% 
                             dplyr::filter(num_of_1x1s >= kk) %>% 
                             dplyr::mutate(y = ymin + (kk * brick_width) - brick_width/2)
                           
                           dplyr::bind_rows(
                             dat %>% dplyr::mutate(x = xmin + brick_width/2),
                             dat %>% dplyr::mutate(x = xmax - brick_width/2)
                           )
                         })
                       
                       # test_coord <<- coords

                       #Outline and text for dark colors
                       coords_knobs$color_intensity <- as.numeric(colSums(col2rgb(coords_knobs$fill)))
                       coords_knobs$text_alpha <- ifelse(coords_knobs$color_intensity < 200, 0.2, 0.2)
                       coords_knobs$text_col <- ifelse(coords_knobs$color_intensity < 200, "#CCCCCC", "#333333")

                       gm_knob_shadow <- grid::circleGrob(
                         coords_knobs$x + (1/4)*coords_knobs$knob_radius,
                         coords_knobs$y - (1/4)*coords_knobs$knob_radius,
                         r= coords_knobs$knob_radius,
                         default.units = "native",
                         gp = grid::gpar(
                           col = NA,
                           fill = alpha("#333333", 0.2),
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
                       #Don't draw if mosaic is larger than threshold size
                       n <- nrow(coords_knobs)
                       if (n > simplified_threshold ) {
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
                             cex = (3/8) * 0.5 * (1), 
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
                     }
                   },
                   
                   draw_key = draw_key_polygon
)