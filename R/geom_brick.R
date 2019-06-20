#' ggplot2 Bar Charts as Bricks
#'
#' `geom_rect`, except bars look like LEGO(R) bricks.
#'
#' @inheritParams ggplot2::geom_rect
#' @export
geom_brick_rect <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            label = "LEGO", simplified_threshold = 24*24,
                            linejoin = "mitre",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  layer_brick <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBrick,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      linejoin = linejoin,
      label = label,
      na.rm = na.rm,
      simplified_threshold = simplified_threshold,
      ...
    )
  )

  return(layer_brick)
}


# Need some hacks to rescale text size based on plot size
# https://ryouready.wordpress.com/2012/08/01/creating-a-text-grob-that-automatically-adjusts-to-viewport-size/
#' @rdname brickr-ggproto
#' @export
resizingTextGrob <- function(...) {
  grid::grob(tg=grid::textGrob(...), cl="resizingTextGrob")
}
#' @rdname brickr-ggproto
drawDetails.resizingTextGrob <- function(x, recording=TRUE){
  grid::grid.draw(x$tg)
}
#' @rdname brickr-ggproto
preDrawDetails.resizingTextGrob <- function(x){
  w <- grid::convertWidth(unit(1, "snpc"), "mm", valueOnly=TRUE)
  fs <- scales::rescale(w, to=c(20, 7), from=c(120, 20))
  grid::pushViewport(grid::viewport(gp = grid::gpar(fontsize = fs)))
}
#' @rdname brickr-ggproto
postDrawDetails.resizingTextGrob <- function(x){
  grid::popViewport()
}


#' GeomBrick
#'
#' ggproto for brickr geoms
#'
#' @rdname brickr-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBrick <- ggproto("GeomBrick", Geom,
                     default_aes = aes(colour = "#333333", fill = "#C4281B", size = 0.5, linetype = 1,
                                       alpha = NA, label = "LEGO",
                                       angle = 0, family = "", fontface = 1, lineheight = 1.2),
                     
                     required_aes = c("x", "y"),
                     
                     setup_data = function(data, params) {
                       #This data manipulation happens BEFORE splitting the data into the PANEL
                       return(data)
                     },
                     
                     draw_panel = function(self, data, panel_params, coord, linejoin = "mitre", 
                                           simplified_threshold = 24*24) {
                       #This happens to EACH panel
                       if (!coord$is_linear()) {
                         stop("geom_brick_rect must be used with linear coordinates")
                       } else {
                         
                         #Brick border ----
                         
                         points_to_rects <- function(data){
                           #Probably super frowned upon to use dplyr inside of a ggproto, but this simplifies the data that can be fed into geom
                           data$Level <- data$PANEL
                           if(is.null(data$fill)){
                             data$Lego_name <- "#C4281B"
                             data$Lego_color <- "#C4281B"
                           }else{
                             data$Lego_name <- data$fill
                             data$Lego_color <- data$fill 
                           }
                           
                           dat <- collect_bricks(list(Img_lego = data))$Img_bricks
                           dat <- transform(dat,
                                            PANEL = Level, Level = NULL,
                                            fill = Lego_color)
                           
                           # print(dat)
                           return(dat)
                         }
                         
                         coords_rect <- coord$transform(data %>% points_to_rects, panel_params) %>% 
                           mutate(size = data$size[1], linetype = data$linetype[1], 
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

                         x_size <- median(abs(diff(coords$x)[diff(coords$x)>0]))
                         y_size <-  median(abs(diff(coords$y)[diff(coords$y)>0]))
                         diameter <- max(x_size, y_size)
                         
                         coords_nudge <- ggplot2::transform_position(coords,  
                                                                     function(x) x + x_size*(5/8)*(1/2)*(1/4),
                                                                     function(y) y - y_size*(5/8)*(1/2)*(1/4))

                         gm_knob_shadow <- grid::circleGrob(
                           coords_nudge$x,
                           coords_nudge$y,
                           r= diameter*(5/8)*(1/2),
                           default.units = "native",
                           gp = grid::gpar(
                             col = NA,
                             fill = alpha("#333333", 0.2),
                             size = coords$size * .pt,
                             lty = coords$linetype
                           )
                         )

                         gm_knob_base <- grid::circleGrob(
                           coords$x, coords$y,
                           r= diameter*(5/8)*(1/2),
                           default.units = "native",
                           gp = grid::gpar(
                             col = alpha("#333333", 0.2),
                             fill = alpha(coords$fill, coords$alpha),
                             size = coords$size * .pt,
                             lty = coords$linetype
                           )
                         )
                         
                         #Text ----
                         #Don't draw if mosaic is larger than threshold size
                         n <- nrow(data)
                         if (n > simplified_threshold ) {
                           gm_knob_text <- grid::nullGrob()
                          } else {
                            lab <- data$label
                            if(any(nchar(lab) > 6)){
                              warning("aes `label` is too long and will be truncated. Please limit to 6 characters or less.")
                              lab <- substr(lab, 1, 6)
                            }
                            
                            label_num <- nchar(lab)[1]
                            
                            gm_knob_text <- resizingTextGrob(
                              lab,
                              coords$x, coords$y,
                              default.units = "native",
                              hjust = data$hjust, vjust = data$vjust,
                              rot = data$angle,
                              gp = grid::gpar(
                                col = alpha("#333333", 0.2),
                                cex = 3/8 * 0.5 * (1.5) * ((100/n)^(1/2)), #100 bricks is optimal size for labels by default?
                                fontfamily = data$family,
                                fontface = "bold",
                                lineheight = data$lineheight
                              )
                            )
                          }
                         
                         # Combine ----
                         ggplot2:::ggname("geom_brick_rect", 
                                          grid::grobTree(gm_brick,
                                                         gm_knob_shadow, gm_knob_base,
                                                         gm_knob_text
                                                         ))
                       }
                     },
                     
                     draw_key = draw_key_polygon
)






