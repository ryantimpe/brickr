#' ggplot2 Bar Charts as Bricks
#'
#' `geom_rect`, except bars look like LEGO(R) bricks.
#'
#' @inheritParams ggplot2::geom_rect
#' @export
geom_area_brick <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
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
      na.rm = na.rm,
      ...
    )
  )
  
  #This is the knob ggproto, but we fix the attributes and nudge the circle to the bottom right
  layer_knob_shadow <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBrickKnob,
    position = position_nudge(x = +0.08, y = -0.08),
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fill = "#333333",
      color = NA,
      alpha = 0.2,
      na.rm = na.rm
    )
  )
  
  layer_knob_base <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBrickKnob,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
  
  layer_knob_text <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBrickKnobText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = FALSE,
    params = list(
      na.rm = na.rm
    )
  )
  
  return(list(layer_brick, layer_knob_shadow, layer_knob_base, layer_knob_text
              ))
}

#' @rdname brickr-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBrick <- ggproto("GeomBrick", Geom,
                     default_aes = aes(colour = "#333333", fill = "#C4281B", size = 0.5, linetype = 1,
                                       alpha = NA),
                     
                     required_aes = c("x", "y"),
                     
                     setup_data = function(data, params) {
                       data$width <- 1
                       data$height <- 1
                       
                       transform(data,
                                 xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
                                 ymin = y - height / 2, ymax = y + height / 2, height = NULL
                       )
                     },
                     
                     draw_panel = function(self, data, panel_params, coord, linejoin = "mitre") {
                       if (!coord$is_linear()) {
                         aesthetics <- setdiff(
                           names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
                         )
                         
                         polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
                           poly <- rect_to_poly(row$xmin, row$xmax, row$ymin, row$ymax)
                           aes <- new_data_frame(row[aesthetics])[rep(1,5), ]
                           
                           GeomPolygon$draw_panel(cbind(poly, aes), panel_params, coord)
                         })
                         
                         ggplot2:::ggname("bar", do.call("grobTree", polys))
                       } else {
                         coords <- coord$transform(data, panel_params)
                         ggplot2:::ggname("geom_area_brick", grid::rectGrob(
                           coords$xmin, coords$ymax,
                           width = coords$xmax - coords$xmin,
                           height = coords$ymax - coords$ymin,
                           default.units = "native",
                           just = c("left", "top"),
                           gp = grid::gpar(
                             col = alpha(coords$colour, 0.2),
                             fill = alpha(coords$fill, coords$alpha),
                             lwd = coords$size * .pt,
                             lty = coords$linetype,
                             linejoin = linejoin,
                             # `lineend` is a workaround for Windows and intentionally kept unexposed
                             # as an argument. (c.f. https://github.com/tidyverse/ggplot2/issues/3037#issuecomment-457504667)
                             lineend = if (identical(linejoin, "round")) "round" else "square"
                           )
                         ))
                       }
                     },
                     
                     draw_key = draw_key_polygon
)

GeomBrickKnob <- ggproto("GeomBrickKnob", Geom,
                     required_aes = c("x", "y"),
                     default_aes = aes(
                       colour = "#333333", fill = "#C4281B", size = 0.3, linetype = 1,
                       alpha = NA
                     ),
                     
                     setup_data = function(data, params) {
                       data$r <- (5/8)/2 #5mm out of 8mm diameter, divided by 2
                       return(data)
                     },
                     
                     draw_panel = function(self, data, panel_params, coord, na.rm = FALSE) {

                       coords <- coord$transform(data, panel_params)

                       ggplot2:::ggname("geom_area_brick",
                              grid::circleGrob(
                                coords$x, coords$y,
                                #coords$r, 
                                r= coords$x[1]*(5/8)*(1/2)*(.8),
                                default.units = "native",
                                gp = grid::gpar(
                                  col = alpha(coords$colour, 0.2),
                                  fill = alpha(coords$fill, coords$alpha),
                                  size = coords$size * .pt,
                                  lty = coords$linetype
                                )
                              )
                       )
                     },
                     
                     draw_key = draw_key_polygon
)

# Need some hacks to rescale text size based on plot size
# https://ryouready.wordpress.com/2012/08/01/creating-a-text-grob-that-automatically-adjusts-to-viewport-size/

resizingTextGrob <- function(...) {
  grid::grob(tg=grid::textGrob(...), cl="resizingTextGrob")
}

drawDetails.resizingTextGrob <- function(x, recording=TRUE){
  grid::grid.draw(x$tg)
}

preDrawDetails.resizingTextGrob <- function(x){
  w <- grid::convertHeight(unit(1, "snpc"), "mm", valueOnly=TRUE)
  fs <- scales::rescale(w, to=c(18, 7), from=c(120, 20))
  grid::pushViewport(grid::viewport(gp = grid::gpar(fontsize = fs)))
}

GeomBrickKnobText <- ggproto("GeomBrickKnobText", Geom,
                         required_aes = c("x", "y"),
                         default_aes = aes(
                           label = "LEGO", colour = "#333333", alpha = 0.2,
                           angle = 0, family = "", fontface = 1, lineheight = 1.2
                         ),

                         draw_panel = function(self, data, panel_params, coord, na.rm = FALSE) {

                           coords <- coord$transform(data, panel_params)
                           # print(coords)

                           resizingTextGrob(
                             data$label,
                             coords$x, coords$y, 
                             default.units = "native",
                             hjust = data$hjust, vjust = data$vjust,
                             rot = data$angle,
                             gp = grid::gpar(
                               col = alpha(data$colour, data$alpha),
                               cex = 3/9 * 0.6,
                               #fontsize = 10,
                               #size = unit((5/8)/2, "npc"),
                               fontfamily = data$family,
                               fontface = data$fontface,
                               lineheight = data$lineheight
                             )
                           )
                         },

                         draw_key = draw_key_polygon
)
