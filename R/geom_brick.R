#' ggplot2 Bar Charts as Bricks
#'
#' `geom_col`, except bars look like LEGO(R) bricks.
#'
#' @inheritParams ggplot2::geom_col
#' @export
geom_brick <- function(mapping = NULL, data = NULL,
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
    geom = GeomColBrick,
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
GeomColBrick <- ggproto("GeomColBrick", GeomRectBrick,
                   required_aes = c("x", "y"),
                   
                   # These aes columns are created by setup_data(). They need to be listed here so
                   # that GeomRect$handle_na() properly removes any bars that fall outside the defined
                   # limits, not just those for which x and y are outside the limits
                   non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),
                   
                   setup_data = function(data, params) {
                     data$width <- data$width %||%
                       params$width %||% (resolution(data$x, FALSE) * 0.9)
                     
                     #Here we want the ys to be multiples of the width of the bars
                     
                     transform(dat,
                               ymin = pmin(y, 0) %/% width, #Same as geom_col
                               ymax = pmax(y, 0) %/% width,
                               xmin = x - width / 2, 
                               xmax = x + width / 2, 
                               width = NULL
                     )
                   },
                   
                   draw_panel = function(self, data, panel_params, coord, width = NULL) {
                     # Hack to ensure that width is detected as a parameter
                     ggproto_parent(GeomRectBrick, self)$draw_panel(data, panel_params, coord)
                   }
)

#' @rdname brickr-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomRectBrick <- ggproto("GeomRectBrick", Geom,
                    default_aes = aes(colour = "black", fill = "#C4281B", size = 0.5, linetype = 1,
                                      alpha = NA),
                    
                    required_aes = c("xmin", "xmax", "ymin", "ymax"),
                    
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
                        
                        ggname("bar", do.call("grobTree", polys))
                      } else {
                        coords <- coord$transform(data, panel_params)
                        ggname("geom_rect", rectGrob(
                          coords$xmin, coords$ymax,
                          width = coords$xmax - coords$xmin,
                          height = coords$ymax - coords$ymin,
                          default.units = "native",
                          just = c("left", "top"),
                          gp = gpar(
                            col = coords$colour,
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
