#' ggplot2 Points, as Bricks
#'
#' `geom_point`, except points look like LEGO(R) studs
#'
#' @inheritParams ggplot2::geom_rect
#' @export
geom_brick_point <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            label = "LEGO", simplified_threshold = 24,
                            size = 1, stud_scale = 6/5,
                            linejoin = "mitre",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  layer_stud <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomStud,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      stud_scale = stud_scale,
      nudge = c(0, 0),
      na.rm = na.rm,
      ...
    )
  )

  layer_knob_shadow <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomStud,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      stud_scale = stud_scale,
      nudge = c(0.08, -0.08),
      fill = "#333333",
      color = NA,
      alpha = 0.2,
      size = size * (5/8),
      na.rm = na.rm,
      ...
    )
  )
  
  layer_knob_base<- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomStud,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      stud_scale = stud_scale,
      nudge = c(0, 0),
      size = size * (5/8),
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
      label = label,
      na.rm = na.rm,
      simplified_threshold = simplified_threshold
    )
  )
  
  return(list(layer_stud, layer_knob_shadow, layer_knob_base))
}


#' @rdname brickr-ggproto
GeomStud <- ggproto("GeomStud", Geom,
                    required_aes = c("x", "y"),
                    default_aes = aes(colour = "#333333", fill = "#C4281B", size = 1, linetype = 1,
                                      alpha = NA),
                    
                    draw_panel = function(data, panel_params, coord, na.rm = FALSE, 
                                          stud_scale = 6/5, nudge = c(0,0)) {
                      
                      coords <- coord$transform(data, panel_params)
                      
                      #Size of stud is proportional to number of studs on single plot?
                      n <- max(table(data$PANEL))
                      diameter <- 1/(n^(stud_scale))
                      
                      xn <- coords$x + (nudge[1])*diameter
                      yn <- coords$y + (nudge[2])*diameter
                      
                      ggplot2:::ggname("geom_brick_point",
                             grid::circleGrob(
                               xn, yn,
                               r= diameter * (1/2) * coords$size,
                               gp = grid::gpar(
                                 col = alpha(coords$colour, coords$alpha),
                                 fill = alpha(coords$fill, coords$alpha)
                               )
                             )
                      )
                    },
                    
                    draw_key = draw_key_polygon
)