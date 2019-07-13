#' Cartesian coordinates with fixed "aspect ratio"
#'
#' A fixed scale coordinate system forces a specified ratio between the
#' physical representation of data units on the axes. The ratio represents the
#' number of units on the y-axis equivalent to one unit on the x-axis.
#'
#' @export
coord_brick <- function(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") {
  ggproto(NULL, CoordBrick,
          limits = list(x = xlim, y = ylim),
          expand = expand,
          clip = clip
  )
}

#' Flipped Cartesian coordinates with fixed "aspect ratio"
#'
#' A fixed scale coordinate system forces a specified ratio between the
#' physical representation of data units on the axes. The ratio represents the
#' number of units on the y-axis equivalent to one unit on the x-axis. X- and y- axes are flipped.
#'
#' @export
coord_brick_flip <- function(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") {
  ggproto(NULL, CoordBrickFlip,
          limits = list(x = xlim, y = ylim),
          expand = expand,
          clip = clip
  )
}

#' @rdname brickr-ggproto
#' @usage NULL
#' @export
CoordBrick <- ggproto("CoordBrick", CoordCartesian,
                      # is_free = function() FALSE,
                      is_flipped = function() FALSE,
                      aspect = function(self, ranges) {
                        1
                      }
)

#' @rdname brickr-ggproto
#' @export
CoordBrickFlip <- ggproto("CoordBrickFlip", CoordCartesian,
                          
                     is_linear = function() "flipped",
                     
                     transform = function(data, panel_params) {
                       data <- ggplot2:::flip_labels(data)
                       CoordCartesian$transform(data, panel_params)
                     },
                     
                     backtransform_range = function(self, panel_params) {
                       self$range(panel_params)
                     },
                     
                     range = function(self, panel_params) {
                       # summarise_layout() expects the original x and y ranges here,
                       # not the ones we would get after flipping the axes
                       un_flipped_range <- ggproto_parent(CoordCartesian, self)$range(panel_params)
                       list(x = un_flipped_range$y, y = un_flipped_range$x)
                     },
                     
                     setup_panel_params = function(self, scale_x, scale_y, params = list()) {
                       parent <- ggproto_parent(CoordCartesian, self)
                       panel_params <- parent$setup_panel_params(scale_x, scale_y, params)
                       ggplot2:::flip_labels(panel_params)
                     },
                     
                     labels = function(panel_params) {
                       ggplot2:::flip_labels(CoordCartesian$labels(panel_params))
                     },
                     
                     setup_layout = function(layout, params) {
                       # Switch the scales
                       layout[c("SCALE_X", "SCALE_Y")] <- layout[c("SCALE_Y", "SCALE_X")]
                       layout
                     },
                     
                     modify_scales = function(scales_x, scales_y) {
                       lapply(scales_x, ggplot2:::scale_flip_position)
                       lapply(scales_y, ggplot2:::scale_flip_position)
                     },
                     
                     is_free = function() FALSE,
                     
                     aspect = function(self, ranges) {
                       1
                     }
                     
)
