#' Cartesian coordinates for bricks - ggplot2 extension
#'
#' A fixed scale coordinate system that ensures correct brick proportions are maintained regardless of device size. 
#' Use \code{coord_brick_flip()} for horizontal bars.
#'
#' @inheritParams ggplot2::coord_fixed
#' @examples
#' #geom_brick_col should be used in conjunction with other brickr charting functions, especially coord_brick.
#' df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
#' ggplot(df, aes(trt, outcome)) +
#'   geom_brick_col(aes(fill = trt)) +
#'   coord_brick()
#'   
#'  #horizontal bars
#' ggplot(df, aes(trt, outcome)) +
#'   geom_brick_col(aes(fill = trt)) +
#'   coord_brick_flip()
#'   
#' @export
#' @rdname coord-brick
coord_brick <- function(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") {
  ggproto(NULL, CoordBrick,
          limits = list(x = xlim, y = ylim),
          expand = expand,
          clip = clip
  )
}

#' @export
#' @rdname coord-brick
coord_brick_flip <- function(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") {
  ggproto(NULL, CoordBrickFlip,
          limits = list(x = xlim, y = ylim),
          expand = expand,
          clip = clip
  )
}

#' @rdname brickr-ggproto
#' @format NULL
#' @usage NULL
CoordBrick <- ggproto("CoordBrick", CoordCartesian,
                      is_free = function() FALSE,
                      is_flipped = function() FALSE,
                      aspect = function(self, ranges) {
                        1
                      }
)

#' @rdname brickr-ggproto
#' @format NULL
#' @usage NULL
CoordBrickFlip <- ggproto("CoordBrickFlip", CoordCartesian,
                          
                     is_flipped = function() TRUE,
                     
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
