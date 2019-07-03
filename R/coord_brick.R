#' Cartesian coordinates with fixed "aspect ratio"
#'
#' A fixed scale coordinate system forces a specified ratio between the
#' physical representation of data units on the axes. The ratio represents the
#' number of units on the y-axis equivalent to one unit on the x-axis.
#'
#' @export
#' @param ratio aspect ratio, expressed as `y / x`
#' 
#' # Resize the plot to see that the specified aspect ratio is maintained
coord_brick <- function(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") {
  ggproto(NULL, CoordBrick,
          limits = list(x = xlim, y = ylim),
          ratio = ratio,
          expand = expand,
          clip = clip
  )
}



#' @rdname brickr-ggproto
#' @export
CoordBrick <- ggproto("CoordBrick", CoordCartesian,
                      is_free = function() FALSE,
                      
                      aspect = function(self, ranges) {
                        1 * self$ratio
                      }
)