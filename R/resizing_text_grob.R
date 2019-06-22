#' grob for scaleable text size in geom_brick_*()
#'
#' @rdname brickr-ggproto
#' @export 
#'

resizingTextGrob <- function(...) {
  # https://ryouready.wordpress.com/2012/08/01/creating-a-text-grob-that-automatically-adjusts-to-viewport-size/
  
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
