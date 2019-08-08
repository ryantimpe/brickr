#' Color scales for bricks - ggplot2 extension
#'
#' brickr counterpart to \code{ggplot2::scale_fill_discrete()} to map bar colors to the palette of LEGO mold colors.
#'
#' @inheritParams ggplot2::scale_fill_manual
#' @param brick_theme Color palette for bricks. Same as \code{brickr::theme_brick()}. Options include: 
#' \code{ c("classic", "hp", "sw_light", "sw_dark", "friends", "elves", 
#' "ninja", "classy", "city", "ocean", "movie", "space", 
#' "jurassic", "duplo", "superhero", "80s",
#'  "rainbow7", "rainbow13", "doublerainbow", "blue")}.
#' @examples
#' df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
#' p <- ggplot(df, aes(trt, outcome)) +
#'   geom_brick_col(aes(fill = trt)) +
#'   coord_brick()
#'   
#' p  + scale_fill_brick()
#' 
#' #Select a brick_theme and use with theme_brick()
#' tm <- "hp"
#' p  + 
#'  scale_fill_brick(tm) + 
#'  theme_brick(tm)
#' @export

scale_fill_brick <- function(brick_theme = "classic", ...) {
  
  if(!(brick_theme %in% brickr_themes$theme)){
    warning(paste0("Defaulting scale_brick() to 'classic'. Use a brick_theme included in brickr:\n",
                   paste(unique(brickr_themes$theme), collapse = ", "))) 
    brick_theme <- "classic"
  }
  
  values <- brickr_themes[brickr_themes$theme == brick_theme &
                            brickr_themes$index >= 1, ]

  ggplot2:::manual_scale("fill", values = values$hex, ...)
}
