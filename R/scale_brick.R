#' Create your own discrete scale
#'
#' These functions allow you to specify your own set of mappings from levels in the
#' data to aesthetic values.
#'
#' The functions `scale_colour_manual()`, `scale_fill_manual()`, `scale_size_manual()`,
#' etc. work on the aesthetics specified in the scale name: `colour`, `fill`, `size`,
#' etc. However, the functions `scale_colour_manual()` and `scale_fill_manual()` also
#' have an optional `aesthetics` argument that can be used to define both `colour` and
#' `fill` aesthetic mappings via a single function call (see examples). The function
#' `scale_discrete_manual()` is a generic scale that can work with any aesthetic or set
#' of aesthetics provided via the `aesthetics` argument.
#'
#' @param brick_theme Color options for brickr.
#' @param ... Other parameters from ggplots::scale_fill_manual().
#' @section Color Blindness:
#' Many color palettes derived from RGB combinations (like the "rainbow" color
#' palette) are not suitable to support all viewers, especially those with
#' color vision deficiencies. Using `viridis` type, which is perceptually
#' uniform in both colour and black-and-white display is an easy option to
#' ensure good perceptive properties of your visulizations.
#' The colorspace package offers functionalities
#' - to generate color palettes with good perceptive properties,
#' - to analyse a given color palette, like emulating color blindness,
#' - and to modify a given color palette for better perceptivity.
#'
#' For more information on color vision deficiencies and suitable color choices
#' see the [paper on the colorspace package](https://arxiv.org/abs/1903.06490)
#' and references therein.
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(colour = factor(cyl)))
#' p + scale_colour_manual(values = c("red", "blue", "green"))
#' @export

scale_fill_brick <- function(brick_theme = "classic", ...) {
  
  if(!(brick_theme %in% brickr_themes$theme)){
    warning(paste0("Defaulting scale_brick() to 'classic'. Use a brick_theme included in brickr:\n",
                   paste(unique(brickr_themes$theme), collapse = ", "))) 
  }
  
  values <- brickr_themes[brickr_themes$theme == brick_theme &
                            brickr_themes$index >= 1, ]

  ggplot2:::manual_scale("fill", values = values$hex, ...)
}
