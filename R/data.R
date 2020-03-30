#' Brickr colors available for mosaics & 3D models
#'
#' A dataset containing the 54 colors available in 'brickr', along with metadata
#'
#' @format A data frame with 54 rows and 10 variables:
#' \describe{
#'   \item{brickrID}{integer, simple color number for use in mosaic creation}
#'   \item{Color}{color name}
#'   \item{LEGONo}{integer, color number according to The LEGO Group}
#'   \item{Palette}{Name of palette, either Universal, Generic, or Special}
#'   \item{R_lego}{Red channel, (0-1)}
#'   \item{G_lego}{Green channel, (0-1)}
#'   \item{B_lego}{Blue channel, (0-1)}
#'   \item{Trans_lego}{Transparent color, TRUE or FALSE}
#'   \item{hex}{Color hex code}
#'   \item{lum}{Color brightness, (0-1)}
#' }
#' @source \url{https://brickarchitect.com/color/}
"lego_colors"