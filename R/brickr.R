#' \code{brickr} package
#'
#' Google spreadsheets R API
#'
#'
#' @docType package
#' @name brickr
#' @importFrom purrr %||%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", "aes", "alpha", "area_act", "area_tar", "B", "B_lego", 
                           "brick_name", "Brick_size", "brick_type", "brickr_themes", "brickrID", 
                           "channel", "col2rgb", "Color", "color_hex", "dist", "G", "G_lego", 
                           "ggproto", "hex", "lego", "Lego_color", "LEGO_color", "lego_colors", 
                           "Lego_name", "Level", "Lum", "n", "offset_x", "offset_y", "Palette", 
                           "R", "R_lego", "shade", "shade_bw", "size1", "size2", "stud", 
                           "studs", "theme", "Tr", "TYPE", "user_color", "x", "x_mid", "x_scaled", 
                           "xg", "xmax", "xmin", "xx", "y", "y_mid", "y_scaled", "yg", "ymax", 
                           "ymin", "yy", "z"))
}