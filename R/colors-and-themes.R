#' Available brick colors
#' 
#' Generates a plot of available brick colors. Use .names_only = TRUE to get a list of color names.
#' @param .names_only Return an array of the 39 brick color names. Does not plot.
#' @return A table and ggplot of brick colors & ID numbers.
#' @examples 
#' #Generate plot of colors
#' brick_colors()
#' 
#' #Print list of colors
#' brick_colors(TRUE)
#' @export 
#'
brick_colors <- function(.names_only = FALSE){
  if(.names_only){
    return(lego_colors$Color)
  }
  message("Use View(lego_colors) to see these in a table format.")

  tidyr::crossing(x=1:2, y=1:2, color = lego_colors$Color) %>% 
    dplyr::left_join(lego_colors %>% dplyr::select(color = Color, color_hex = hex), by = "color") %>% 
    dplyr::mutate(color = factor(gsub(" ", "\n", color), 
                                 levels = gsub(" ", "\n", lego_colors$Color))) %>% 
    ggplot2::ggplot(aes(x=x, y=y, group=color)) +
    ggplot2::labs(title = "Brick colors available in {brickr}") +
    geom_brick_rect(aes(fill = color_hex), label_scale = 0.1, 
                    #Including the use_bricks inputs greatly increases the speed of this.
                    use_bricks = c("2x2")) +
    ggplot2::coord_fixed(x=c(0.5, 2.5), y=c(0.5, 2.5)) +
    ggplot2::scale_fill_identity() +
    ggplot2::facet_wrap(~color, ncol = 9) +
    ggplot2::theme_void()
  
}

#' @export
#' @rdname brick_colors
#' 
display_colors <- function(...){
  warning("display_colors() is deprecated. Please use brick_colors()")
  brick_colors(...)
}

#' Available brick themes for scale_fill_brick()
#' 
#' Generates a plot of available brick themes.
#' @param .names_only Return an array of the 39 brick color names. Does not plot.
#' @return A table and ggplot of brick colors & ID numbers.
#' @examples 
#' #Generate plot of colors
#' brick_colors()
#' 
#' #Print list of colors
#' brick_colors(TRUE)
#' @export 
#'
brick_themes <- function(show_themes = "all"){
  if(show_themes == "all" | !any(show_themes %in% brickr_themes$theme)){
    thms <- brickr_themes$theme
  } else {
    thms <- show_themes
  }
  
  dat <- brickr_themes %>% 
    dplyr::filter(theme %in% thms) %>%
    dplyr::group_by(theme) %>% 
    dplyr::mutate(y = 5 - as.numeric(cut(dplyr::row_number(), 4)),
                  x = dplyr::row_number()) %>% 
    dplyr::ungroup()
  
  max(dat$x)
  
  dat %>% 
    dplyr::filter(TYPE == "color") %>% 
    ggplot2::ggplot(ggplot2::aes(x=x, y=y)) +
    ggplot2::geom_rect(aes(fill = hex),
                       xmin = 0, xmax = max(dat$x), ymin = 0, ymax = 4, 
                       data = dat %>% dplyr::filter(TYPE == "plot")) +
    geom_brick_col(ggplot2::aes(fill = hex), two_knob = F) +
    ggplot2::scale_fill_identity() +
    # ggplot2::scale_x_reverse() +
    coord_brick() +
    ggplot2::facet_wrap(~theme) +
    # ggplot2::theme_void() +
    NULL
}
