#' Display available brick colors
#' 
#' Generates a plot of available brick colors. Use .names_only = TRUE to get a list of color names.
#' 
#' @param .names_only Return an array of the 41 brick color names. Does not plot.
#' @return A table and ggplot of brick colors & ID numbers.
#' @examples 
#' #Generate plot of colors
#' build_colors()
#' 
#' #Print list of colors
#' build_colors(TRUE)
#' @family Resources
#' @export 

build_colors <- function(.names_only = FALSE){
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

#' Display available brick themes for ggplot feature scale_fill_brick()
#' 
#' Generates a plot of available brick themes.
#' @param show_theme Defaults to "all". Pass an array of theme names to only plot a subset.
#' @param .names_only Logical. Return an array of the theme names. Does not plot.
#' @return A table and ggplot of brick colors & ID numbers.
#' @examples 
#' #Generate plot of themes
#' build_themes()
#' build_themes(c("ducks", "ocean", "space"))
#' 
#' #Print list of themes
#' build_themes(.names_only = TRUE)
#' @family Resources
#' @export 

build_themes <- function(show_themes = "all", .names_only = FALSE){
  if(.names_only){
    return(unique(brickr_themes$theme))
  }
  
  if("all" %in% show_themes | !any(show_themes %in% brickr_themes$theme)){
    thms <- brickr_themes$theme
  } else {
    thms <- show_themes
  }
  
  dat <- brickr_themes %>% 
    dplyr::filter(TYPE == "color") %>% 
    dplyr::filter(theme %in% thms) %>%
    dplyr::group_by(theme) %>% 
    dplyr::mutate(y = (dplyr::row_number()-1) %/% 4,
                  x = (dplyr::row_number()-1) %% 4, 
                  x = x * 1.1, y = y * 1.1) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(theme = factor(theme, levels = unique(brickr_themes$theme)))
  
  dat$y = max(dat$y) - dat$y + 1

  brickr_themes %>% 
    dplyr::filter(TYPE == "plot") %>% 
    dplyr::filter(theme %in% thms)  %>% 
    dplyr::mutate(xmin = -0.7, xmax = 4, ymin = 0.2, ymax = max(dat$y)+0.7, x=0, y=0,
                  theme = factor(theme, levels = unique(dat$theme))) %>% 
    ggplot2::ggplot(ggplot2::aes(x=x, y=y)) +
    ggplot2::geom_rect(ggplot2::aes(fill = hex, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) + 
    geom_brick_rect(data = dat, ggplot2::aes(fill = hex), use_bricks = "1x1", 
                    label_scale = 0.2 * (20 / length(thms))^(1/6), label = "") +
    ggplot2::labs(title = "Brick themes available in {brickr}") +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_fixed(expand = FALSE) +
    ggplot2::facet_wrap(~theme, ncol = 7) +
    ggplot2::theme_void() +
    NULL
}
