#' Modify components of a theme
#'
#' Themes are a powerful way to customize the non-data components of your
#' plots: i.e. titles, labels, fonts, background, gridlines, and legends.
#' Themes can be used to give plots a consistent customized look.
#' Modify a single plot's theme using `theme()`; see [theme_update()] if
#' you want modify the active theme, to affect all subsequent plots. Theme
#' elements are documented together according to inheritance, read more
#' about theme inheritance below.
#'
#'@export

theme_brick <- function(brick_theme = "classic"){
  if(!(brick_theme %in% brickr_themes$theme)){
    warning(paste0("Defaulting theme_brick() to 'classic'. Use a brick_theme included in brickr:\n",
                   paste(unique(brickr_themes$theme), collapse = ", "))) 
  }
  
  values <- brickr_themes[brickr_themes$theme == brick_theme &
                            brickr_themes$index <= 0, ]
  
  theme_base <- if(brick_theme %in% c("sw_dark", "space", "ninja", "jurassic", "superhero")){
    ggplot2::theme_dark()
  } else {
    ggplot2::theme_minimal()
  }
  
  theme_base +
    ggplot2::theme(
      # plot.background = element_rect(fill = values$hex[1]),
      panel.background = element_rect(fill = values$hex[1]),
      text = element_text(color = values$hex[2]),
      legend.text = element_text(color = values$hex[2], size = 8)
    )
  
}