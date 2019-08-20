#' Brick color themes - ggplot2 extension
#' 
#' @param brick_theme Color palette for bricks. Same as \code{brickr::theme_brick()}. Options include: 
#' \code{ c("classic", "hp", "sw_light", "sw_dark", "friends", "elves", 
#' "ninja", "classy", "city", "ocean", "movie", "space", 
#' "jurassic", "duplo", "superhero", "80s",
#' "rainbow7", "rainbow13", "doublerainbow", "blue")}.
#' @family Graphs
#' @export

theme_brick <- function(brick_theme = "classic"){
  if(!(brick_theme %in% brickr_themes$theme)){
    warning(paste0("Defaulting theme_brick() to 'classic'. Use a brick_theme included in brickr:\n",
                   paste(unique(brickr_themes$theme), collapse = ", "))) 
    brick_theme <- "classic"
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
      panel.grid = element_blank(),
      text = element_text(color = values$hex[2]),
      legend.text = element_text(color = values$hex[2], size = 8)
    )
  
}