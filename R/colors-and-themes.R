#' Display available brick colors
#' 
#' Generates a plot of available brick colors. Use .names_only = TRUE to get a list of color names.
#' 
#' @param .names_only Return an array of the 41 solid brick color names and 13 transparent colors. Does not plot.
#' @param include_transparent Include transparent colors in the plot output.
#' @return A table and ggplot of brick colors & ID numbers.
#' @examples 
#' #Generate plot of colors
#' build_colors()
#' 
#' #Print list of colors
#' build_colors(TRUE)
#' @family Resources
#' @export 

build_colors <- function(.names_only = FALSE, include_transparent = TRUE){
  if(.names_only){
    return(lego_colors$Color)
  }
  message("Use View(lego_colors) to see these in a table format.")

  if(include_transparent){
    message("Transparent colors are only used for 3D models, not color matching in mosaics or ggplot.")
  
    use_colors <- brickr::lego_colors$Color
    use_columns <- 12
  } else{
    use_colors <- brickr::lego_colors %>% 
      dplyr::filter(!Trans_lego) %>% 
      dplyr::pull(Color)
    
    use_columns <- 9
  }
  
  tidyr::crossing(x=1:2, y=1:2, color = use_colors) %>% 
    dplyr::left_join(lego_colors %>% 
                       dplyr::select(color = Color, color_hex = hex, Trans_lego), by = "color") %>% 
    dplyr::mutate(color = factor(gsub('(.{1,8})(\\s|$)', '\\1\n',  color), 
                                 levels =gsub('(.{1,8})(\\s|$)', '\\1\n', use_colors)),
                  alpha = ifelse(Trans_lego, 0.5, 1)) %>% 
    ggplot2::ggplot(aes(x=x, y=y, group=color)) +
    ggplot2::labs(title = "Brick colors available in {brickr}") +
    brickr:::geom_brick_rect(aes(fill = color_hex, alpha = alpha), label_scale = 0.1, 
                    #Including the use_bricks inputs greatly increases the speed of this.
                    use_bricks = c("2x2")) +
    ggplot2::coord_fixed(x=c(0.5, 2.5), y=c(0.5, 2.5)) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_alpha_identity()+
    ggplot2::facet_wrap(~color, ncol = use_columns) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none"
    )
  
}
