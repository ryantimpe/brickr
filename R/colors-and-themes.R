#' Display available brick colors
#' 
#' Generates a plot of available brick colors. These names must be used exactly when creating custom name lists.
#' There are 41 solid brick color names and 13 transparent colors. Transparent colors are not used in mosaics.
#' 
#' Use .names_only = TRUE to get a list of color names.
#' 
#' @param .names_only Return an array of the 41 solid brick color names and 13 transparent colors. Does not plot.
#' @param include_transparent Include transparent colors in the plot output.
#' @return An array or ggplot of brick colors & ID numbers.
#' @examples 
#' #Generate plot of colors
#' build_colors(include_transparent = FALSE)
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
    use_columns <- 5
  } else{
    use_colors <- brickr::lego_colors %>% 
      dplyr::filter(!Trans_lego) %>% 
      dplyr::pull(Color)
    
    use_columns <- 4
  }
  
  tibble::tibble(x=1, y=length(use_colors):1, color = use_colors) %>% 
    dplyr::left_join(lego_colors %>% 
                       dplyr::select(color = Color, color_hex = hex, Trans_lego), by = "color") %>% 
    #Split into different lists
    dplyr::mutate(id = 5 - (y %% use_columns) )  %>% 
    dplyr::group_by(id) %>% 
    dplyr::mutate(y = dplyr::n() - dplyr::row_number() + 1) %>% 
    dplyr::mutate(alpha = ifelse(Trans_lego, 0.5, 1)) %>% 
    ggplot2::ggplot(ggplot2::aes(x=x, y=y, group=color)) +
    ggplot2::labs(title = "Brick colors available in {brickr}") +
    ggplot2::geom_raster(ggplot2::aes(fill = color_hex, alpha = alpha)) +
    ggplot2::geom_label(ggplot2::aes(label = color, alpha = 0.7), size = 3) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_alpha_identity()+
    ggplot2::facet_wrap(~id, ncol = use_columns) +
    ggplot2::theme_void() +
    ggplot2::theme(
      strip.text = ggplot2::element_blank(),
      legend.position = "none"
    )
  
}
