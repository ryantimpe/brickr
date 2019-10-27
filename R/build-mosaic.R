#' Display 2D LEGO mosaic as a plot image
#'
#' @param brick_obj List output from image_to_bricks(). Contains an element  \code{Img_lego}.
#' @param title Optional title to include above plotted mosaic.
#' @family Mosaics
#' @export 
#'

build_mosaic <- function(brick_obj, title=NULL){
  in_list <- brick_obj
  image <- in_list$Img_bricks
  type <- in_list$mosaic_type
  
  use_bricks <- in_list$use_bricks
  
  coord_x <- c(min(image$xmin)+0.5, max(image$xmax)-0.5)
  coord_y <- c(min(image$ymin)+0.5, max(image$ymax)-0.5)

  img <- ggplot2::ggplot(in_list$Img_lego, ggplot2::aes(x=x, y=y))  +
    geom_brick_rect(ggplot2::aes(fill = Lego_color), color = "#333333",
                    use_bricks = use_bricks)+
    ggplot2::scale_fill_identity() + 
    ggplot2::coord_fixed(expand = 0.5) 
  
  img <- img +
    ggplot2::labs(title = title) +
    ggplot2::theme_void() 
  
  return(img)
} 

