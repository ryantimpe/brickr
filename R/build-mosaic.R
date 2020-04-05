#' Display 2D LEGO mosaic as a plot image
#' 
#' Render a plot image of the 2D brick mosaic with optional title.
#'
#' @param brick_obj List output from image_to_bricks(). Contains an element \code{Img_lego}.
#' @param title Optional title to include above plotted mosaic.
#' @return A single plot object to display 2D mosaic.
#' @family Mosaics
#' @export 
#' @examples 
#' 
#' # Import a jpeg or png
#'  demo_file <- system.file("extdata", "demo_img.jpg", 
#'                           package = "brickr", mustWork = TRUE)
#'  demo_image <- jpeg::readJPEG(demo_file)
#'  
#' #Build a very small 12x12 mosaic.
#'  \donttest{
#' demo_image %>% 
#'  image_to_mosaic(12) %>% 
#'  build_mosaic()
#'  }
#' 
#' #Build a mosaic in the default size of 48x48 studs with title
#' \donttest{
#' demo_image %>% 
#'  image_to_mosaic() %>% 
#'  build_mosaic("Demo mosaic")
#'  }

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

