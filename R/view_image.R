#' Display 2D mosaic output as a plot image
#'
#' @param image_list List output from collect_bricks() or image_to_bricks(). Contains an element  \code{Img_lego}.
#' @param title Optional title to include above plotted mosaic.
#' @export 
#'

display_set <- function(image_list, title=NULL){
  in_list <- image_list
  image <- in_list$Img_bricks
  type <- in_list$mosaic_type
  
  coord_x <- c(min(image$xmin)+0.5, max(image$xmax)-0.5)
  coord_y <- c(min(image$ymin)+0.5, max(image$ymax)-0.5)

  #FLat mosaics use the new geom_brick_rect, which looks for individual x and ys out of $Img_lego
  if(type == "flat"){
    img <- ggplot2::ggplot(in_list$Img_lego, ggplot2::aes(x=x, y=y))  +
      geom_brick_rect(ggplot2::aes(fill = Lego_color), color = "#333333")+
      ggplot2::scale_fill_identity() + 
      ggplot2::coord_fixed(expand = 0.5) 
  } else {
    img <- ggplot2::ggplot(image) +
      gplot2::geom_rect(ggplot2::aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
                                   fill = Lego_color), color = "#333333")
      ggplot2::coord_fixed(ratio = 6/5, expand = FALSE)
  }
  
  img <- img +
    ggplot2::labs(title = title) +
    ggplot2::theme_void() 
  
  return(img)
} 

#' Create instruction manual for 2D image mosaics
#'
#' @param image_list List output from collect_bricks() or image_to_bricks(). Contains an element  \code{Img_lego}.
#' @param num_steps Number of discrete steps in instruction manual
#' @export 
#'

generate_instructions <- function(image_list, num_steps=6) {
  in_list <- image_list
  image <- in_list$Img_bricks
  type <- in_list$mosaic_type
  
  num_steps <- min(abs(round(num_steps)), 40)
  
  rows_per_step <- ceiling((max(image$ymax)-0.5) / (num_steps+1))
  
  create_steps <- function(a, n_steps) {
    if(a < n_steps){
      image %>% 
        dplyr::group_by(brick_id) %>% 
        dplyr::filter(min(ymin) <= a*rows_per_step+(min(image$ymin)+0.5)) %>% 
        dplyr::ungroup() %>%
        dplyr::mutate(Step = paste("Step", (if(a<10){paste0('0', a)}else{a})))
    } else {
      image %>% 
        dplyr::mutate(Step = paste("Step", (if(a<10){paste0('0', a)}else{a})))
    }
    
  }
  
  #Ratio of the "pixels" is different for flat or stacked bricks
  if(type == "flat"){
    coord_ratio <- 1
  } else {
    coord_ratio <- 6/5
  }
  
  1:num_steps %>% 
    purrr::map(create_steps, num_steps) %>% 
    dplyr::bind_rows() %>% 
    ggplot2::ggplot() +
    ggplot2::geom_rect(ggplot2::aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
                       fill = Lego_color), color = "#333333") +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_fixed(ratio = coord_ratio, expand = FALSE) +
    ggplot2::facet_wrap(~Step) +
    ggplot2::theme_minimal() +
    ggplot2::theme( panel.background = ggplot2::element_rect(fill = "#7EC0EE"),
                    strip.background = ggplot2::element_rect(fill = "#F7F18D"),
                    strip.text = ggplot2::element_text(color = "#333333", face = "bold"),
                    axis.line = ggplot2::element_blank(),
                    axis.title.x = ggplot2::element_blank(),
                    axis.text.x = ggplot2::element_blank(),
                    axis.title.y = ggplot2::element_blank(),
                    axis.text.y = ggplot2::element_blank())
}
