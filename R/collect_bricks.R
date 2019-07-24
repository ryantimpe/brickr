
#' Collect legoize image from individual bricks into grouped bricks.
#'
#' @param image_list List output from legoize(). Contains an element  \code{Img_lego}.
#' @param use_bricks Array of brick sizes to use in mosaic. Defaults to \code{c('4x2', '2x2', '3x1', '2x1', '1x1')}`.
#' @param mosaic_type DEPRECATED. All mosaics are "flat" or knobs-up.
#' @return A list with element \code{Img_bricks} containing a data frame of the x- & y-coordinates, R, G, B channels, and brick ID. Other helper elements.
#' @export 
#'
collect_bricks <- function(image_list, use_bricks = NULL,
                           mosaic_type = NULL){
  
  #No more mosaic_type options as of Summer 2019
  if(!is.null(mosaic_type)){warning("`mosaic_type` is deprecated. Stacked mosaics have been removed from brickr for now.")}
  
  in_list <- image_list
  
  #Allowed bricks ----
  
  if(is.null(use_bricks)){
    use_bricks <- c('4x2', '2x2', '4x1', '3x1', '2x1', '1x1')
  } else {
    #Must contain 1x1... duplicated gets dropped
    use_bricks <- c(use_bricks, '1x1')
  }
  
  brick_sizes <- tibble::tibble(bricks = use_bricks) %>% 
    tidyr::separate(bricks, c("xx", "yy"), sep = "[^[\\d+]]+") %>% 
    dplyr::mutate_all(as.numeric)

  brick_sizes2 <- dplyr::bind_rows(
    brick_sizes,
    brick_sizes %>% dplyr::rename(yy=1, xx=2)
  ) %>%
    dplyr::distinct() %>% 
    dplyr::mutate(offset_x = purrr::map(xx, ~.x:1 -1)) %>% 
    tidyr::unnest() %>% 
    dplyr::mutate(offset_y = purrr::map(yy, ~.x:1 -1)) %>% 
    tidyr::unnest() %>% 
    dplyr::arrange(dplyr::desc(xx*yy), #Start with bricks with most area
                   xx+yy, #Then smaller perimeter... so 2x2 is before 1x4,
                   dplyr::desc(xx), #Then widest first, so offsets are collected
                   offset_x, offset_y) %>% 
    dplyr::mutate(brick_id_loc = dplyr::row_number())
  

  # Brick looping ----
  # Does any xx*yy space contain all the same color?
  img <- (1:nrow(brick_sizes2)) %>% 
    purrr::map_dfr(function(aa){
      xx <- brick_sizes2$xx[aa]
      yy <- brick_sizes2$yy[aa]
      offset_x <- brick_sizes2$offset_x[aa]
      offset_y <- brick_sizes2$offset_y[aa]
      
      in_list$Img_lego %>%
        #Weird bug when resetting the level
        dplyr::select(Level, x, y, Lego_name, Lego_color) %>%
        # dplyr::mutate(Level = ifelse(!is.numeric(lll), as.numeric(as.factor(lll)), lll)) %>% 
        # dplyr::select(-lll) %>% 
        dplyr::group_by(Level, 
                        xg = (x + offset_x -1 + Level -1) %/% xx, 
                        yg = (y + offset_y -1 + Level -1) %/% yy) %>%
        dplyr::mutate(brick_type = paste0("x", xx, "y", yy, "_offx", offset_x, "_offy", offset_y)) %>% 
        dplyr::mutate(brick_name = ifelse(length(unique(Lego_name)) == 1 & dplyr::n() == (xx*yy),
                                          paste0("brick_", "x", min(x), "_y", min(y), "_", Level), NA)) %>% 
        dplyr::ungroup() %>% 
        dplyr::select(-xg, -yg)
      
    }
    )
  
  #Output of all brick types... size * layout
  bricks <- unique(img$brick_type)
  
  bricks_df <- img %>% 
    dplyr::filter(dplyr::row_number() < 1)
  
  #Reduce to the biggest and first brick ----
  #Iteratively go through each brick, in order from largest to smallest, removing them and then checking the remaining image for complete bricks.
  for(bb in bricks){
    dat <- img %>% 
      dplyr::filter(brick_type == bb) %>% 
      tidyr::drop_na(brick_name) %>% 
      dplyr::anti_join(bricks_df, by = c("Level", "x", "y")) %>% 
      #Necessary Area
      dplyr::mutate(area_tar = as.numeric(substr(brick_type, 2,2)) *  as.numeric(substr(brick_type, 4,4))) %>%
      #Actual Area
      dplyr::group_by(brick_name) %>% 
      dplyr::mutate(area_act = dplyr::n()) %>% 
      dplyr::ungroup() %>% 
      #Drop rows where the areas don't match
      dplyr::filter(area_act == area_tar) %>% 
      dplyr::select(-dplyr::starts_with("area"))
    
    bricks_df <- bricks_df %>% 
      dplyr::bind_rows(dat)
  }
  
  img2 <- bricks_df %>% 
    # min/max coord for geom_rect()
    dplyr::group_by(Level, brick_type, brick_name, Lego_color, Lego_name) %>% 
    dplyr::summarise(xmin = min(x)-0.5, xmax = max(x)+0.5,
                     ymin = min(y)-0.5, ymax = max(y)+0.5) %>% 
    dplyr::ungroup()
  
  # Pieces ----
  # This is very brute-force. Probably a much cleaner way to do this
  pcs <- img2 %>% 
    dplyr::select(Level, brick_type, brick_name, Lego_name, Lego_color) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(size1 = as.numeric(substr(brick_type, 2, 2)), 
                  size2 = as.numeric(substr(brick_type, 4, 4))) %>% 
    dplyr::mutate(Brick_size = ifelse(size1>size2, paste(size1, "x", size2), paste(size2, "x" , size1))) %>% 
    dplyr::count(Brick_size, Lego_name, Lego_color) 
  
  in_list[["Img_bricks"]] <- img2
  in_list[["ID_bricks"]] <- bricks_df
  in_list[["mosaic_type"]] <- "flat"
  in_list[["pieces"]] <- pcs
  
  return(in_list)
}

