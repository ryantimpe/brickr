#' Consolidate 1x1 bricks into larger ones of the same color
#' 
#' Internal function for collecting single bricks into large ones. Used in both mosaics and 3D models.
#' Automatically generate missing piece_type or mid_level.
#'
#' @param image_list List output containing an element \code{Img_lego}.
#' @param use_bricks Array of brick sizes to use in mosaic. Defaults to \code{c('4x2', '2x2', '3x1', '2x1', '1x1')}`.
#' @param default_piece_type Piece type to use in absence of piece_type column.
#' @return A list with element \code{Img_bricks} containing a data frame of the x- & y-coordinates, R, G, B channels, and brick ID. Other helper elements.
#' @keywords internal 

collect_bricks <- function(image_list, use_bricks = NULL, 
                           default_piece_type = "b"){

  in_list <- image_list
  img_lego <- in_list$Img_lego
  
  #If no mid_level (as in a mosaic), add it
  if(!("mid_level" %in% names(img_lego))){
    img_lego <- img_lego %>%
      dplyr::mutate(mid_level = 0)
  }
  #Same with piece_type
  if(!("piece_type" %in% names(img_lego))){
    img_lego <- img_lego %>%
      dplyr::mutate(piece_type = default_piece_type)
  }
  
  #Allowed bricks ----
  
  if(is.null(use_bricks)){
    use_bricks <- c('4x2', '2x2', '4x1', '3x1', '2x1', '1x1') #3x2 once I change build_pieces()
  } else {
    #Must contain 1x1... duplicated gets dropped
    use_bricks <- c(use_bricks, '1x1')
  }
  
  brick_sizes <- tibble::tibble(bricks = use_bricks) %>% 
    tidyr::separate(bricks, c("xx", "yy"), sep = "\\D+") %>% 
    dplyr::mutate_all(as.numeric)

  brick_sizes2 <- dplyr::bind_rows(
    brick_sizes,
    brick_sizes %>% dplyr::rename(yy=1, xx=2) #Rotate the brick
  ) %>%
    dplyr::distinct() %>% 
    dplyr::mutate(offset_x = ifelse(xx <=10, purrr::map(xx, ~.x:1 -1), 0)) %>% 
    tidyr::unnest_legacy() %>% 
    dplyr::mutate(offset_y = ifelse(yy <=10, purrr::map(yy, ~.x:1 -1), 0)) %>% 
    tidyr::unnest_legacy() %>% 
    dplyr::arrange(dplyr::desc(xx*yy), #Start with bricks with most area
                   xx+yy, #Then smaller perimeter... so 2x2 is before 1x4,
                   dplyr::desc(xx), #Then widest first, so offsets are collected
                   offset_x, offset_y) %>% 
    dplyr::mutate(brick_id_loc = dplyr::row_number())
  

  # Brick looping ----
  multidim_bricks <- c("B", "P")
  multidim_bricks <- c(multidim_bricks, tolower(multidim_bricks))
  
  # Does any xx*yy space contain all the same color?
  # Only "brick" shapes will get sizes greater than 1x1
  img_multi <- (1:nrow(brick_sizes2)) %>% 
    purrr::map_dfr(function(aa){
      xx <- brick_sizes2$xx[aa]
      yy <- brick_sizes2$yy[aa]
      offset_x <- brick_sizes2$offset_x[aa]
      offset_y <- brick_sizes2$offset_y[aa]
      
      img_lego %>%
        dplyr::filter(piece_type %in% multidim_bricks) %>% 
        dplyr::select(Level, mid_level, piece_type, x, y, Lego_name, Lego_color) %>%
        dplyr::group_by(Level, mid_level, piece_type,
                        xg = (x + offset_x -1 + Level -1 + mid_level) %/% xx, 
                        yg = (y + offset_y -1 + Level -1 + mid_level) %/% yy) %>%
        dplyr::mutate(brick_type = paste0("x", xx, "y", yy, "_offx", offset_x, "_offy", offset_y)) %>% 
        dplyr::mutate(brick_name = ifelse(length(unique(Lego_name)) == 1 & dplyr::n() == (xx*yy),
                                          paste0("brick_", "x", min(x), "_y", min(y), "_", 
                                                 Level, "_", mid_level, "_", piece_type), NA_character_),
                      brick_area = xx*yy, brick_height = yy, brick_width = xx) %>% 
        dplyr::ungroup() %>% 
        dplyr::select(-xg, -yg) %>% 
        dplyr::filter(!is.na(Lego_name)) %>% 
        as.data.frame()
    }
    )
  
  img_single <- img_lego %>%
    dplyr::filter(!(piece_type %in% multidim_bricks)) %>% 
    dplyr::select(Level, mid_level, piece_type, x, y, Lego_name, Lego_color) %>%
    dplyr::mutate(brick_type = paste0("x", 1, "y", 1, "_offx", 0, "_offy", 0)) %>% 
    dplyr::mutate(brick_name = paste0("brick_", "x", x, "_y", y, "_",
                                      Level, "_", mid_level, "_", piece_type),
                  brick_area = 1, brick_height = 1, brick_width = 1) %>% 
    dplyr::filter(!is.na(Lego_name)) %>% 
    as.data.frame()
  
  #Combine multi- and single- bricks
  if(nrow(img_multi) == 0){
    img <- img_single
  } else if(nrow(img_single) == 0){
    img <- img_multi
  } else{
    img <- dplyr::bind_rows(list(img_multi, img_single))
  }
  
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
      dplyr::anti_join(bricks_df, by = c("Level", "mid_level", "x", "y")) %>% 
      #Actual Area
      dplyr::group_by(brick_name) %>% 
      dplyr::mutate(area_act = dplyr::n()) %>% 
      dplyr::ungroup() %>% 
      #Drop rows where the areas don't match
      dplyr::filter(area_act == brick_area) %>% 
      dplyr::select(-dplyr::starts_with("area")) %>% 
      as.data.frame()
    
    bricks_df <- bricks_df %>% 
      dplyr::bind_rows(dat)
  }
  
  img2 <- bricks_df %>% 
    # min/max coord for geom_rect()
    dplyr::group_by(Level, mid_level, piece_type, brick_type, brick_name, 
                    Lego_color, Lego_name, brick_width, brick_height) %>% 
    dplyr::summarise(xmin = min(x)-0.5, xmax = max(x)+0.5,
                     ymin = min(y)-0.5, ymax = max(y)+0.5) %>% 
    dplyr::ungroup()
  
  # Pieces ----
  # This is very brute-force. Probably a much cleaner way to do this
  pcs <- img2 %>% 
    dplyr::select(Level, mid_level, piece_type, brick_type, brick_name, 
                  Lego_name, Lego_color, brick_width, brick_height) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(size1 = brick_width, 
                  size2 = brick_height) %>% 
    dplyr::mutate(Brick_size = ifelse(size1>size2, paste(size1, "x", size2), paste(size2, "x" , size1))) %>% 
    dplyr::mutate(Piece = tolower(substr(piece_type, 1, 1))) %>% 
    dplyr::count(Brick_size, Piece, Lego_name, Lego_color) 
  
  in_list[["Img_bricks"]] <- img2
  in_list[["ID_bricks"]] <- bricks_df
  in_list[["pieces"]] <- pcs
  in_list[["use_bricks"]] <- use_bricks
  
  return(in_list)
}

