#' Convert a data frame in 3D matrix format into bricks for 3D Model
#'
#' @param matrix_table A data frame of a 3D brick model desigh. Left-most column is level/height/z dimension, with rows as Y axis and columns as X axis. See example. Use \code{tribble} for ease.
#' @param color_guide A data frame linking numeric \code{.value} in \code{matrix_table} to official LEGO color names. Defaults to data frame 'lego_colors'.
#' @param .re_level Logical to reassign the Level/z dimension to layers in alphanumeric order. Set to FALSE to explicitly provide levels.
#' @param increment_level Default '0'. Use in animations. Shift  Level/z dimension by an integer.
#' @param max_level Default 'Inf'. Use in animations. Any Level/z values above this value will be cut off.
#' @param increment_x Default '0'. Use in animations. Shift x dimension by an integer.
#' @param max_x Default 'Inf'. Use in animations. Any x values above this value will be cut off.
#' @param increment_y Default '0'. Use in animations. Shift y dimension by an integer.
#' @param max_y Default 'Inf'. Use in animations. Any y values above this value will be cut off.
#' @param exclude_color Numeric array of color ID numbers to exclude.
#' @param exclude_level Numeric array of Level/z dimensions to exclude.
#' @return A list with elements \code{Img_lego} to pass to \code{collect_bricks()}.
#' @export 
#'
bricks_from_table <- function(matrix_table, color_guide = lego_colors, .re_level = TRUE,
                              increment_level = 0, max_level = Inf,
                              increment_x = 0, max_x = Inf,
                              increment_y = 0, max_y = Inf,
                              exclude_color = NULL, exclude_level = NULL){
  
  #Reformat input table to consistent format
  bricks_raw <- matrix_table
  names(bricks_raw)[1] <- "Level"
  names(bricks_raw)[-1] <- paste0("X", seq_along(names(bricks_raw)[-1]))
  
  #Color mapping
  color_guide_error_msg <- "Color guide should be a data frame with at least 2 columns: `.value` and `Color`. 
  `Color` should match official LEGO names in the data frame `lego_colors`."
  
  if(identical(color_guide, brickr::lego_colors)){
    color_map <- lego_colors %>% 
      dplyr::rename(.value = brickrID)
  } else if(is.data.frame(color_guide)){
    if(ncol(color_guide) < 2){stop(color_guide_error_msg)}
    if(!(".value" %in% names(color_guide)) | !("Color" %in% names(color_guide))){stop(color_guide_error_msg)}
    
    if(!all(color_guide$Color %in% display_colors(.names_only = TRUE))){
      stop(paste("At least one color name supplied does not match allowed brick color names. See display_colors().\n\n",
                    paste(color_guide$Color[!(color_guide$Color %in% display_colors(.names_only = TRUE))],collapse = ", ")
                    ))
      
    }
      
    color_map <- color_guide %>% 
      dplyr::mutate(Color = as.character(Color)) %>% 
      dplyr::left_join(lego_colors, by = "Color")
    
  } else{
    stop(color_guide_error_msg)
  }
  
  #Literal levels or names
  if(.re_level){
    bricks_raw <- bricks_raw %>% 
      dplyr::mutate(Level = as.numeric(as.factor(as.character(Level))))
  }
  
  #Clean up increments
  incr_level <- as.numeric(increment_level)[1]
  if(is.na(incr_level)){incr_level<-0}
  incr_x <- as.numeric(increment_x)[1]
  if(is.na(incr_x)){incr_x<-0}
  incr_y <- as.numeric(increment_y)[1]
  if(is.na(incr_y)){incr_y<-0}

  brick_set <- bricks_raw %>% 
    dplyr::mutate_all(dplyr::funs(ifelse(is.na(.), 0, .))) %>% 
    dplyr::group_by(Level) %>% 
    dplyr::mutate(y = dplyr::n() - dplyr::row_number() + 1) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(Level, y, dplyr::everything()) %>% 
    tidyr::gather(x, .value, 3:ncol(.)) %>% 
    dplyr::mutate(x = as.numeric(substr(x, 2, 20))) %>% 
    dplyr::arrange(Level, x, dplyr::desc(y)) %>% 
    tidyr::drop_na(.value) %>% 
    dplyr::left_join(color_map, by = ".value") %>% 
    dplyr::mutate_at(dplyr::vars(dplyr::contains("_lego")), dplyr::funs(ifelse(is.na(.), 0, .))) %>% 
    dplyr::mutate(Lego_color = grDevices::rgb(R_lego, G_lego, B_lego)) %>% 
    dplyr::mutate(Lego_color = ifelse(is.na(Color),NA, Lego_color)) %>% 
    dplyr::rename(Lego_name = Color) %>%
    dplyr::arrange(Level) %>% 
    #Exclusions
    dplyr::filter(!(.value %in% exclude_color)) %>% 
    dplyr::filter(!(Level %in% exclude_level)) %>% 
    #Increment coordinates
    dplyr::mutate(Level = Level + incr_level,
                  x = x + incr_x, y = y + incr_y) %>% 
    dplyr::filter(Level >= 1, Level <= max_level,
                  x >= 1, x <= max_x,
                  y >= 1, y <= max_y) %>% 
    #In the end, drop empty levels
    dplyr::group_by(Level) %>% 
    dplyr::filter(!all(is.na(Lego_color))) %>% 
    dplyr::ungroup()
  
  #Return an object from collect_bricks()
  return(
    list(Img_lego =  brick_set) %>% collect_bricks
  )
}

#' Convert an Excel {brickr} template into a 3D model. https://github.com/ryantimpe/brickr_toybox
#' @param excel_table Sheet imported from a brickr Excel template to build model. Contains stud placement and colors.
#' @param repeat_levels How many times to repeat a level. Can save time in model planning. Default is 1.
#' @inheritParams bricks_from_table
#' @return A list with elements \code{Img_lego} to pass to \code{collect_bricks()}.
#' @export 
#'
bricks_from_excel <- function(excel_table, repeat_levels = 1,
                              increment_level = 0, max_level = Inf,
                              increment_x = 0, max_x = Inf,
                              increment_y = 0, max_y = Inf,
                              exclude_color = NULL, exclude_level = NULL){
  
  columns_meta_start <- max(which(grepl("^\\d", names(excel_table))))
  
  #Set Instructions
  instructions <- excel_table %>% 
    dplyr::select(1:columns_meta_start) %>% 
    dplyr::rename(Level = 1) %>% 
    dplyr::filter(Level != "Level")
  
  #Repeat levels. 
  #TODO: DO this for x and y too
  if(is.numeric(repeat_levels)){
    rep_levels <- max(round(repeat_levels), 1)
    
    if(rep_levels == 1){instructions <- instructions}
    else{
      instructions <- 1:rep_levels %>% 
        purrr::map_df(
          ~dplyr::mutate(instructions, Level = paste0(Level, .x))
        )
    }
  }
  
  #Color Instructions
  colors_user <- excel_table %>% 
    dplyr::select(.value = user_color, Color = LEGO_color) %>% 
    tidyr::drop_na()
  
  #Render as brickr output
  brickr_out <- instructions %>% 
    bricks_from_table(color_guide =  colors_user,
                      .re_level = TRUE,
                      increment_level = increment_level, max_level = max_level,
                      increment_x = increment_x, max_x = max_x,
                      increment_y = increment_y, max_y = max_y,
                      exclude_color = exclude_color, exclude_level = exclude_level)
  return(brickr_out)
}

#' Convert a data frame with x, y, & z coordinates & Color into bricks for 3D Model
#'
#' @param coord_table A data frame of a 3D brick model design. Contains x, y, and z (vertical height) dimensions, as well as Color from official LEGO color names. See \code{display_colors()}.
#' @param increment_level Default '0'. Use in animations. Shift  Level/z dimension by an integer.
#' @param max_level Default 'Inf'. Use in animations. Any Level/z values above this value will be cut off.
#' @param increment_x Default '0'. Use in animations. Shift x dimension by an integer.
#' @param max_x Default 'Inf'. Use in animations. Any x values above this value will be cut off.
#' @param increment_y Default '0'. Use in animations. Shift y dimension by an integer.
#' @param max_y Default 'Inf'. Use in animations. Any y values above this value will be cut off.
#' @param exclude_color Numeric array of color ID numbers to exclude.
#' @param exclude_level Numeric array of Level/z dimensions to exclude.
#' @return A list with elements \code{Img_lego} to pass to \code{collect_bricks()}.
#' @export 
#'
bricks_from_coords <- function(coord_table, color_guide = lego_colors,
                              increment_level = 0, max_level = Inf,
                              increment_x = 0, max_x = Inf,
                              increment_y = 0, max_y = Inf,
                              exclude_color = NULL, exclude_level = NULL){
  
  #Reformat input table to consistent format
  bricks_raw <- coord_table
  
  #Check for x y z and Color columns
  names(bricks_raw)[names(bricks_raw) %in% c("X", "Y", "Z")] <- tolower(names(bricks_raw)[names(bricks_raw) %in% c("X", "Y", "Z")])
  names(bricks_raw)[tolower(names(bricks_raw)) == "color"] <- "Color"
  
  if(!all(c("x", "y", "z", "Color") %in% names(bricks_raw))){
    stop("Input 'coord_table' must include the columns x, y, z, and Color. z should be >1. Color uses offical brick color names. See display_colors().")
  }
  
  #x, y, z, must be whole numbers and unique
  bricks_raw <- bricks_raw %>% 
    dplyr::mutate_at(dplyr::vars("x", "y", "z"), round) %>% 
    dplyr::group_by(x, y, z) %>% 
    dplyr::filter(dplyr::row_number() == 1) %>% 
    dplyr::ungroup()
  
  #Need to assume users might supply NA-supressed or non-cube data. But layer_from_brick() needs cubes.
  bricks_full <- expand.grid(
    x = min(bricks_raw$x):max(bricks_raw$x),
    y = min(bricks_raw$y):max(bricks_raw$y),
    z = min(bricks_raw$z):max(bricks_raw$z)
  ) %>% 
    dplyr::left_join(bricks_raw, by = c("x", "y", "z"))
  
  #Clean up increments
  incr_level <- as.numeric(increment_level)[1]
  if(is.na(incr_level)){incr_level<-0}
  incr_x <- as.numeric(increment_x)[1]
  if(is.na(incr_x)){incr_x<-0}
  incr_y <- as.numeric(increment_y)[1]
  if(is.na(incr_y)){incr_y<-0}
  
  brick_set <- bricks_full %>% 
    dplyr::rename(Level = z) %>% 
    dplyr::left_join(brickr::lego_colors %>% dplyr::select(Color, dplyr::contains("_lego")), 
                     by = "Color") %>% 
    dplyr::mutate_at(dplyr::vars(dplyr::contains("_lego")), dplyr::funs(ifelse(is.na(.), 0, .))) %>% 
    dplyr::mutate(Lego_color = grDevices::rgb(R_lego, G_lego, B_lego)) %>% 
    dplyr::mutate(Lego_color = ifelse(is.na(Color),NA, Lego_color)) %>% 
    dplyr::rename(Lego_name = Color) %>%
    dplyr::arrange(Level) %>% 
    #Exclusions
    dplyr::filter(!(Lego_name %in% exclude_color)) %>%
    dplyr::filter(!(Level %in% exclude_level)) %>% 
    #Increment coordinates
    dplyr::mutate(Level = Level + incr_level,
                  x = x + incr_x, y = y + incr_y) %>% 
    dplyr::filter(Level >= 1, Level <= max_level,
                  x >= 1, x <= max_x,
                  y >= 1, y <= max_y) %>% 
    #In the end, drop empty levels
    dplyr::group_by(Level) %>%
    dplyr::filter(!all(is.na(Lego_color))) %>%
    dplyr::ungroup()
  
  #Return an object from collect_bricks()
  return(
    list(Img_lego =  brick_set) %>% collect_bricks
  )
}
