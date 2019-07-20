#' Helper function to match an RGB color to a subset of allowed colors
#'
#' @param R,G,B R, G, and B color values between 0 and 1.
#' @param dat_color Data frame of allowed color. Use the attached data \code{lego_colors.rda}.
#' @return A data frame of a matched color with closest Euclidean distance to the input R, G, & B.
#' @export 
#' 
convert_to_match_color <- function(R, G, B, dat_color){
  dat_color %>% 
    dplyr::mutate(dist = ((R_lego - R)^2 + (G_lego - G)^2 + (B_lego - B)^2)^(1/2)) %>% 
    dplyr::top_n(-1, dist) %>% 
    dplyr::mutate(Lego_color = grDevices::rgb(R_lego, G_lego, B_lego)) %>% 
    dplyr::select(Lego_name = Color, Lego_color)
}

#' Convert image output from scale_image() to bricks
#'
#' @param image_list List output from scale_image(). Contains an element  \code{Img_scaled}.
#' @param color_table Defaults to \code{lego_colors}. Data frame of brick colors to map onto image. Must contain Name and R, G, B channels. See attached data  \code{lego_colors} as examples.
#' @param theme Theme of brick colors to use. Set to \code{"bw"} for grayscale mosaics.
#' @param contrast For \code{theme = "bw"}. A value >1 will increase the contrast of the image while a positive value <1 will decrease the contrast.
#' @return A list with element \code{Img_lego} containing a data frame of the x- & y-coordinates, R, G, B channels, and mapped color of each brick (pixel).
#' @export 
#'
legoize <- function(image_list, color_table = lego_colors, theme = "default", contrast = 1){
  in_list <- image_list
  
  if(theme == "default"){
    #Speed up calc by round pixel to nearest 1/20 & only calculating unique
    mosaic_colors <- in_list$Img_scaled %>% 
      dplyr::mutate_at(dplyr::vars(R, G, B), dplyr::funs(round(.*20)/20)) %>% 
      dplyr::select(R, G, B) %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(lego = purrr::pmap(list(R, G, B), convert_to_match_color, color_table)) %>% 
      tidyr::unnest(lego)
    
    img <- in_list$Img_scaled %>% 
      dplyr::mutate_at(dplyr::vars(R, G, B), dplyr::funs(round(.*20)/20)) %>%
      dplyr::left_join(mosaic_colors, by = c("R", "G", "B"))
    
  } else if (theme == "bw"){
    #Black and white is simpler... cut the colors into 4 groups, then assign lightest = white, darkest = black
    bw_colors <- color_table %>% 
      dplyr::filter(Color %in% c("Black", "White", "Dark stone grey", "Medium stone grey")) %>% 
      dplyr::arrange((R_lego + G_lego + B_lego)) %>% 
      dplyr::mutate(Lego_color = grDevices::rgb(R_lego, G_lego, B_lego))
    
    img <- in_list$Img_scaled %>% 
      dplyr::mutate(shade = (R+G+B)/3,
                    shade = shade ^ contrast) %>% 
      dplyr::mutate(shade_bw = as.numeric(as.factor(cut(shade, 4)))) %>% 
      dplyr::mutate(Lego_name = bw_colors$Color[shade_bw],
                    Lego_color = bw_colors$Lego_color[shade_bw]) %>% 
      dplyr::select(-dplyr::starts_with("shade"))
    
  }
  in_list[["Img_lego"]] <- img %>% 
    dplyr::mutate(Level = 1)
  
  return(in_list)
  
}
