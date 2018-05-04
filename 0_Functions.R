library(tidyverse); library(jpeg)

# LEGO colors ----
lego_colors <- read_csv("Colors/Lego_Colors.csv") %>% 
  filter(c_Palette2016, !c_Transparent, !c_Glow, !c_Metallic) %>% 
  mutate_at(vars(R, G, B), funs(./255)) %>% 
  rename(R_lego = R, G_lego = G, B_lego = B)%>% 
  mutate_at(vars(starts_with("w_")), funs(ifelse(is.na(.), 0, .)))

#GGplot theme to remove axes, etc ----
theme_lego <- theme(panel.background = element_rect(fill = "#7EC0EE"),
        strip.background = element_rect(fill = "#F7F18D"),
        strip.text = element_text(color = "#333333", face = "bold"),
        axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

#1 SCALE IMAGE ----
scale_image <- function(image, img_size){
  #Convert image to a data frame with RGB values
  img <- bind_rows(
    list(
      (as.data.frame(image[, , 1]) %>% 
         mutate(y=row_number(), channel = "R")),
      (as.data.frame(image[, , 2]) %>% 
         mutate(y=row_number(), channel = "G")),
      (as.data.frame(image[, , 3]) %>% 
         mutate(y=row_number(), channel = "B"))
    )
  ) %>% 
    gather(x, value, -y, -channel) %>% 
    mutate(x = as.numeric(gsub("V", "", x))) %>% 
    spread(channel, value)
  
  #Wide or tall image? Shortest side should be `img_size` pixels
    if(max(img$x) > max(img$y)){
      img_scale_x <-  max(img$x) / max(img$y)
      img_scale_y <- 1
    } else {
      img_scale_x <- 1
      img_scale_y <-  max(img$y) / max(img$x)
    }
  
  #If only 1 img_size value, create a square image
    if(length(img_size) == 1){
      img_size2 <- c(img_size, img_size)
    } else {
      img_size2 <- img_size[1:2]
    }

  #Rescale the image
  img2 <- img %>% 
    mutate(y_scaled = (y - min(y))/(max(y)-min(y))*img_size2[2]*img_scale_y + 1,
           x_scaled = (x - min(x))/(max(x)-min(x))*img_size2[1]*img_scale_x + 1) %>% 
    select(-x, -y) %>% 
    group_by(y = ceiling(y_scaled), x = ceiling(x_scaled)) %>% 
    #Get average R, G, B and convert it to hexcolor
    summarize_at(vars(R, G, B), funs(mean(.))) %>% 
    rowwise() %>% 
    mutate(color = rgb(R, G, B)) %>% 
    ungroup() %>% 
    #Center the image
    filter(x <= median(x) + img_size2[1]/2, x > median(x) - img_size2[1]/2,
           y <= median(y) + img_size2[2]/2, y > median(y) - img_size2[2]/2) %>%
    #Flip y
    mutate(y = (max(y) - y) + 1)
  
  out_list <- list()
  out_list[["Img_scaled"]] <- img2
  
  return(out_list)

}

#2 Legoize - Convert image Lego colors -----
convert_to_lego_colors <- function(R, G, B){
  lego_colors %>% 
    mutate(dist = ((R_lego - R)^2 + (G_lego - G)^2 + (B_lego - B)^2)^(1/2)) %>% 
    top_n(-1, dist) %>% 
    mutate(Lego_color = rgb(R_lego, G_lego, B_lego)) %>% 
    select(Lego_name = Color, Lego_color)
}

legoize <- function(image_list){
  in_list <- image_list
  
  img <- in_list$Img_scaled %>% 
    mutate(lego = purrr::pmap(list(R, G, B), convert_to_lego_colors)) %>% 
    unnest(lego)
  
  in_list[["Img_lego"]] <- img
  
  return(in_list)
  
}

#3 collect_bricks - Combine bricks into larger ones ----
collect_bricks <- function(image_list, mosaic_type = "flat"){
  in_list <- image_list
  
  if(mosaic_type == "flat"){
    img <- in_list$Img_lego %>% 
      select(x, y, Lego_name, Lego_color) %>% 
      #4x2 bricks - horizontal
      group_by(xg = x %/% 4, yg = y %/% 2) %>% 
      mutate(g_1_x4y2_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 8,
                                 paste0("x4y2_", "x", min(x), "_y", min(y)), NA)) %>% 
      #4x2 bricks - vertical
      ungroup() %>% group_by(xg = x %/% 2, yg = y %/% 4) %>% 
      mutate(g_2_x2y4_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 8,
                                 paste0("x2y4_", "x", min(x), "_y", min(y)), NA)) %>% 
      #2x2 bricks
      ungroup() %>% group_by(xg = x %/% 2, yg = y %/% 2) %>% 
      mutate(g_5_x2y2_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 4,
                                 paste0("x2y2_", "x", min(x), "_y", min(y)), NA)) %>% 
      #4x1 bricks - horizontal
      ungroup() %>% group_by(xg = x %/% 4, yg = y ) %>% 
      mutate(g_7_x4y1_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 4,
                                 paste0("x4y1_", "x", min(x), "_y", min(y)), NA)) %>% 
      #4x1 bricks -  vertical
      ungroup() %>% group_by(xg = x, yg = y %/% 4) %>% 
      mutate(g_8_x1y4_1 = ifelse(length(unique(Lego_name)) == 1 & n() == 4,
                                 paste0("x1y4_", "x", min(x), "_y", min(y)), NA)) %>% 
      #3x1 bricks - horizontal
      ungroup() %>% group_by(xg = x %/% 3, yg = y ) %>% 
      mutate(g_7_x3y1_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 3,
                                 paste0("x3y1_", "x", min(x), "_y", min(y)), NA)) %>% 
      #3x1 bricks -  vertical
      ungroup() %>% group_by(xg = x, yg = y %/% 3) %>% 
      mutate(g_8_x1y3_1 = ifelse(length(unique(Lego_name)) == 1 & n() == 3,
                                 paste0("x1y3_", "x", min(x), "_y", min(y)), NA)) %>% 
      #2x1 bricks - horizontal
      ungroup() %>% group_by(xg = x %/% 2, yg = y ) %>% 
      mutate(g_9_x2y1_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 2,
                                 paste0("x2y1_", "x", min(x), "_y", min(y)), NA)) %>% 
      #2x1 bricks -  vertical
      ungroup() %>% group_by(xg = x, yg = y %/% 2) %>% 
      mutate(g_10_x1y2_1 = ifelse(length(unique(Lego_name)) == 1 & n() == 2,
                                  paste0("x1y2_", "x", min(x), "_y", min(y)), NA)) %>% 
      ungroup() %>% 
      #1x1
      mutate(g_11_x1y1_0 = paste0("x1y1_", "x", x, "_y", y)) %>% 
      select(-xg, -yg)
  }
  else if(mosaic_type == "stacked"){
    img <- in_list$Img_lego %>% 
      select(x, y, Lego_name, Lego_color) %>% 
      #4x1 bricks - horizontal
      ungroup() %>% group_by(xg = x %/% 4 + y %% 4, yg = y ) %>% 
      mutate(g_7_x4y1_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 4,
                                 paste0("x4y1_", "x", min(x), "_y", min(y)), NA)) %>% 
      #3x1 bricks - horizontal
      ungroup() %>% group_by(xg = x %/% 3 + y %% 3, yg = y ) %>% 
      mutate(g_7_x3y1_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 3,
                                 paste0("x3y1_", "x", min(x), "_y", min(y)), NA)) %>% 
      #2x1 bricks - horizontal
      ungroup() %>% group_by(xg = x %/% 2 + y %% 2, yg = y ) %>% 
      mutate(g_9_x2y1_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 2,
                                 paste0("x2y1_", "x", min(x), "_y", min(y)), NA)) %>% 
      ungroup() %>% 
      #1x1
      mutate(g_11_x1y1_0 = paste0("x1y1_", "x", x, "_y", y)) %>% 
      select(-xg, -yg)
  }
  
  img2 <- img %>% 
    gather(Brick, brick_id, dplyr::starts_with("g_")) %>% 
    #Only keep first Brick group has a name
    group_by(x, y) %>% 
    filter(Brick == Brick[min(which(!is.na(brick_id)))]) %>% 
    ungroup() %>% 
    # min/max coord for geom_rect()
    group_by(Brick, brick_id, Lego_color, Lego_name) %>% 
    summarise(xmin = min(x)-0.5, xmax = max(x)+0.5,
           ymin = min(y)-0.5, ymax = max(y)+0.5) %>% 
    ungroup()
  
  # This is very brute-force. Probably a much cleaner way to do this
  pcs <- img2 %>% 
    select(Brick, brick_id, Lego_name, Lego_color) %>% 
    distinct() %>% 
    separate(Brick, c("g", "gn", "size", "gi")) %>% 
    select(-dplyr::starts_with("g")) %>% 
    mutate(size1 = as.numeric(substr(size, 2, 2)), 
           size2 = as.numeric(substr(size, 4, 4))) %>% 
    mutate(Brick_size = ifelse(size1>size2, paste(size1, "x", size2), paste(size2, "x" , size1))) %>% 
    count(Brick_size, Lego_name, Lego_color) 
  
  in_list[["Img_bricks"]] <- img2
  in_list[["mosaic_type"]] <- mosaic_type
  in_list[["pieces"]] <- pcs
  
  return(in_list)
}

#3a display_set  - plot output of collect_bricks()
display_set <- function(image_list, title=NULL){
  in_list <- image_list
  image <- in_list$Img_bricks
  
  coord_x <- c(min(image$xmin)+0.5, max(image$xmax)-0.5)
  coord_y <- c(min(image$ymin)+0.5, max(image$ymax)-0.5)
  
  ggplot(image) +
    geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
                  fill = Lego_color), color = "#333333")+
    scale_fill_identity() +
    geom_point(data = expand.grid(x=coord_x[1]:coord_x[2], y=coord_y[1]:coord_y[2]),
               aes(x=x, y=y), color = "#333333", alpha = 0.2, shape = 1, size = 2) +
    coord_fixed(expand = FALSE) +
    labs(title = title) +
    theme_minimal() +
    theme_lego
} 

#4 Instructions ----
generate_instructions <- function(image_list, num_steps) {
  in_list <- image_list
  image <- in_list$Img_bricks
  
  rows_per_step <- ceiling((max(image$ymax)-0.5) / num_steps)
  
  create_steps <- function(a) {
    image %>% 
      group_by(brick_id) %>% 
      filter(min(ymin) <= a*rows_per_step+(min(image$ymin)+0.5)) %>% 
      ungroup() %>%
      mutate(Step = paste("Step", (if(a<10){paste0('0', a)}else{a})))
  }
  
  1:num_steps %>% 
    map(create_steps) %>% 
    bind_rows() %>% 
    ggplot() +
    geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
                  fill = Lego_color), color = "#333333")+
    scale_fill_identity() +
    coord_fixed(expand = FALSE) +
    facet_wrap(~Step) +
    theme_minimal()+
    theme_lego
}

#5 Piece count ----
#Print as data frame
table_pieces <- function(image_list){
  pcs <- image_list$pieces
  
  pcs %>% 
    select(-Lego_color) %>% 
    spread(Brick_size, n, fill = 0) %>% 
    rename(`LEGO Brick Color` = Lego_name)
}

#Print as image
display_pieces <- function(image_list){
  in_list <- image_list
  pcs <- in_list$pieces
  
  pcs_coords <- tibble(
    Brick_size = c("1 x 1", "2 x 1", "3 x 1", "4 x 1", "2 x 2", "4 x 2"),
    xmin = c(0, 0, 0, 0, 6, 6),
    xmax = c(1, 2, 3, 4, 8, 8),
    ymin = c(0, 2, 4, 6, 0, 3),
    ymax = c(1, 3, 5, 7, 2, 7)
  ) %>% 
    #This function creates nodes in each brick for stud placement
    mutate(studs = purrr::pmap(list(xmin, xmax, ymin, ymax), function(a, b, c, d){
      expand.grid(x=seq(a+0.5, b-0.5, by=1), 
                  y=seq(c+0.5, d-0.5, by=1))
    }))
  
  pcs2 <- pcs %>% 
    arrange(Lego_color) %>% 
    mutate(Lego_name = factor(Lego_name, 
                              levels = c("Black", 
                                         unique(Lego_name)[!(unique(Lego_name) %in% c("Black", "White"))],
                                         "White"))) %>% 
    left_join(pcs_coords, by = "Brick_size")
  
  pcs2 %>% 
    ggplot() +
    geom_rect(aes(xmin=xmin, xmax=xmax, ymin=-ymin, ymax=-ymax,
                  fill = Lego_color), color = "#333333")+
    scale_fill_identity() +
    geom_point(data = pcs2 %>% unnest(studs),
               aes(x=x, y=-y), color = "#333333", alpha = 0.2, 
               shape = 1, size = 2) +
    geom_text(aes(x = xmax + 0.25, y = -(ymin+ymax)/2, label = paste0("x", n)), 
              hjust = 0, vjust = 0.5, size = 3.5) +
    coord_fixed(xlim = c(-0.5, 10)) +
    facet_wrap(~Lego_name, ncol=5) +
    theme_minimal()+
    theme_lego +
    theme(
      panel.grid = element_blank()
    )
}
  
