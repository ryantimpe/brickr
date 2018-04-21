library(tidyverse); library(jpeg)

img.raw <- readJPEG("Images/goldengirls.jpg")

#Theme
theme_lego <- theme(panel.background = element_rect(fill = "#cccccc"),
        strip.background = element_rect(fill = "#00436b"),
        strip.text = element_text(color = "#ffffff", face = "bold"),
        axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

#SCALE IMAGE ----

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
  
  #Rescale the image
  img2 <- img %>% 
    mutate(y_scaled = (y - min(y))/(max(y)-min(y))*img_size*img_scale_y + 1,
           x_scaled = (x - min(x))/(max(x)-min(x))*img_size*img_scale_x + 1) %>% 
    select(-x, -y) %>% 
    group_by(y = ceiling(y_scaled), x = ceiling(x_scaled)) %>% 
    #Get average R, G, B and convert it to hexcolor
    summarize_at(vars(R, G, B), funs(mean(.))) %>% 
    rowwise() %>% 
    mutate(color = rgb(R, G, B)) %>% 
    ungroup() %>% 
    #Center the image
    filter(x <= median(x) + img_size/2, x >= median(x) - img_size/2,
           y <= median(y) + img_size/2, y >= median(y) - img_size/2) %>%
    #Flip y
    mutate(y = max(y) - y + 1)
  
  return(img2)

}

img2 <- readJPEG("Images/goldengirls.jpg") %>% scale_image(48)

ggplot(img2, aes(x=x, y=y, fill = color)) +
  geom_raster()+
  scale_fill_identity() +
  coord_fixed(expand = FALSE) +
  theme_minimal() +
  theme_lego

#Lego colors -----
lego_colors <- read_csv("Colors/Lego_Colors.csv")

lego_colors <- lego_colors %>% 
  filter(c_Palette2016, !c_Transparent, !c_Glow, !c_Metallic) %>% 
  mutate_at(vars(R, G, B), funs(./255)) %>% 
  rename(R_lego = R, G_lego = G, B_lego = B)%>% 
  mutate_at(vars(starts_with("w_")), funs(ifelse(is.na(.), 0, .)))

convert_to_lego_colors <- function(R, G, B){
  dat <- lego_colors %>% 
    mutate(dist = ((R_lego - R)^2 + (G_lego - G)^2 + (B_lego - B)^2)^(1/2)) %>% 
    top_n(-1, dist) %>% 
    rename(Lego_name = Color) %>% 
    mutate(Lego_color = rgb(R_lego, G_lego, B_lego))
  
  return(dat %>% select(Lego_name, Lego_color))
}

legoize <- function(image){
  image %>% 
    mutate(lego = purrr::pmap(list(R, G, B), convert_to_lego_colors)) %>% 
    unnest(lego)
}
  
l_img2 <- legoize(img2)


ggplot(l_img2, aes(x=x, y=y, fill = Lego_color)) +
  geom_tile(width = 0.9, height = 0.9)+
  scale_fill_identity() +
  geom_point(color = "#333333", alpha = 0.2, shape = 1, size = 2.5) +
  coord_fixed(expand = FALSE) +
  theme_minimal()+
  theme(panel.background = element_rect(fill = "#999999"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())


#Combine bricks into larger ones
#Give each theoretical brick a unique ID

l_img3 <- l_img2 %>% 
  select(x=x2, y=y2, Lego_name, Lego_color) %>% 
  #4x2 bricks - horizontal
  group_by(xg = x %/% 4, yg = y %/% 2) %>% 
  mutate(g_1_x4y2_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 8,
                      paste0("x4y2_", "x", min(x), "_y", min(y)), NA)) %>% 
  ungroup() %>% 
  #4x2 bricks - vertical
  group_by(xg = x %/% 2, yg = y %/% 4) %>% 
  mutate(g_2_x2y4_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 8,
                             paste0("x2y4_", "x", min(x), "_y", min(y)), NA)) %>% 
  ungroup() %>% 
  #2x2 bricks
  group_by(xg = x %/% 2, yg = y %/% 2) %>% 
  mutate(g_5_x2y2_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 4,
                             paste0("x2y2_", "x", min(x), "_y", min(y)), NA)) %>% 
  ungroup() %>% 
  #4x1 bricks - horizontal
  group_by(xg = x %/% 4, yg = y ) %>% 
  mutate(g_7_x4y1_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 4,
                             paste0("x4y1_", "x", min(x), "_y", min(y)), NA)) %>% 
  ungroup() %>% 
  #4x1 bricks -  vertical
  group_by(xg = x, yg = y %/% 4) %>% 
  mutate(g_8_x1y4_1 = ifelse(length(unique(Lego_name)) == 1 & n() == 4,
                             paste0("x1y4_", "x", min(x), "_y", min(y)), NA)) %>% 
  ungroup() %>% 
  #3x1 bricks - horizontal
  group_by(xg = x %/% 3, yg = y ) %>% 
  mutate(g_7_x3y1_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 3,
                             paste0("x3y1_", "x", min(x), "_y", min(y)), NA)) %>% 
  ungroup() %>% 
  #3x1 bricks -  vertical
  group_by(xg = x, yg = y %/% 3) %>% 
  mutate(g_8_x1y3_1 = ifelse(length(unique(Lego_name)) == 1 & n() == 3,
                             paste0("x1y3_", "x", min(x), "_y", min(y)), NA)) %>% 
  ungroup() %>% 
  #2x1 bricks - horizontal
  group_by(xg = x %/% 2, yg = y ) %>% 
  mutate(g_9_x2y1_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 2,
                             paste0("x2y1_", "x", min(x), "_y", min(y)), NA)) %>% 
  ungroup() %>% 
  #2x1 bricks -  vertical
  group_by(xg = x, yg = y %/% 2) %>% 
  mutate(g_10_x1y2_1 = ifelse(length(unique(Lego_name)) == 1 & n() == 2,
                              paste0("x1y2_", "x", min(x), "_y", min(y)), NA)) %>% 
  ungroup() %>% 
  #1x1
  mutate(g_11_x1y1_0 = paste0("x1y1_", "x", x, "_y", y)) %>% 
  select(-xg, -yg)

l_img4 <- l_img3 %>% 
  gather(Brick, brick_id, dplyr::starts_with("g_")) %>% 
  #Only keep first Brick group that is true...biggest ones are first!
  group_by(x, y) %>% 
  filter(Brick == Brick[min(which(!is.na(brick_id)))]) %>% 
  ungroup()

l_img5 <- l_img4 %>% 
  group_by(brick_id) %>% 
  mutate(brick_size = n()) %>% #HMMMnot perfect. some overlap with assignments
  ungroup() %>% 
  group_by(brick_id) %>% 
  mutate(xmin = min(x)-0.5, xmax = max(x)+0.5,
         ymin = min(y)-0.5, ymax = max(y)+0.5) %>% 
  ungroup() %>% 
  select(Brick, brick_id, xmin, xmax, ymin, ymax, Lego_color, Lego_name)

ggplot(l_img5) +
  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
                fill = Lego_color), color = "#333333")+
  scale_fill_identity() +
  geom_point(data = l_img2, aes(x=x2, y=y2),
             color = "#666666", alpha = 0.2, shape = 1, size = 3) +
  coord_fixed(expand = FALSE) +
  theme_minimal()+
  theme(panel.background = element_rect(fill = "#999999"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

#Piece counts!
pieces <- l_img5 %>% 
  select(Brick, brick_id, Lego_name, Lego_color) %>% 
  distinct() %>% 
  separate(Brick, c("g", "gn", "size", "gi")) %>% 
  select(-g, -gn, -gi) %>% 
  mutate(size1 = as.numeric(substr(size, 2, 2)), 
         size2 = as.numeric(substr(size, 4, 4))) %>% 
  mutate(Brick_size = ifelse(size1>size1, paste(size1, "x", size2), paste(size2, "x" , size1))) %>% 
  count(Brick_size, Lego_name) %>% 
  arrange(desc(Brick_size), desc(n))

pieces %>% 
  spread(Brick_size, n, fill = "")

sum(pieces$n)

#Instructions
num_steps <- 6
rows_per_step <- ceiling(img_size / num_steps)

create_steps <- function(a) {
  l_img5 %>% 
    group_by(brick_id) %>% 
    filter(min(ymin) <= a*rows_per_step+(min(l_img4$y))) %>% 
    ungroup() %>%
    mutate(Step = paste("Step", (if(a<10){paste0('0', a)}else{a})))
}

plot_instructions <- 1:num_steps %>% 
  map(create_steps) %>% 
  bind_rows()


ggplot(plot_instructions) +
  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
                fill = Lego_color), color = "#333333")+
  scale_fill_identity() +
  coord_fixed(expand = FALSE) +
  facet_wrap(~Step) +
  theme_minimal()+
  theme(panel.background = element_rect(fill = "#cccccc"),
        strip.background = element_rect(fill = "#00436b"),
        strip.text = element_text(color = "#ffffff", face = "bold"),
        axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

ggplot(plot_instructions) +
  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
                fill = Lego_color), color = "#333333")+
  scale_fill_identity() +
  coord_fixed(expand = FALSE) +
  facet_wrap(~Step, ncol = 6) +
  theme_minimal()+
  theme(panel.background = element_rect(fill = "#7EC0EE"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

