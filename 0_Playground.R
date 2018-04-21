library(tidyverse); library(jpeg)

img.raw <- readJPEG("Images/anthonyryan.jpg")

img <- bind_rows(
  list(
    (as.data.frame(img.raw[, , 1]) %>% 
       mutate(y=row_number(), channel = "R") %>% 
       gather(x, value, -y, -channel) %>% 
       mutate(x = as.numeric(gsub("V", "", x)))),
    (as.data.frame(img.raw[, , 2]) %>% 
       mutate(y=row_number(), channel = "G") %>% 
       gather(x, value, -y, -channel) %>% 
       mutate(x = as.numeric(gsub("V", "", x)))),
    (as.data.frame(img.raw[, , 3]) %>% 
       mutate(y=row_number(), channel = "B") %>% 
       gather(x, value, -y, -channel) %>% 
       mutate(x = as.numeric(gsub("V", "", x))))
  )
) %>% 
  # mutate(value = value * 255) %>% 
  spread(channel, value)

#Goal is 48 px tall

max(img$x)
max(img$y)

img_height <- 48

img2 <- img %>% 
  mutate(y_scaled = (y - min(y))/(max(y)-min(y))*img_height + 1,
         x_scaled = (x - min(x))/(max(x)-min(x))*img_height + 1) %>% 
  group_by(y2 = ceiling(y_scaled), x2 = ceiling(x_scaled)) %>% 
  summarize_at(vars(R, G, B), funs(mean(.))) %>% 
  rowwise() %>% 
  mutate(color = rgb(R, G, B)) %>% 
  ungroup()

ggplot(img2 %>% filter(x2 <= 100), aes(x=x2, y=-y2, fill = color)) +
  geom_raster()+
  scale_fill_identity() +
  coord_fixed(expand = FALSE)

#Lego colors
lego_colors <- read_csv("Colors/Lego_Colors.csv")

lego_colors2 <- lego_colors %>% 
  filter(c_Palette2016, !c_Transparent, !c_Glow, !c_Metallic) %>% 
  mutate_at(vars(R, G, B), funs(./255)) %>% 
  rename(R_lego = R, G_lego = G, B_lego = B)%>% 
  mutate_at(vars(starts_with("w_")), funs(ifelse(is.na(.), 0, .)))

lego_colors3 <- lego_colors2 %>% 
  filter(t_Classic) 
  
convert_to_lego <- function(R, G, B){
  
  dat <- lego_colors2 %>% 
    mutate(dist = ((R_lego - R)^2 + (G_lego - G)^2 + (B_lego - B)^2)^(1/2)) %>% 
    # mutate(dist = dist^w_Classic) %>% 
    filter(dist == min(dist)) %>% 
    top_n(1, dist)
  
  return(data.frame(Lego_name = dat$Color, 
              Lego_color = rgb(dat$R_lego, dat$G_lego, dat$B_lego),
              stringsAsFactors = F))
  
}

l_img2 <- img2 %>% 
  # filter(x2 <= 100) %>% 
  mutate(lego = purrr::pmap(list(R, G, B), convert_to_lego)) %>% 
  unnest(lego)

ggplot(l_img2, aes(x=x2, y=-y2, fill = Lego_color)) +
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
  # #4x2 bricks - horizontal offset
  # group_by(xg = (x+2) %/% 4, yg = (y+1) %/% 2) %>% 
  # mutate(g_3_x4y2_1 = ifelse(length(unique(Lego_name)) == 1 & n() == 8,
  #                            paste0("x4y2_", "x", min(x), "_y", min(y)), NA)) %>% 
  # ungroup() %>% 
  # #4x2 bricks - vertical offset
  # group_by(xg = (x+1) %/% 2, yg = (y+2) %/% 4) %>% 
  # mutate(g_4_x2y4_1 = ifelse(length(unique(Lego_name)) == 1 & n() == 8,
  #                            paste0("x2y4_", "x", min(x), "_y", min(y)), NA)) %>% 
  # ungroup() %>% 
  #2x2 bricks
  group_by(xg = x %/% 2, yg = y %/% 2) %>% 
  mutate(g_5_x2y2_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 4,
                             paste0("x2y2_", "x", min(x), "_y", min(y)), NA)) %>% 
  ungroup() %>% 
  # #2x2 bricks -  offset
  # group_by(xg = (x+1) %/% 2, yg = (y+1) %/% 2) %>% 
  # mutate(g_6_x2y2_1 = ifelse(length(unique(Lego_name)) == 1 & n() == 4,
  #                            paste0("x2y2_", "x", min(x), "_y", min(y)), NA)) %>% 
  # ungroup() %>% 
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
  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=-ymin, ymax=-ymax,
                fill = Lego_color), color = "#333333")+
  scale_fill_identity() +
  geom_point(data = l_img2, aes(x=x2, y=-y2),
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

sum(pieces$n)
