#Create data of colors for the package

library(tidyverse)

rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255)

names(brickr::lego_colors)

color_df <- readxl::read_xlsx("Colors/Colors_Master.xlsx")
names(color_df)

lego_colors <- color_df %>% 
  select(-Sample) %>% 
  rename_at(vars("R", "G", "B"), list(~paste0(., "_lego"))) %>% 
  filter(c_Palette2016, !c_Metallic, !c_Transparent, !c_Glow) %>% 
  select(-starts_with("c_")) %>% 
  mutate(Palette = factor(Palette, levels = c("Universal", "Generic", "Special")),
         hex = rgb2hex(R_lego, G_lego, B_lego)) %>% 
  arrange(Palette, LEGONo) %>% 
  mutate(brickrID = row_number()) %>% 
  select(brickrID, Color, LEGONo, Palette, everything())

usethis::use_data(lego_colors, overwrite = T)
