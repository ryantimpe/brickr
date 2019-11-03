#Create data of colors for the package

library(tidyverse)

rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255)

names(brickr::lego_colors)

color_df <- readxl::read_xlsx("admin_color_tools/Colors_Master.xlsx")
names(color_df)

lego_colors <- color_df %>% 
  select(-Sample) %>% 
  rename_at(vars("R", "G", "B"), list(~paste0(., "_lego"))) %>% 
  rename(Trans_lego = c_Transparent) %>% 
  filter(c_Palette2016, !c_Metallic, !c_Glow) %>% 
  select(-starts_with("c_")) %>% 
  mutate(Palette = factor(Palette, levels = c("Universal", "Generic", "Special")),
         hex = rgb2hex(R_lego, G_lego, B_lego)) %>% 
  mutate_at(vars(R_lego, G_lego, B_lego), list(~./255)) %>% 
  arrange(Trans_lego, Palette, LEGONo) %>% 
  mutate(brickrID = row_number()) %>% 
  select(brickrID, Color, LEGONo, Palette, everything()) %>% 
  #Calculate brightness of color
  # https://stackoverflow.com/questions/596216/formula-to-determine-brightness-of-rgb-color
  mutate(lum = 0.299*R_lego + 0.587*G_lego + 0.114*B_lego)

usethis::use_data(lego_colors, overwrite = T)
