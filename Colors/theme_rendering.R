library(tidyverse)

raw <- readxl::read_xlsx("Colors/color_themes.xlsx")


themes <- raw[, 1:(which(grepl("..", names(raw), fixed = T))[1]-1)]

color_key <- raw[, (which(grepl("..", names(raw), fixed = T))[2]+1):ncol(raw)]


brickr_themes <- themes %>% 
  filter(!is.na(TYPE)) %>% 
  mutate(index = row_number() - 2) %>% 
  gather(theme, user_color, -TYPE, -index) %>% 
  filter(!is.na(user_color)) %>% 
  inner_join(color_key %>% select(user_color, hex)) %>% 
  select(-user_color)

usethis::use_data(brickr_themes, overwrite = T)
