library(tidyverse)

raw <- readxl::read_xlsx("Colors/color_themes.xlsx")

themes <- raw[, 1:(which(grepl("..", names(raw), fixed = T))[1]-1)]

color_key <- brickr::lego_colors 

brickr_themes <- themes %>% 
  filter(!is.na(TYPE)) %>% 
  mutate(index = row_number() - 2) %>% 
  gather(theme, brickrID, -TYPE, -index) %>% 
  filter(!is.na(brickrID)) %>% 
  inner_join(color_key %>% select(brickrID, hex)) %>% 
  select(-brickrID)

usethis::use_data(brickr_themes, overwrite = T)
