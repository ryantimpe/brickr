library(dplyr)
library(readr)

lego_colors <- readr::read_csv("data-raw/Lego_Colors.csv", 
                               col_types = cols(
                                 LEGONo = col_double(),
                                 Color = col_character(),
                                 Sample = col_logical(),
                                 R = col_double(),
                                 G = col_double(),
                                 B = col_double(),
                                 c_Palette2016 = col_logical(),
                                 c_Transparent = col_logical(),
                                 c_Glow = col_logical(),
                                 c_Metallic = col_logical(),
                                 t_BW = col_logical(),
                                 t_Classic = col_logical(),
                                 t_Friends = col_logical(),
                                 w_weight = col_double(),
                                 w_Classic = col_double()
                               ))

lego_colors <- lego_colors %>%
  filter(c_Palette2016, !c_Transparent, !c_Glow, !c_Metallic) %>% 
  mutate(hex_code = purrr::pmap_chr(
    list(red = R, green = G, blue = B), 
    function(red, green, blue) { grDevices::rgb(red, green, blue, maxColorValue = 255) }
  )
  ) %>%
  mutate_at(vars(R, G, B), list(~ ./255)) %>% 
  rename(R_lego = R, G_lego = G, B_lego = B) %>% 
  mutate_at(vars(starts_with("w_")), list(~ ifelse(is.na(.), 0, .)))

usethis::use_data(lego_colors, overwrite = TRUE)
