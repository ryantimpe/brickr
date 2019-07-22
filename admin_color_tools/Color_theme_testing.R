
library(tidyverse)
library(brickr)

dat <- iris %>% 
  group_by(Species) %>% 
  summarize_if(is.numeric, mean) %>% 
  gather(Series, avg, -Species)

# dat$avg <- dat$avg * sample(c(-1, 1), nrow(dat), replace=T)

theme_list_for_loop <- unique(brickr_themes$theme)

# themesss <- "hp"

for(i in seq_along(theme_list_for_loop)){
  
  title = theme_list_for_loop[i]
  subtitle <- paste0("brickr::scale_fill_brick( brick_theme = '", theme_list_for_loop[i], "')")
  
  p <- ggplot(dat, aes(x=Species, y = avg)) +
    geom_brick_col(aes(fill = Series), two_knob = F, split_bricks = T,
                   label = "brickr", min_radius_for_text = 0 )  +
    labs(title = title, caption = subtitle)
  
  if(i <= 8){
    p <- p +
      scale_fill_brick(theme_list_for_loop[i]) +
      coord_brick() +
      theme_brick(theme_list_for_loop[i]) 
  } else {
    p <- p +
      scale_fill_brick(theme_list_for_loop[i]) +
      coord_brick_flip() +
      theme_brick(theme_list_for_loop[i]) +
      theme(legend.position = "bottom")
  }
    
  
  ggsave(paste0("admin_color_tools/demos/img", i+1, ".png"), plot = p, device = "png", width = 7, height = 5)
}
# 
# movie_input <- list.files(pattern = "demo")
# av::av_encode_video(movie_input, output = "brickrdemo.mp4", framerate = 1,
#                 vfilter = "null", codec = NULL, audio = NULL, verbose = TRUE)


