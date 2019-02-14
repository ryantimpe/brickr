#####
# Generate LEGO mosaic an image
#####

source("0_Functions.R")

#This will take a few minutes to run
lego_mosaic <- readJPEG("Images/Ryan.jpg") %>% 
  # scale_image(48) %>% #Single value for square,
  # scale_image(c(56, 48)) %>% # c(W, H) for rectangle
  scale_image(c(48, 56)) %>% # c(W, H) for rectangle
  legoize() %>% 
  collect_bricks() 

lego_mosaic %>% display_set()

#Instruction manual - calculate and graph
lego_mosaic %>% 
  generate_instructions(6) 
  
#Get summary of pieces
pieces <- lego_mosaic %>% table_pieces()

lego_mosaic %>% display_pieces()

ggsave("Emmet.png", device = "png", height = 5, width = 5)

#3D
library(rayshader)

lego_mosaic_3d <- lego_mosaic %>% 
  collect_3d(mosaic_height = 9, highest_el = "dark") 

lego_mosaic_3d$`threed_hillshade`%>%
  plot_3d(lego_mosaic_3d$`threed_elevation`, zscale=0.125,fov=0,theta=-30,phi=30,windowsize=c(1000,800),zoom=0.75
  )

#mosaic_height is the elevation of the mosaic in LEGO plates... 3 plates =  1 LEGO brick
#Set highest_el = "dark" for dark bricks to be tallest... otherwise light bricks are tallest
lego_mosaic %>% 
  collect_3d(mosaic_height = 9, highest_el = "dark") %>% 
  display_3d(zscale=0.125,fov=0,theta=-30,phi=30,windowsize=c(1000,800),zoom=0.75)

#display_3d() takes all inputs of rayshader::plot_3d()
