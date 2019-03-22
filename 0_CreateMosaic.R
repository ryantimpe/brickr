#####
# Generate LEGO mosaic an image
#####

#Packages Required: dplyr, tidyr, purrrr, ggplot2, readr

#RUN THIS CODE AFTER DOWNLOADING SOURCE FILES
library(brickr)

#This will take a few seconds to run
lego_mosaic <- jpeg::readJPEG("Images/goldengirls.jpg") %>% 
  image_to_bricks(48)

lego_mosaic %>% display_set()

#Instruction manual - calculate and graph
lego_mosaic %>% 
  generate_instructions(6) 
  
#Get summary of pieces
pieces <- lego_mosaic %>% table_pieces()

sum(lego_mosaic$pieces$n)

lego_mosaic %>% display_pieces()

#Save it
ggplot2::ggsave("GG_fewer_pieces.png", device = "png", height = 8, width = 8)

#3D with rayshader ----
library(rayshader)

#mosaic_height is the elevation of the mosaic in LEGO plates... 3 plates =  1 LEGO brick
#Set highest_el = "dark" for dark bricks to be tallest... otherwise light bricks are tallest
lego_mosaic %>% 
  collect_3d(mosaic_height = 6) %>% 
  display_3d(fov=0,theta=0,phi=90,windowsize=c(1000,800),zoom=0.75,shadow=FALSE)


#display_3d() takes all inputs of rayshader::plot_3d() EXCEPT hillshade, heightmap, & zscale
# If you want to use plot_3d() instead, use the items 'threed_hillshade' and 'threed_elevation' in the collect_3d() output
