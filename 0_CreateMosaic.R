#####
# Generate LEGO mosaic an image
#####

source("0_Functions.R")

#This will take a few minutes to run
lego_mosaic <- readJPEG("Images/goldengirls.jpg") %>% 
  scale_image(48) %>% 
  legoize() %>% 
  collect_bricks() 

lego_mosaic %>% display_set("Golden Girls!")

#Instruction manual - calculate and graph
lego_mosaic %>% 
  generate_instructions(6) 
  

#Get summary of pieces
pieces <- lego_mosaic %>% table_pieces()
