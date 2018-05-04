#####
# Generate LEGO mosaic an image
#####

source("0_Functions.R")

#This will take a few minutes to run
lego_mosaic <- readJPEG("Images/goldengirls2.jpg") %>% 
  scale_image(c(56, 48)) %>% #Single value for square, c(W, H) for rectangle
  legoize() %>% 
  collect_bricks() 

lego_mosaic %>% display_set("Golden Girls!")

#Instruction manual - calculate and graph
lego_mosaic %>% 
  generate_instructions(6) 
  
#Get summary of pieces
pieces <- lego_mosaic %>% table_pieces()

lego_mosaic %>% display_pieces()
