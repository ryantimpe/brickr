###
# Development of bricks_from_data()
###
library(brickr)
dat_table <- PlantGrowth %>% 
  dplyr::group_by(group) %>%
  dplyr::summarize_at(dplyr::vars(weight), mean) %>% 
  dplyr::ungroup()

dat_table <- Titanic %>% 
  as.data.frame() %>% 
  dplyr::count(Class, wt = Freq)



bricks_from_data <- function(dat_table = dat_table, x, y, color,
                             color_scale = lego_colors$Color[-1:-4],
                             brick_value = 1){
  xvar <- rlang::enquo(x)
  yvar <- rlang::enquo(y)
  colorvar <- rlang::enquo(color)

  dat_prep <- dat_table %>% 
    #If X is a factor, convert to numeric. If not, convert to factor, then numeric
    dplyr::mutate(.value_x = as.numeric(as.factor(!!xvar))) %>% 
    dplyr::mutate(.value_z = round(!!yvar / brick_value),
                  .value_color1 = as.numeric(as.factor(!!colorvar))) %>% 
    dplyr::do(if(color_scale[1] == "identity"){
      dplyr::mutate(., .value_color = !!colorvar)
    } else {
      dplyr::mutate(., .value_color = color_scale[.value_color1])
    })
     
  
  dat_bricks <- dat_prep %>%
    dplyr::mutate(x = .value_x * 6 + 1,
                  z = .value_z,
                  Color = .value_color) %>%
    #Duplicate over x... bars are 4-studs wide
    dplyr::bind_rows(list(
      dplyr::mutate(., x = x+1),
      dplyr::mutate(., x = x+2),
      dplyr::mutate(., x = x+3))) %>%
    #Duplicate over y
    dplyr::mutate(y = 3) %>%
    dplyr::bind_rows(dplyr::mutate(., y = 4))

  #Duplicate over Z
  dat_bricks2 <- 1:max(dat_bricks$z) %>%
    purrr::map_df(~dplyr::filter(dat_bricks, z >= .x) %>% dplyr::mutate(z=.x))

  bricks <- bricks_from_coords(dat_bricks2)
  return(bricks)
}


dat_table %>% 
  bricks_from_data(x = Class, y = n, color = Class, brick_value = 50) %>% 
  display_bricks()
