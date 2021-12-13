library(tidyverse)
library(unglue)

file <- "./Day_13/Part_1/input.txt"
input <- readLines(file)

lines <-
  unglue_data(input, c("{x_coord},{y_coord}",
                       "fold along {fold_direction}={fold_value}"), convert = TRUE)

dots <- 
  lines %>% 
  filter(!is.na(x_coord)) %>% 
  select(x_coord,y_coord) %>% 
  mutate_all(as.numeric)

directions <- 
  lines %>% 
  filter(!is.na(fold_direction)) %>% 
  select(fold_direction,fold_value) %>% 
  mutate(fold_value = as.numeric(fold_value))

for(i in 1:nrow(directions)){
  
  if(directions$fold_direction[i]=="x"){
    
    fold_value <- directions$fold_value[i]
    
    dots <- 
      dots %>% 
      mutate(x_coord = if_else(x_coord>fold_value, 
                               x_coord - 2*(x_coord-fold_value), 
                               as.numeric(x_coord)))
  }
  
  if(directions$fold_direction[i]=="y"){
    
    fold_value <- directions$fold_value[i]
    
    dots <- 
      dots %>% 
      mutate(y_coord = if_else(y_coord>fold_value, 
                               y_coord - 2*(y_coord-fold_value), 
                               as.numeric(y_coord)))
  }
  
  dots <- 
    dots %>% 
    count(x_coord, y_coord) %>% 
    select(-n)
  
  if(i == 1){
    print("Part 1")
    print(nrow(dots))
  }
}

  ggplot(data = dots, aes(x = x_coord, y = -y_coord)) + 
  geom_point()
