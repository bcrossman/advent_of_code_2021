library(tidyverse)
library(unglue)
library(gifski)
library(gganimate)

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

gganimate_list <- list()

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
  gganimate_list[[as.character(i)]] <- dots
}

gganimate_dots <- 
  bind_rows(gganimate_list, .id = "id") %>% 
  mutate(id = as.numeric(id))

gif <- 
  gganimate_dots %>% 
  # filter(id == max(id)) %>% 
  ggplot(aes(x = x_coord, y = -y_coord)) + 
  geom_tile() +  
  coord_fixed() +
  transition_states(id, transition_length = 20, 
                    state_length = c(rep(1, 
                                         length(unique(gganimate_dots$id))-1),
                                     99))+
  view_follow(aspect_ratio = 1)

gif

anim_save("aoc_day_13.gif")

gganimate_dots %>% 
  # filter(id == max(id)) %>% 
  ggplot(aes(x = x_coord, y = -y_coord)) + 
  geom_tile() +  
  coord_fixed() +
  transition_manual(id)+
  view_follow(aspect_ratio = 1)
