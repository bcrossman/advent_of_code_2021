library(tidyverse)
directions <- read_tsv("./Day_2/Part_1/input.txt", col_names = c("instruction"))

##Part 1

directions %>% 
  separate(instruction, into = c("direction","units"), 
           sep = " ", remove = F, convert = T) %>% 
  mutate(units = if_else(direction == "up", -units, units),
         direction = if_else(direction == "up", "down", direction)) %>%
  group_by(direction) %>% 
  summarise(units = sum(units)) %>% 
  pull(units) %>% 
  prod()
   

##Part 2

directions %>% 
  separate(instruction, into = c("direction","units"), 
           sep = " ", remove = F, convert = T) %>% 
  mutate(order = as.numeric(cumsum(direction=="forward"))) %>% 
  mutate(units = if_else(direction == "up", -units, units),
         direction = if_else(direction == "up", "down", direction)) %>% 
  mutate(order = if_else(direction == "down", order+1, order)) %>%
  group_by(order, direction) %>% 
  summarise(units = sum(units)) %>% 
  pivot_wider(names_from = direction, values_from = units, values_fill = 0) %>%
  ungroup() %>% 
  mutate(aim = cumsum(down)) %>% 
  mutate(depth = forward*aim) %>% 
  summarise_all(sum) %>% 
  mutate(prod = forward*depth) %>% 
  pull(prod)
  