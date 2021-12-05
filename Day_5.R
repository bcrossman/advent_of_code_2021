library(tidyverse)
library(unglue)

file <- "./Day_5/Part_1/input.txt"
input <- readLines(file)

# part 1
lines <-
  unglue_data(input, "{start_x},{start_y} -> {end_x},{end_y}", convert = TRUE)

max_value <- max(lines)

grid <- 
  crossing(lines, 
           data.frame("x_point" = 0:max_value),
           data.frame("y_point" = 0:max_value))

grid %>% 
  filter(start_x==end_x | start_y==end_y) %>%   ##Keep only vert/horiz
  mutate(in_line = x_point>=pmin(start_x, end_x)&
           x_point<=pmax(start_x, end_x)&
           y_point>=pmin(start_y, end_y)&
           y_point<=pmax(start_y, end_y)) %>% 
  filter(in_line) %>% 
  group_by(x_point, y_point) %>% 
  summarise(crosses = sum(in_line)>1) %>% 
  ungroup() %>% 
  pull(crosses) %>% 
  sum()


## Part 2
grid %>% 
  mutate(line_slope = (end_y-start_y)/(end_x-start_x),
         point_slope = (y_point - start_y)/(x_point-start_x)) %>% 
  mutate(in_line = x_point>=pmin(start_x, end_x)& #in box
           x_point<=pmax(start_x, end_x)& #in box
           y_point>=pmin(start_y, end_y)& #in box
           y_point<=pmax(start_y, end_y)& #in box
           (is.nan(point_slope)|(line_slope==point_slope))) %>%  #in diagonal
  filter(in_line) %>% 
  group_by(x_point, y_point) %>% 
  summarise(crosses = sum(in_line)>1) %>% 
  ungroup() %>% 
  pull(crosses) %>% 
  sum()
