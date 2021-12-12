library(tidyverse)
library(igraph)

file <- "./Day_12/Part_1/input.txt"
input <- readLines(file)

network_data <- 
  data.frame(link = input) %>% 
  separate(link, into = c("beg","end"), sep = "-") 

## Fake multiple capitals
beginning <- 
  network_data %>% 
  mutate(is_upper = (beg == toupper(beg))) %>%
  filter(is_upper)

beginning <- 
  bind_rows(beginning,
           beginning %>% group_by(beg) %>% slice(1)) %>% 
  group_by(beg) %>% 
  mutate(new_beg = paste(beg, row_number(), sep="__")) %>% 
  select(beg, new_beg)
  
ending <- 
  network_data %>% 
  mutate(is_upper = (end == toupper(end))) %>%
  filter(is_upper)

ending <- 
  bind_rows(ending,
            ending %>% group_by(end) %>% slice(1)) %>% 
  group_by(end) %>% 
  mutate(new_end = paste(end, row_number(), sep="__")) %>% 
  select(end, new_end)

network_data_full <- 
  network_data %>% 
  left_join(beginning) %>% 
  mutate(beg = if_else(is.na(new_beg), beg, new_beg)) %>% 
  select(-new_beg) %>% 
  left_join(ending) %>% 
  mutate(end = if_else(is.na(new_end), end, new_end)) %>% 
  select(-new_end) %>% 
  
  graph_from_data_frame(directed = FALSE)

paths <- 
  network_data_full %>% 
  igraph::all_simple_paths(from="start", 
                           to = "end", mode = "all") 

long_vec_paths <- sapply(paths, as_ids) %>% unlist

paths <- 
  data.frame(node = long_vec_paths) %>% 
  mutate(path_id = cumsum(node == "start")) %>% 
  separate(node, into = c("node_real", "node_id"), sep="__") %>% 
  mutate(is_upper = (node_real == toupper(node_real))) %>% 
  group_by(path_id) %>% 
  summarise(path = paste0(node_real, collapse= ",")) %>% 
  distinct(path)

paths %>% 
  nrow()
