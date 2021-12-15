library(tidyverse)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(igraph)

file <- "./Day_15/Part_1/input.txt"
input <- readLines(file)

data <- 
  data.frame(risk = input) %>% 
  rowid_to_column() %>%
  separate_rows(risk, sep="", convert = T) %>% 
  drop_na(risk) %>% 
  filter(risk != "") %>% 
  group_by(rowid) %>% 
  mutate(colid = row_number()) %>% 
  ungroup() %>% 
  mutate(key = paste(rowid, colid, sep=",")) %>% 
  mutate(risk = as.numeric(risk))

data_list <- list()

for(to_left in 0:4){
  for(to_down in 0:4){
    
    block_data <- 
      data %>%
      mutate(rowid = rowid+to_down*100,
             colid = colid+to_left*100) %>% 
      mutate(risk = risk + to_down + to_left) %>% 
      mutate(key = paste(rowid, colid, sep=",")) %>% 
      mutate(risk = if_else((risk-9)>0,(risk-9),risk))
    
    data_list[[paste(to_left, to_down)]] <- block_data
  }
}

data <- bind_rows(data_list)
nrow(data) == length(unique(data$key))

##  Join edges

edges_points <- 
  data %>% 
  left_join(data %>% mutate(rowid = rowid-1),
            by = c("rowid","colid"),
            suffix = c("", "_down")) %>% 
  left_join(data %>% mutate(rowid = rowid+1),
            by = c("rowid","colid"),
            suffix = c("", "_up")) %>% 
  left_join(data %>% mutate(colid = colid+1),
            by = c("rowid","colid"),
            suffix = c("", "_left")) %>% 
  left_join(data %>% mutate(colid = colid-1),
            by = c("rowid","colid"),
            suffix = c("", "_right")) %>% 
  rename(key_start= key,
         risk_start = risk) %>% 
  pivot_longer(cols = -c(rowid,colid),
               names_to = c(".value","direction"),
               names_sep = "_") %>% 
  filter(direction != "start") %>% 
  rename(end = key,
         weight = risk) %>%
  mutate(start = paste(rowid, colid, sep=",")) %>% 
  select(start, end, weight) %>% 
  drop_na(weight)
  
network_data_full <- 
  edges_points %>% 
  graph_from_data_frame(directed = TRUE)

paths <- 
  network_data_full %>% 
  igraph::get.shortest.paths(from="1,1", 
                           to = "500,500",  
                         weights = NULL,  #uses weight attribute from df
                         mode = "out") 

long_vec_paths <- sapply(paths$vpath, as_ids) %>% unlist

data %>% 
  filter(key %in% long_vec_paths) %>% 
  filter(key !="1,1") %>% 
  pull(risk) %>% 
  sum()


## This also gives total cost, needed to name the inputs to distances to get it to work.
## Figured out from @_riinu_
network_data_full %>% 
  distances(v = "1,1", to ="500,500", mode = "out") 
