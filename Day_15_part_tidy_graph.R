library(tidyverse)
library(tidygraph)
library(ggraph)

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
  rename(to = key,
         weight = risk) %>%
  mutate(from = paste(rowid, colid, sep=",")) %>% 
  select(from, to, weight) %>% 
  drop_na(weight)

network_data_full <- 
  edges_points %>% 
  as_tbl_graph()

thm <- theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
  ) 

theme_set(thm)

graph_routes <- 
  network_data_full %>%
  activate(nodes) %>%
  mutate(
    title = str_to_title(name),
    label = str_replace_all(title, " ", "\n")
  )

stations <- 
  graph_routes %>%
  activate(nodes) %>%
  pull(title)

from <- which(stations == "1,1")
to <-  which(stations == "500,500")

shortest <- 
  graph_routes %>%
  morph(to_shortest_path, from, to, weights = weight)

shortest <- shortest %>%
  mutate(selected_node = TRUE) %>%
  activate(edges) %>%
  mutate(selected_edge = TRUE) %>%
  unmorph() 

shortest <- shortest %>%
  activate(nodes) %>%
  mutate(selected_node = ifelse(is.na(selected_node), 1, 2)) %>%
  activate(edges) %>%
  mutate(selected_edge = ifelse(is.na(selected_edge), 1, 2)) %>%
  arrange(selected_edge)

shortest %>%
  ggraph(layout = "matrix") +
  geom_edge_diagonal(aes(alpha = selected_edge), color = "gray") +
  geom_node_text(aes(label = label, color =name, alpha = selected_node ), size = 3) 
    