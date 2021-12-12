library(tidyverse)
library(igraph)

file <- "./Day_12/Part_1/input.txt"
input <- readLines(file)

network_data <- 
  data.frame(link = input) %>% 
  separate(link, into = c("beg","end"), sep = "-") 

network_data_bi_direct <- 
  network_data %>% 
  bind_rows(network_data %>% rename(beg = end,
                                    end = beg)) %>% 
  filter(beg != "end",
         end != "start")

##Part 1

current_net <- 
  network_data_bi_direct %>% 
  filter(beg == "start")

net_len <- nrow(current_net)
chg <- 1
count <- 0
while(TRUE){
  count <- count+1
  current_net <- 
    current_net %>% 
    left_join(network_data_bi_direct, 
              by = c("end"="beg")) %>%
    rename(!!paste0("end_",count):= end) %>% 
    rename(end = end.y) %>% 
    mutate(path_id = row_number()) %>% 
    pivot_longer(cols = -path_id, names_to = "node", values_to = "value") %>%
    drop_na(value) %>% 
    mutate(is_upper = (value == toupper(value))) %>% 
    group_by(path_id, value) %>% 
    mutate(n = n()) %>% 
    ungroup() %>% 
    mutate(is_good = is_upper|(n==1)) %>% 
    group_by(path_id) %>% 
    mutate(path_good = all(is_good)) %>% 
    filter(path_good) %>% 
    select(path_id, node, value) %>% 
    pivot_wider(id_cols = path_id, names_from = "node", values_from="value") %>% 
    ungroup %>% 
    select(-path_id)
  
  if(nrow(current_net)==
     nrow(current_net %>% filter(end == "end" | is.na(end)))){break}

}

nrow(current_net)

##Part 2

current_net <- 
  network_data_bi_direct %>% 
  filter(beg == "start")

net_len <- nrow(current_net)
chg <- 1
count <- 0
while(TRUE){
  count <- count+1
  current_net <- 
    current_net %>% 
    left_join(network_data_bi_direct, 
              by = c("end"="beg")) %>%
    rename(!!paste0("end_",count):= end) %>% 
    rename(end = end.y) %>% 
    mutate(path_id = row_number()) %>% 
    pivot_longer(cols = -path_id, names_to = "node", values_to = "value") %>%
    drop_na(value) %>% 
    mutate(is_upper = (value == toupper(value))) %>% 
    group_by(path_id, value) %>% 
    mutate(n = n()) %>% 
    ungroup() %>% 
    mutate(is_good = is_upper|(n<=2)) %>% 
    group_by(path_id, is_upper) %>%
    mutate(n_type = n()) %>% 
    mutate(n_distinct_type = n_distinct(value)) %>% 
    mutate(is_good_type = is_upper| ((n_type-n_distinct_type)<=1)) %>% 
    group_by(path_id) %>% 
    mutate(path_good = all(is_good)&all(is_good_type)) %>% 
    filter(path_good) %>% 
    select(path_id, node, value) %>% 
    pivot_wider(id_cols = path_id, names_from = "node", values_from="value") %>% 
    ungroup %>% 
    select(-path_id)
  
  if(nrow(current_net)==
     nrow(current_net %>% filter(end == "end" | is.na(end)))){break}
  print(count)
}

nrow(current_net)
