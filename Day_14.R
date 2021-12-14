library(tidyverse)
library(unglue)
# library(qdap)

file <- "./Day_14/Part_1/input.txt"
input <- readLines(file)

lines <-
  unglue_data(input, "{pairs} -> {insertion}", convert = TRUE) %>% 
  drop_na(insertion)

patterns <- 
  lines %>% 
  separate(pairs, into = c("first", "second"), sep = 1,remove = F) %>% 
  unite(insertion_2, "insertion", "second", sep = "", remove = F) %>% 
  unite(insertion_1, "first", "insertion", sep = "", remove = F) %>% 
  select(pairs, insertion_1, insertion_2) %>% 
  pivot_longer(-pairs, names_to = "type", values_to = "new_pairs") %>% 
  select(-type)

word <- input[1]

pairs <- list()
for(i in 1:(str_length(word)-1)){
  #i <- 2
  pairs[[as.character(i)]] <- str_sub(word, i,i+1)
  
}

## Part 1

pairs_df <- map_df(pairs, ~data.frame("pairs" = .x)) %>% count(pairs)

steps <- 10

for(i in 1:steps){
  
pairs_df <- 
  pairs_df %>% 
    left_join(patterns) %>% 
    group_by(new_pairs) %>% 
    summarise(n = sum(n)) %>% 
    mutate(pairs = new_pairs) %>% 
    select(-new_pairs)
}

final_letter <- str_sub(word, -1)

## Part 2

pairs_df <- map_df(pairs, ~data.frame("pairs" = .x)) %>% count(pairs)

steps <- 40

for(i in 1:steps){
  
  pairs_df <- 
    pairs_df %>% 
    left_join(patterns) %>% 
    group_by(new_pairs) %>% 
    summarise(n = sum(n)) %>% 
    mutate(pairs = new_pairs) %>% 
    select(-new_pairs)
}

final_letter <- str_sub(word, -1)

final_count <- 
  pairs_df %>% 
  separate(pairs, into = c("first", "second"), sep = 1,remove = T) %>% 
  pivot_longer(-n, names_to="order", values_to = "Element") %>% 
  group_by(Element) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(n = if_else(Element == final_letter, (n+1)/2, n/2)) %>% 
  arrange(-n) %>% 
  pull(n) 

  (max(final_count)- min(final_count)) %>% as.character()

  