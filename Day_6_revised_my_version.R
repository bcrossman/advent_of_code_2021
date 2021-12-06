library(tidyverse)

file <- "./Day_6/Part_1/input.txt"
fish <- 
  read.delim(file, 
             header = F, sep = ",") %>% 
  unlist %>% 
  as.numeric

days <- 256
breed_period <- 6
initial_wait_period <- 2

starting_possible <- 0:(breed_period+initial_wait_period)

chart <- list()
## Calculates State change for each day
for(j in starting_possible){
  initial_fish <- c(j)
  babies <- sum((initial_fish-1) < 0)
  babies_clocks <- rep(breed_period+initial_wait_period, babies)
  updated_fish_clock <- initial_fish-1
  updated_fish_clock[updated_fish_clock==-1] <- breed_period
  initial_fish <- c(updated_fish_clock, babies_clocks)
  
  end_fish <- 
    data.frame("end" = initial_fish) %>% 
    count(end) %>% 
    mutate(start = j)
chart[[as.character(j)]] <- end_fish
}

chart_df <- bind_rows(chart)

result <- 
  data.frame("start" = fish) %>% 
  count(start)

for(l in 1:days){
  result <- 
    left_join(result,chart_df, by = c("start")) %>% 
    mutate(n = as.numeric(n.x)*as.numeric(n.y)) %>% 
    group_by(end) %>% 
    summarise(n = sum(n)) %>% 
    rename(start = end)
}

options("scipen"=99999)
sum(result$n)
