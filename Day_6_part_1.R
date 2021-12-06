library(tidyverse)

file <- "./Day_6/Part_1/input.txt"
  read.delim(file, 
             header = F, sep = ",") %>% 
  unlist %>% 
  as.numeric

days <- 80
breed_period <- 6
initial_wait_period <- 2

starting_possible <- unique(fish)
# days <- 1
chart <- list()
for(j in starting_possible){
  initial_fish <- c(j)
  for(i in 1:days){
    print(i)
    babies <- sum((initial_fish-1) < 0)
    babies_clocks <- rep(breed_period+initial_wait_period, babies)
    updated_fish_clock <- initial_fish-1
    updated_fish_clock[updated_fish_clock==-1] <- breed_period
    initial_fish <- c(updated_fish_clock, babies_clocks)
  }
  chart[[as.character(j)]] <- data.frame("start" = j, 
                                         "final" = length(initial_fish))
}

chart_df <- bind_rows(chart)

result <- left_join(data.frame("start" = fish), chart_df)

sum(result$final)
