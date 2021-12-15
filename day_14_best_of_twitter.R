## from 
#Antoine Fabri
#@antoine_fabri

library(tidyverse)

file <- "./Day_14/Part_1/input.txt"
input <- readLines(file)

pairs <- tibble(
  from = substring(input[1], 1:(nchar(input[1]) - 1), 2:nchar(input[1])), ## a vector of start and stops
  n = 1)

first_letter <- substr(input[1], 1, 1)
mapper <-
  unglue::unglue_data(input[-(1:2)], "{from} -> {to}") %>%
  mutate(to = pmap(list(from, to), ~ c(
    paste0(substr(.x, 1, 1), .y),
    paste0(.y, substr(.x, 2, 2))))) %>%
  unnest(to)

solve <- function(steps) {
  for (i in 1:steps) {
    pairs <-
      left_join(pairs, mapper, by = "from") %>%
      count(from = to, wt = n)  #I had no idea there was a weight option in count
  }
  counts <-
    pairs %>%
    separate(from, c("from1", "from2"), sep = 1) %>%
    count(letter = from2, wt = n) %>%
    mutate(n = ifelse(letter == first_letter, n+1, n))
  format(max(counts$n) - min(counts$n), scientific = FALSE)
}

solve(10)
solve(40)