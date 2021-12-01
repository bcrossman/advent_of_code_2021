library(tidyverse)

readings <- read_tsv("./Day_1/Part_1/input.txt", col_names = c("depth"))

readings %>% 
  mutate(changes = depth>lag(depth)) %>% 
  pull(changes) %>% 
  sum(na.rm=T)
