library(tidyverse)
library(RcppRoll)

readings <- read_tsv("./Day_1/Part_1/input.txt", col_names = c("depth"))

readings %>% 
  mutate(rolling_depth = RcppRoll::roll_sum(depth, 3, align = "right", fill =NA)) %>% 
  mutate(changes = rolling_depth>lag(rolling_depth)) %>% 
  pull(changes) %>% 
  sum(na.rm=T)
