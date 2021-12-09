# library(data.table)
# library(dtplyr)
# library(dplyr, warn.conflicts = FALSE)
library(tidyverse)

file <- "./Day_9/Part_1/input.txt"
input <- readLines(file)

data <- 
  data.frame(height = input) %>% 
  rowid_to_column() %>%
  separate_rows(height, sep="", convert = T) %>% 
  filter(!is.na(height)) %>% 
  mutate(colid = rep(1:100, times = 100))

##  Join edges

low_points <- 
  data %>% 
  left_join(data %>% mutate(rowid = rowid-1) %>% mutate(height = height-1),#ties
            by = c("rowid","colid"),
            suffix = c("", "_down")) %>% 
  left_join(data %>% mutate(rowid = rowid+1)%>% mutate(height = height-1),
            by = c("rowid","colid"),
            suffix = c("", "_up")) %>% 
  left_join(data %>% mutate(colid = colid+1)%>% mutate(height = height-1),
            by = c("rowid","colid"),
            suffix = c("", "_left")) %>% 
  left_join(data %>% mutate(colid = colid-1)%>% mutate(height = height-1),
            by = c("rowid","colid"),
            suffix = c("", "_right")) %>% 
  rename(height_key = height) %>% 
  pivot_longer(cols = -c(rowid,colid),
               names_to = c("key"),
               values_to = "value") %>% 
  group_by(rowid,colid) %>% 
  mutate(is_min = value == min(value, na.rm=T)) %>% 
  ungroup() %>% 
  filter(key == "height_key") %>% 
  filter(is_min) 
  
  low_points %>% 
  mutate(risk = value+1) %>% 
  pull(risk) %>% 
  sum()

## Part 2

data <- 
  data %>% 
  mutate(key = paste(rowid, colid, sep=",")) 

to_try <-   
  low_points %>% 
  mutate(key = paste(rowid, colid, sep=",")) %>% 
  mutate(basin_id = row_number()) %>% 
  rename(height = value)

tried <- tibble()

while(nrow(to_try)>0){
  
  new_to_try <- 
    data %>% 
    filter(((colid==to_try$colid[1])&
              (rowid>=(to_try$rowid[1]-1)&
                 rowid<=(to_try$rowid[1]+1)))|
             ((rowid==to_try$rowid[1])&(
               colid<=(to_try$colid[1]+1)&
                 colid>=(to_try$colid[1]-1))),
  height !=9) %>% 
  mutate(basin_id = to_try$basin_id[1])

new_tried <- to_try %>% slice(1)

tried <- bind_rows(tried, new_tried)

to_try <- 
  to_try %>% 
  bind_rows(new_to_try) %>% 
  anti_join(tried, by = "key")

print(nrow(to_try))
}

tried %>% 
  count(basin_id) %>% 
  arrange(desc(n)) %>% 
  slice(1:3) %>% 
  pull(n) %>% 
  prod()
