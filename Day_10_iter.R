# library(data.table)
# library(dtplyr)
# library(dplyr, warn.conflicts = FALSE)
library(tidyverse)

file <- "./Day_10/Part_1/input.txt"
input <- readLines(file) %>% data.frame("characters" = .)

start <- input$characters %>% str_length() %>% sum()
chg <- 1
while(chg>0){
  
  input <-
    input %>%
    mutate(characters = gsub(pattern = "()",
                             replacement = "", fixed = T,
                             characters)) %>% 
    mutate(characters = gsub(pattern = "<>",
                             replacement = "", fixed = T,
                             characters)) %>% 
    mutate(characters = gsub(pattern = "[]",
                             replacement = "", fixed = T,
                             characters)) %>% 
    mutate(characters = gsub(pattern = "{}",
                             replacement = "", fixed = T,
                             characters))
  new_length <- input$characters %>% str_length() %>% sum()  
  chg = start - new_length
  start <- new_length
}  

data <- 
  input %>% 
  rowid_to_column() %>%
  separate_rows(characters, sep="", convert = T) %>% 
  filter(!is.na(characters)) %>% 
  filter(characters != "") %>% 
  group_by(rowid) %>% 
  mutate(colid = row_number())

mapping <- 
  tibble( characters =      c("{", "<", "[", "(", ">", "]", ")", "}"),
          group_character = c("{", "<", "[", "(", "<", "[", "(", "{"),
          value = c(rep(1,4),rep(-1,4)),
          type = c(rep("open",4),rep("close",4)))

points_df <-
  tibble(characters = c(")", "]", "}", ">"), 
         points =     c(3,57,1197,25137))

data %>% 
  left_join(mapping) %>% 
  filter(type=="close") %>% 
  group_by(rowid) %>% 
  slice(1) %>% 
  left_join(points_df) %>% 
  pull(points) %>% 
  sum()

##Part 2

corrupted <- 
  data %>% 
  left_join(mapping) %>% 
  filter(type=="close") %>% 
  group_by(rowid) %>% 
  slice(1) 


points_df <-
  tibble(characters = c("(", "[", "{", "<"), 
         points =     c(1,2,3,4))

result <- 
  data %>% 
  filter(!(rowid %in% corrupted$rowid)) %>% 
  left_join(mapping) %>% 
  left_join(points_df) %>% 
  arrange(rowid, desc(colid)) 

total_points_vec <- c()

for(i in unique(result$rowid)){
  #i <- 10
  row_d <- 
    result %>% 
    filter(rowid == i) 
  
  total_points <- row_d$points[1]
  
  for(j in 2:nrow(row_d)){
    #j <- 3
    total_points <- 5*total_points+row_d$points[j]
  }
    
  total_points_vec <- c(total_points_vec, total_points)
}
sort(total_points_vec) %>% median()
