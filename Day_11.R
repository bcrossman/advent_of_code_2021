library(tidyverse)

file <- "./Day_11/Part_1/input.txt"
input <- readLines(file)

data <- 
  data.frame(energy = input) %>% 
  rowid_to_column() %>%
  separate_rows(energy, sep="", convert = T) %>% 
  drop_na(energy) %>% 
  group_by(rowid) %>% 
  mutate(colid = row_number()) %>% 
  ungroup() %>% 
  mutate(key = paste(rowid, colid, sep=",")) %>% 
  select(key, rowid, colid, energy)

total_triggered <- list()

for(i in 1:99999){
  # i <- 1
  data$energy <- data$energy+1
  
  triggered <-   
    data %>% 
    filter(energy==10)
  
  already_triggered <- triggered
  
  count <- 0
  while(nrow(triggered)>0){
    count <- count+1
    new_data_list <- list()
    
    for(j in 1:nrow(triggered)){
      new_data <- 
        data %>% 
        filter(rowid>=(triggered$rowid[j]-1),
               rowid<=(triggered$rowid[j]+1),
               colid<=(triggered$colid[j]+1),
               colid>=(triggered$colid[j]-1)) %>% 
        mutate(energy = 1)
      
      new_data_list[[as.character(j)]] <- new_data
    }
    
    data <- 
      new_data_list %>% 
      bind_rows() %>% 
      bind_rows(data) %>% 
      group_by(key, rowid, colid) %>% 
      summarise(energy = sum(energy), .groups = "drop") 
    
    triggered <-   
      data %>% 
      filter(energy>=10) %>% 
      anti_join(already_triggered, by = "key")
    
    already_triggered <- bind_rows(already_triggered, triggered)
  }
  
  data <- 
    data %>% 
    mutate(energy = if_else(energy>9, 0, energy))
  
  total_triggered[[paste(i,count)]] <- already_triggered
  
  flashes <- bind_rows(total_triggered, .id = "step_count")
  
  if(i == 100){
    
    
    flashes %>% 
      nrow() %>% 
      print()##Part 1
  }
  
  ##Check if done Part 2
  
  check <- 
    flashes %>% 
    separate(step_count, into = c("step"," count"), 
             sep = " ", 
             remove = T,
             convert=T) %>% 
    count(step) %>% 
    filter(n == 100) %>% 
    nrow()>0
  
  if(check){break}
}  

i ##Part 2