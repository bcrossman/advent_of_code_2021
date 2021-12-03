library(tidyverse)
data <- readLines("./Day_3/Part_1/input.txt" )

##Part 1
diagnostic <- as.data.frame(str_split(data, pattern = "", simplify = T))

Mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  if(all(tab==tab[1])){"1"}else{
    ux[which.max(tab)]
  }
}
gamma <- 
  diagnostic %>% 
  summarise_all(Mode)  %>% 
  unlist(., use.names=FALSE) %>% 
  paste0(collapse = "") %>% 
  strtoi(base = 2)

epsilon <- 
  diagnostic %>% 
  summarise_all(Mode)  %>%
  mutate(across(everything(), ~as.character(abs(as.numeric(.)-1)))) %>% 
  unlist(., use.names=FALSE) %>% 
  paste0(collapse = "") %>% 
  strtoi(base = 2)

gamma*epsilon

##Part 2

oxygen <- diagnostic

for(i in 1:ncol(diagnostic)){
  if(nrow(oxygen)==1){next}
  oxygen <- 
    oxygen[oxygen[,i]==Mode(oxygen[,i]),]
}

oxygen_rating <- 
  oxygen %>% 
  # summarise_all(Mode)  %>% 
  unlist(., use.names=FALSE) %>% 
  paste0(collapse = "") %>% 
  strtoi(base = 2)

CO <- diagnostic

for(i in 1:ncol(diagnostic)){
  if(nrow(CO)==1) {next}
  CO <- 
    CO[CO[,i]==as.character(abs(as.numeric(Mode(CO[,i]))-1)),]
}

CO_rating <- 
  CO %>% 
  # summarise_all(Mode)  %>% 
  unlist(., use.names=FALSE) %>% 
  paste0(collapse = "") %>% 
  strtoi(base = 2)

oxygen_rating*CO_rating
