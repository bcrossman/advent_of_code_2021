##From Darrin Speegle @speegled
library(matrixcalc)
library(tidyverse)

file <- "./Day_6/Part_1/input.txt"
input <- readLines(file)

vals <- 
  data.frame(x = input) %>% 
  separate_rows(x) %>% 
  mutate(x = factor(x, levels = 0:8)) %>%   ##Makes sure you get 0's for the missing variables
  with(table(x))   ##not 100% sure what the with does but seems to produce similar to this table(vals$x) while using pipe.

mm <- shift.right(diag(9), fill = c(0,0,0,0,0,0,1,0,1))  ##Creates the movement (markov??) shift for each day

print(sum(matrix.power(mm, 256) %*% vals), digits = 20)  #matrix multiplication of initial starting value times the final state....


## Next one is David Selby  @TeaStats, very intuitive. I rearranged it to pipe so I could see each step easier

x <- 
  read.delim(file, 
             header = F, sep = ",") %>% 
  unlist %>% 
  as.numeric

fish <- x %>% factor(levels = 0:8) %>% table() %>% as.double()
fish
for(i in 1:256){
  fish <- c(fish[2:7], fish[8]+fish[1], fish[9], fish[1])  ##The 2-7 just step down to become 1:6 in the queue, the 7's are the previous 8's and the newly refreshed 1, the 9's become 8's, and new 9's are also the old 1's (babies)
  
}

print(sum(fish), digits = 20) 
