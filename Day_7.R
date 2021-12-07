
file <- "./Day_7/Part_1/input.txt"
readings <- 
  read.delim(file, 
             header = F, sep = ",") %>% 
  unlist %>% 
  as.numeric

## Part 1
avg <- round(median((readings)))
sum(abs(readings-avg))

## Part 2

nth_triang_num <- function(x){(x^2+x)/2}

crossing(min(readings):max(readings), 
         data.frame("readings" = readings) %>% count(readings)) %>% 
  set_names(c("test_number", "readings", "n")) %>% 
  mutate(fuel = n*nth_triang_num(abs(readings-test_number))) %>% 
  group_by(test_number) %>% 
  summarise(fuel=sum(fuel)) %>% 
  filter(fuel == min(fuel)) %>% 
  pull(fuel)
