library(tidyverse)

file <- "./Day_8/Part_1/input.txt"
signal <- 
  read.delim(file, 
             header = F, sep = "|", col.names = c("input", "output")) 

seven_digit_bin <- 
  read_csv("./Day_8/Part_1/seven_digit_bin.csv")

seven_digit_bin

##Create all combinations of binary numbers
all <- expand.grid(a = 2^(0:6), b = 2^(0:6), c = 2^(0:6), d = 2^(0:6), e = 2^(0:6), f = 2^(0:6), g = 2^(0:6)) 

perms <- all[apply(all, 1, function(x) {length(unique(x)) == 7}),]

possible_maps <- 
  perms %>% 
  mutate(combination = row_number()) %>% 
  pivot_longer(cols =-combination, names_to="code", values_to="value")

save_output <- list()

for(i in 1:nrow(signal)){

reading <- 
  signal %>% 
  rowid_to_column() %>%
  filter(rowid == i) %>% 
  pivot_longer(cols = -rowid, names_to ="type", values_to = "code") %>% 
  separate_rows(code, sep=" ") %>% 
  group_by(rowid) %>% 
  mutate(word_id = row_number()) %>% 
  separate_rows(code, sep = "") %>% 
  filter(code != "") %>% 
  group_by(rowid, word_id) %>% 
  mutate(letter_id = row_number()) %>% 
  left_join(possible_maps, by="code") %>% 
  group_by(rowid, combination, word_id) %>% 
  mutate(word_value = sum(value),
         length_word = n()) %>% 
  group_by(rowid, combination) %>% 
  mutate(all_work = all(word_value %in% seven_digit_bin$Dec))%>% 
  filter(all_work) %>% 
  left_join(seven_digit_bin, by= c("word_value"="Dec"))

print(i)
print(length(unique(reading$combination)))

save_output[[as.character(i)]] <- reading

}

final_done <- bind_rows(save_output)

##Part 1 Answer

final_done %>% 
  filter(type == "output") %>% 
  group_by(rowid, word_id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(Digit) %>% 
  filter(Digit %in% c(1,4,7,8)) %>% 
  pull(n) %>% 
  sum()

##Part 2 Answer

final_done %>% 
  filter(type == "output") %>% 
  group_by(rowid, word_id) %>% 
  slice(1) %>% 
  group_by(rowid) %>% 
  summarize(number = paste0(Digit, collapse = "")) %>% 
  mutate(number = as.numeric(number)) %>% 
  pull(number) %>% 
  sum()




