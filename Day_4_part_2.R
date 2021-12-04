library(tidyverse)
input <- read.delim("./Day_4/Part_1/input.txt", 
                    header = F, sep = ",", blank.lines.skip = F)
boards <- read_fwf(file = "./Day_4/Part_1/input.txt", skip =  1, 
                   skip_empty_rows = F)

##Part 1
numbers <- input[1,] %>% unlist %>% as.numeric

clean_boards <- 
  boards %>% 
  mutate(board_num = cumsum(is.na(X1))) %>% 
  filter(!is.na(X1)) %>% 
  group_by(board_num) %>% 
  mutate(rownum = row_number()) %>% 
  pivot_longer(X1:X5, names_to = "colnum", values_to = "value")

won_boards_prev <- c()
won_game = F
for(i in 1:length(numbers)){
  if(won_game){next}
  # i <- 15
  drawn_num <- numbers[1:i]
  
  any_good_rows <- 
    clean_boards %>% 
    mutate(is_drawn = value %in% drawn_num) %>% 
    group_by(board_num, rownum) %>% 
    summarise(is_drawn = sum(is_drawn)) %>% 
    filter(is_drawn == 5)
  
  any_good_cols <- 
    clean_boards %>% 
    mutate(is_drawn = value %in% drawn_num) %>% 
    group_by(board_num, colnum) %>% 
    summarise(is_drawn = sum(is_drawn)) %>% 
    filter(is_drawn == 5)
  
  winning_num <- numbers[i]
  won_boards <- unique(c(any_good_rows$board_num,
                         any_good_cols$board_num))
  last_board_won <- setdiff(won_boards, won_boards_prev)
  not_won <- setdiff(unique(clean_boards$board_num),
                     won_boards)
  
  if(length(not_won)==0){won_game=T}
  won_boards_prev <- won_boards
}

# winning_board <- any_good_cols$board_num 
# if(is_empty(winning_board)){
#   winning_board <- any_good_rows$board_num 
# }

clean_boards %>% 
  filter(board_num == last_board_won) %>% 
  mutate(is_not_drawn = !(value %in% drawn_num)) %>% 
  filter(is_not_drawn) %>% 
  summarise(value = sum(value)) %>% 
  pull(value)*winning_num

