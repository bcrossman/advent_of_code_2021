
##Base R with @antoine_fabri

# data
file <- "./Day_4/Part_1/input.txt"
draws <- scan(file, what = numeric(), sep = ",", quiet = TRUE, nlines = 1)
boards_raw <- read.delim(file, skip = 2, header = F, sep ="")
n <- ncol(boards_raw)
n_boards <- nrow(boards_raw) / n
boards <- data.frame(
  num = unlist(boards_raw),  ## so he made it a long vector right away
  board = rep(1:n_boards, each = n),  #then assigned the board an id based on size of board
  col = rep(rep(1:n, each = n * n_boards)),  ## I feel like I'd not know (without testing) how a dataframe unfurls, like is it by col or row
  row = 1:n)  ##But I should say, in the end this gets to my same board dataframe, which is cool

# the row/line indice is stored in `row` because "reshape"
boards_tidy <- reshape(boards, idvar =c("num", "board"),direction = "long", varying = list(c("row", "col")), times = c("row", "col"), timevar = "dir")
##BC: I definitely don't understand what reshape is doing, my guess is it is making two data frames almost
# one of the rows and one of the columns because either can give you the "win"
boards_tidy$match <- match(boards_tidy$num, draws)  ##Antoine mentions seeing @drob use this.  This is very cool.  
#It tells you which index value of the number vector you'd find each number on the bingo card. Thus you can figure out the first time each row or column finishes
##Which is exactly what they do next...I think
boards_tidy$highest_match_by_line  <- ave(boards_tidy$match, boards_tidy$board, boards_tidy$dir, boards_tidy$row, FUN = max)
boards_tidy$winning_match_by_board <- ave(boards_tidy$highest_match_by_line, boards_tidy$board, FUN = min)

# part 1
ending_match <- min(boards_tidy$winning_match_by_board)  ##lowest winning index of called numbers for the winning board
winning_board <- subset(boards_tidy, board == board[winning_match_by_board == ending_match] & match > ending_match)  #filters to the winning board and the numbers that wouldn't have been called based on the ending match
sum(winning_board$num) * draws[ending_match] / 2  ##divided by two because it was repeated for rows and columns

# part 2, same but min -> max
ending_match <- max(boards_tidy$winning_match_by_board)  #last index of winning number called 
winning_board <- subset(boards_tidy, board == board[winning_match_by_board == ending_match] & match > ending_match)
sum(winning_board$num) * draws[ending_match] / 2


## I think for my tidyverse, instead of doing the true/false in the for statement
## I should have matched to get when each number is drawn, then do min/max's to solve
## everything else.