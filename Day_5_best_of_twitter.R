##From Madhur Parihar @pariharmadhur (edited a bit for myself)
library(unglue)
library(tidyverse)

file <- "./Day_5/Part_1/input.txt"
input <- readLines(file)

# part 1

rr <- 
  unglue_data(input, "{start_x},{start_y} -> {end_x},{end_y}", convert = TRUE) %>% 
  filter(start_x==end_x | start_y==end_y) %>%   ##Keep only vert/horiz
  rowwise() %>% ##used since you want to treat each row as it's own input, not the entire vector??
  mutate(points = list(paste0(start_x:end_x,"_",start_y:end_y))) %>%  ##creates a "nested df" / column of lists. Later these are just unlisted and counted
  pull(points) %>% 
  unlist()

sum(table(rr)>1)

rr <- 
  unglue_data(input, "{start_x},{start_y} -> {end_x},{end_y}", convert = TRUE) %>% 
  # filter(start_x==end_x | start_y==end_y) %>%   ##Keep only vert/horiz
  rowwise() %>% ##used since you want to treat each row as it's own input, not the entire vector??
  mutate(points = list(paste0(start_x:end_x,"_",start_y:end_y))) %>%  ##creates a "nested df" / column of lists. Later these are just unlisted and counted
  pull(points) %>% 
  unlist()

sum(table(rr)>1)


## From Riinu @_riinu_  ##changed a bit to match mine

unglue_data(input, "{start_x},{start_y} -> {end_x},{end_y}", convert = TRUE) %>% 
  filter(start_x==end_x | start_y==end_y) %>% 
  # it's all single rows, but this group_by() is a 'hack' to get a non-vectorised function
  # - seq() - to work as a vectorized one
  rowwise() %>% 
  mutate(x = paste0(seq(start_x, end_x), collapse = ","),
         y = paste0(seq(start_y, end_y), collapse = ",")) %>% 
  separate_rows(x, y) %>% 
  count(x, y) %>% 
  count(n > 1)

# Part II - code identical to Part I, only removed filter(x1 == x2 | y1 == y2)

unglue_data(input, "{start_x},{start_y} -> {end_x},{end_y}", convert = TRUE) %>% 
  rowwise() %>% 
  mutate(x = paste0(seq(start_x, end_x), collapse = ","),
         y = paste0(seq(start_y, end_y), collapse = ",")) %>% 
  ungroup() %>% 
  separate_rows(x, y) %>% 
  count(x, y) %>% 
  count(n > 1)

## From Bob Rudis @hrbrmstr
library(stringi)
file <- "./Day_5/Part_1/input.txt"
# more readable MARGIN values
BY_ROW <- 1
BY_COL <- 2

input <- readLines(file)

# make a matrix out of the points pairs
(stri_match_first_regex(input, "^([[:digit:]]+),([[:digit:]]+) -> ([[:digit:]]+),([[:digit:]]+)$")[,2:5]) |>
  apply(BY_COL, as.numeric) |>
  as.data.frame() |>
  setNames(
    c("x1", "y1", "x2", "y2") # i'd rather use x#/y# vs indices
  ) -> points   ##I think this got to where I got with glue

head(points)
##   x1 y1 x2 y2
## 1  0  9  5  9
## 2  8  0  0  8
## 3  9  4  3  4
## 4  2  2  2  1
## 5  7  0  7  4
## 6  6  4  2  0

points <- points + 1 # yay R not handling 0 indices!

# make a grid (matrix) using max values for x & y
grid <- matrix(0, nrow = max(c(points$y1, points$y2)), ncol = max(c(points$x1, points$x2)))

grid
##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
##  [1,]    0    0    0    0    0    0    0    0    0     0
##  [2,]    0    0    0    0    0    0    0    0    0     0
##  [3,]    0    0    0    0    0    0    0    0    0     0
##  [4,]    0    0    0    0    0    0    0    0    0     0
##  [5,]    0    0    0    0    0    0    0    0    0     0
##  [6,]    0    0    0    0    0    0    0    0    0     0
##  [7,]    0    0    0    0    0    0    0    0    0     0
##  [8,]    0    0    0    0    0    0    0    0    0     0
##  [9,]    0    0    0    0    0    0    0    0    0     0
## [10,]    0    0    0    0    0    0    0    0    0     0

# part 1 

for_part_2 <- c() # we'll save lines that don't fit part 1 criteria to save processing time

for (line in seq_len(nrow(points))) { # iterate over the lines
  
  if (points$x1[line] == points$x2[line]) { # is horizontal?
    
    x <- points$x1[line]
    y <- seq(points$y1[line], points$y2[line])
    
    grid[y, x] <- grid[y, x] + 1
    
  } else if (points$y1[line] == points$y2[line]) { # is vertical?
    
    x <- seq(points$x1[line], points$x2[line])
    y <- points$y1[line]
    
    grid[y, x] <- grid[y, x] + 1
    
  } else {
    for_part_2 <- c(for_part_2, line) # save these for part 2
  }
  
}

grid
##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
##  [1,]    0    0    0    0    0    0    0    1    0     0
##  [2,]    0    0    1    0    0    0    0    1    0     0
##  [3,]    0    0    1    0    0    0    0    1    0     0
##  [4,]    0    0    0    0    0    0    0    1    0     0
##  [5,]    0    1    1    2    1    1    1    2    1     1
##  [6,]    0    0    0    0    0    0    0    0    0     0
##  [7,]    0    0    0    0    0    0    0    0    0     0
##  [8,]    0    0    0    0    0    0    0    0    0     0
##  [9,]    0    0    0    0    0    0    0    0    0     0
## [10,]    2    2    2    1    1    1    0    0    0     0

sum(grid>1)
## [1] 5

# part 2

# already did horiz & vert, now do diag
for (line in for_part_2) {
  
  # make grid (matrix) points from the x & y sequences
  #
  # "vents" would look like this when processing the first diagonal line (line 2)
  #
  #      [,1] [,2]
  # [1,]    1    9
  # [2,]    2    8
  # [3,]    3    7
  # [4,]    4    6
  # [5,]    5    5
  # [6,]    6    4
  # [7,]    7    3
  # [8,]    8    2
  # [9,]    9    1
  
  cbind(
    seq(points$y1[line], points$y2[line]),
    seq(points$x1[line], points$x2[line])
  ) -> vents
  
  grid[vents] <- grid[vents] + 1
  
}

sum(grid>1)
## [1] 12

