
## Lots of People including @drob
## Relies on the fact that diff of a rolling sum is just the dif of a leading and
## lagging variables

readings <- as.numeric(readLines("./Day_1/Part_1/input.txt"))

## Part 1
sum(diff(readings, lag = 1)>0)

## Part 2
sum(diff(readings,lag = 3)>0)