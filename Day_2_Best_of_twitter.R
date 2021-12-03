
## A really cool one that uses eval parce by @antoine_fabri  
## Helpful to have around as eval and str2lang can come in handy

input <- scan("./Day_2/Part_1/input.txt", what = character(), sep = "\n", quiet = TRUE)

# part 1
# transform code into complex arithmetic and eval
code <- paste(input, collapse = "")
code <- gsub("forward ", "+", code)
code <- gsub("down (\\d+)", "+\\1i", code)
code <- gsub("up (\\d+)", "-\\1i", code)
pos <- eval(str2lang(code))
part1 <- Re(pos) * Im(pos)

# part1 again
# same with vectors rather than complex values
code <- paste(input, collapse = "")
code <- gsub("forward (\\d+)", "+c(\\1,0)", code)
code <- gsub("down (\\d+)", "+c(0,\\1)", code)
code <- gsub("up (\\d+)", "-c(0,\\1)", code)
pos <- eval(str2lang(code))
prod(pos)

# part 2
# convert instructions to functions and reduce
code <- gsub("forward (\\d+)", "\\\\(x) x + c(\\1, x[3] * \\1, 0)", input)
code <- gsub("down (\\d+)", "\\\\(x) x + c(0, 0, \\1)", code)
code <- gsub("up (\\d+)", "\\\\(x) x - c(0, 0, \\1)", code)
pos <- Reduce(function(x, f) f(x), lapply(code, \(f) eval(str2lang(f))), c(0,0,0))
prod(pos[1:2])

## Best Tidyverse solution by @jonatanpallesen

moves <- vroom::vroom("./Day_2/Part_1/input.txt", col_names = c("command","value")) #splits it for you and can take different delim

## Part 1

##kind of does a pivot via the mutate / case_when
moves <- 
  moves %>%
  mutate(vertical = case_when(
    command == "down" ~ value,
    command == "up" ~-value,
    T ~ 0),
    horizontal = ifelse(command == "forward", value, 0)
  )

sum(moves$horizontal) * sum(moves$vertical)

##Part 2

moves <- 
  moves %>%
  mutate(aim = cumsum(vertical),
         vertical = aim * horizontal
  )

sum(moves$horizontal) * sum(moves$vertical)