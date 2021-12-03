
## A pretty understandable one that uses base r from @_willdebras

input <- read.fwf("./Day_3/Part_1/input.txt", widths = c(rep(1, 12)))

gamma <- apply(input, 2, function(x) ceiling(median(x)))
epsilon <- as.integer(sapply(gamma, `!`))  #0/1 works kind of like false / true, so the ! reverses a 0 or a 1

gamma_dec <- strtoi(paste0(gamma, collapse = ''), 2)  #originally I learned this str_toi from @_Riinu_, so neat
epsilon_dec <- strtoi(paste0(epsilon, collapse = ''), 2)


gamma_dec * epsilon_dec


# Part 2

oxy <- input
co2 <- input
oxy$dec <- apply(oxy, 1, function(x) strtoi(paste0(x, collapse = ''), 2))
co2$dec <- apply(co2, 1, function(x) strtoi(paste0(x, collapse = ''), 2))

while (nrow(oxy) > 1) {
  oxy <- oxy[oxy[[1]]==ceiling(median(oxy[[1]])), -1, drop = F]
}


while (nrow(co2) > 1) {
  co2 <- co2[co2[[1]]==as.integer(sapply(ceiling(median(co2[[1]])), `!`)), -1, drop = F]
}

oxy$dec * co2$dec

