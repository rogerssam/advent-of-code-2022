# Day 5, part 1
library(stringr)
input <- read.delim("input5.txt", col.names = "X")
moves <- str_split_fixed(input[9:nrow(input),1], " ", 6)[,c(2,4,6)]
moves <- as.data.frame(apply(moves, 2, as.numeric))
colnames(moves) <- c("n", "from", "to")

stack <- read.fwf("input5.txt", rep(c(3, -1), times = 9), n = 8, na.strings = "   ")
stack <- t(stack[nrow(stack):1,])
stack <- split(stack, seq(nrow(stack)))
stack <- lapply(stack, function(x) x[!is.na(x)])
stack_2 <- stack

for(i in 1:nrow(moves)) {
    stack[[moves$to[i]]] <- append(stack[[moves$to[i]]], rev(tail(stack[[moves$from[i]]], n = moves$n[i])))
    stack[[moves$from[i]]] <- head(stack[[moves$from[i]]], -moves$n[i])
}
paste(sapply(stack, tail, 1))


# Part 2

for(i in 1:nrow(moves)) {
    stack_2[[moves$to[i]]] <- append(stack_2[[moves$to[i]]], tail(stack_2[[moves$from[i]]], n = moves$n[i]))
    stack_2[[moves$from[i]]] <- head(stack_2[[moves$from[i]]], -moves$n[i])
}
paste(sapply(stack_2, tail, 1))
