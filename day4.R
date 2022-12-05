# Day 4, part 1
input <- matrix(as.numeric(stringr::str_split_fixed(scan("input4.txt", "character"), pattern = "(-|,)", n = 4)), ncol = 4)
sum((input[,1] <= input[,3] & input[,2] >= input[,4]) | (input[,1] >= input[,3] & input[,2] <= input[,4]))

# Part 2
sum(!(input[,2] < input[,3] | input[,1] > input[,4]))
