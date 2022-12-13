# Day 8, Part 1
library(stringr)
input <- readLines("input8.txt")
input <- matrix(as.numeric(str_split_fixed(input, "", n = 99)), ncol = 99)

# Count all edge trees
counter <- 98*4
# Count the middle
for(i in 2:(nrow(input)-1)) {
    for(j in 2:(ncol(input)-1)) {
        if(all(input[i, 1:(j-1)] < input[i,j]) |
           all(input[i, (j+1):99] < input[i,j]) |
           all(input[1:(i-1), j] < input[i,j]) |
           all(input[(i+1):99, j] < input[i,j])) {
            counter <- counter + 1
        }
    }
}

# Part 2

score <- matrix(0, nrow = 99, ncol = 99)

for(i in 2:(nrow(input)-1)) {
    for(j in 2:(ncol(input)-1)) {
        tree <- input[i,j]
        left <- rev(input[i, 1:(j-1)])
        right <- input[i, (j+1):99]
        up <- rev(input[1:(i-1), j])
        down <- input[(i+1):99, j]

        ls <- ifelse(length(which(left >= tree))>0, min(which(left >= tree)), length(left))
        rs <- ifelse(length(which(right >= tree))>0, min(which(right >= tree)), length(right))
        us <- ifelse(length(which(up >= tree))>0, min(which(up >= tree)), length(up))
        ds <- ifelse(length(which(down >= tree))>0, min(which(down >= tree)), length(down))
        score[i,j] <- ls*rs*us*ds
    }
}
