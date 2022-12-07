# Day 7, part 1
library(stringr)
input <- readLines("input7.txt")
structure <- data.frame(dir="/", parent=NA, size=0)
parent_dir <- "/"
cur_dir <- "/"
j <- 2

for(i in 2:length(input)) {
    if(grepl("\\$ cd", input[i]) & !grepl("\\$ cd \\.\\.", input[i])) {
        structure[j,] <- data.frame(substring(input[i], 6), cur_dir, 0)
        parent_dir <- c(parent_dir, cur_dir)
        cur_dir <- substring(input[i], 6)
    }
    else if(grepl("\\$ cd \\.\\.", input[i])) {
        cur_dir <- tail(parent_dir, 1)
        parent_dir <- haed(parent_dir, -1)
    }
    else if(grepl("\\$ ls", input[i])) {
        next_cd <- grep("\\$ cd", tail(input, length(input)-i))[1]
        ls <- str_split_fixed(input[(i+1):(i+next_cd-1)], pattern = " ", n = 2)
        structure[structure$dir==cur_dir,"size"] <- sum(as.numeric(ls[,1]), na.rm = T)
        structure[structure$dir==parent_dir,"size"] <- structure[[cur_dir]] + structure[[parent_dir]]
    }
}
