# Day 7, part 1
library(stringr)
input <- readLines("input7.txt")
structure <- list()
structure[["/"]] <- 0
# find_size <- function(x) {}
parent_dir <- "/"

for(i in 1:length(input)) {
    if(grepl("\\$ cd", input[i]) & !grepl("\\$ cd \\.\\.", input[i])) {
        parent_dir <- cur_dir
        cur_dir <- substring(input[i], 6)
        structure[[cur_dir]] <- 0
    }
    else if(grepl("\\$ cd \\.\\.", input[i])) {
        cur_dir <- parent_dir
    }
    else if(grepl("\\$ ls", input[i])) {
        next_cd <- grep("\\$ cd", tail(input, length(input)-i))[1]
        ls <- str_split_fixed(input[(i+1):(i+next_cd-1)], pattern = " ", n = 2)
        structure[[cur_dir]] <- sum(as.numeric(ls[,1]), na.rm = T) + structure[[cur_dir]]
        structure[[parent_dir]] <- structure[[cur_dir]] + structure[[parent_dir]]
    }
}
