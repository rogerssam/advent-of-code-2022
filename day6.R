# Day 6, part 1
library(stringr)
input <- readLines("input6.txt")
input <- str_split_1(input, "")
for(i in 4:length(input)) {
    if(all(table(input[(i-3):i])==1)) {
        print(i)
        print(table(input[(i-3):i]))
        break
    }
}

# Part 2

for(i in 14:length(input)) {
    if(all(table(input[(i-13):i])==1)) {
        print(i)
        print(table(input[(i-13):i]))
        break
    }
}
