# Day 3, Part 1
library(stringr)
priorities <- data.frame(n= 1:52, row.names = c(letters, LETTERS))
input <- scan("input3.txt", "character")
col1 <- str_sub(input, start = 1, end = (str_length(input)/2))
col2 <- str_sub(input, start = (str_length(input)/2)+1, end = str_length(input))

value <- 0
for(i in seq_along(col1)) {
    result <- str_extract(col2[i], str_split_1(col1[i], ""))
    result <- result[!is.na(result)]
    value <- value + priorities[result,"n"][1]
}

print(value)

# Part 2

groups <- split(input, ceiling(seq_along(input)/3))
value <- 0

for(i in seq_along(groups)) {
    index <- which.min(str_length(groups[[i]]))
    pats <- str_split_1(groups[[i]][index], "")
    for(j in seq_along(pats)) {
        matches <- str_detect(groups[[i]][-index], pats[j])
        if(all(matches)) {
            print(pats[j])
            value <- value + priorities[pats[j], "n"]
            break
        }
    }

}
