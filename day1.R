# Day 1, Part 1
input <- read.delim2("input1.txt", blank.lines.skip = FALSE, )
input <- as.numeric(input$X2494)

val <- 0
max <- 0
for(i in 1:length(input)) {
    if(is.na(input[i])) {
        val <- 0
    }
    else {
        val <- input[i] + val
    }
    if (val > max) {
        max <- val
    }
}


# Part 2
totals <- rep(0, 2250)
val <- 0
max <- 0
j <- 1
for(i in 1:length(input)) {
    if(is.na(input[i])) {
        totals[j] <- val
        val <- 0
        j <- j+1
    }
    else {
        val <- input[i] + val
    }
}

totals <- totals[totals>0]
totals <- sort(totals, decreasing = TRUE)
sum(totals[1:3])
