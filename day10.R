# Day 10, part 1

input <- read.fwf("input10.txt", widths = c(4, -1, 3), col.names = c("inst", "val"))
input$val[is.na(input$val)] <- 0
input$val[1] <- input$val[1]+1
input$X <- cumsum(input$val)
input$ncyc <- ifelse(input$inst=="noop", 1, 2)
input$cycle <- cumsum(input$ncyc)
input$strength <- input$X*input$cycle
sum(input$strength[input$cycle %in% c(20, 60, 100, 140, 179, 220)])
input$test <- 0
