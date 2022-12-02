# Day 2, part 1
# A = Rock, B = Paper, C = Scissors
# X = Rock, Y = Paper, Z = Scissors
input <- read.delim("input2.txt", sep = " ", header = F)
input$opponent <- 1
input$opponent <- ifelse(input$V1=="B",
                       yes = 2,
                       no = ifelse(input$V1=="C",
                                   yes = 3, no = input$opponent))
input$shape <- 1
input$shape <- ifelse(input$V2=="Y",
                       yes = 2,
                       no = ifelse(input$V2=="Z",
                                   yes = 3, no = input$shape))
input$outcome <- input$shape - input$opponent
input$score <- ifelse(input$outcome %in% c(1, -2), 6, ifelse(input$outcome == 0, yes = 3, no = 0))+input$shape

final_score <- sum(input$score)

# Part 2
# X = lose, Y = draw, Z = win

input <- read.delim("input2.txt", sep = " ", header = F, col.names = c("opponent", "outcome"))

input$score <- 0
input$score <- ifelse(input$outcome=="Y",
                      yes = 3,
                      no = ifelse(input$outcome=="Z",
                                  yes = 6, no = 0))

input$shape <- 0
input$shape[input$opponent=="A" & input$outcome=="X"] <- 3
input$shape[input$opponent=="A" & input$outcome=="Y"] <- 1
input$shape[input$opponent=="A" & input$outcome=="Z"] <- 2
input$shape[input$opponent=="B" & input$outcome=="X"] <- 1
input$shape[input$opponent=="B" & input$outcome=="Y"] <- 2
input$shape[input$opponent=="B" & input$outcome=="Z"] <- 3
input$shape[input$opponent=="C" & input$outcome=="X"] <- 2
input$shape[input$opponent=="C" & input$outcome=="Y"] <- 3
input$shape[input$opponent=="C" & input$outcome=="Z"] <- 1

sum(input$score + input$shape)

