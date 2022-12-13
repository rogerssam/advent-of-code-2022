# Day 9, Part 1

input <- read.fwf(file = "input9.txt", c(1, -1, 2), col.names = c("dir", "num"))

# Create grid for movement
grid <- matrix(0, nrow = 100, ncol = 100)

# Initialise starting position in the middle
x <- 50
y <- 50
grid[x,y] <- 1
Head <- c(x, y)
Tail <- c(x, y)
# Loop through directions
for(i in 1:nrow(input)) {
    j <- 1
    while(j <= input$num[i]) {
        # Move Head
        if(input$dir[i]=="U") {
            y <- y-1
            Head[2] <- y
        }
        else if(input$dir[i]=="D") {
            y <- y+1
            Head[2] <- y
        }
        else if(input$dir[i]=="L") {
            x <- x-1
            Head[1] <- x
        }
        else if(input$dir[i]=="R") {
            x <- x+1
            Head[1] <- x
        }

        # Move tail
        if(any(abs(Head-Tail)>1)) {
            # Diagonal, move to follow
            diff <- Head-Tail
            Tail <- Tail + (diff/(abs(diff)))
            grid[Tail[2], Tail[1]] <- 1
        }
        j <- j+1
    }
}
