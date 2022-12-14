# Day 9, Part 1

input <- read.fwf(file = "input9.txt", c(1, -1, 2), col.names = c("dir", "num"))

# Create grid for movement
grid <- matrix(0, nrow = 1000, ncol = 1000)

# Initialise starting position in the middle
x <- 500
y <- 500
grid[x,y] <- 1
Head <- c(x, y)
Tail <- c(x, y)
# plot(expand.grid(1:20, 1:20), pch = 1, cex = 0.2)
# text(Head[1], Head[2], "H")
# text(Tail[1], Tail[2], "T")
# Loop through directions
for(i in 1:nrow(input)) {
    j <- 1
    while(j <= input$num[i]) {
        # Move Head
        if(input$dir[i]=="U") {
            y <- y-1
            Head[2] <- y
            # plot(expand.grid(1:20, 1:20), pch = 1, cex = 0.2)
        }
        else if(input$dir[i]=="D") {
            y <- y+1
            Head[2] <- y
            # plot(expand.grid(1:20, 1:20), pch = 1, cex = 0.2)
        }
        else if(input$dir[i]=="L") {
            x <- x-1
            Head[1] <- x
            # plot(expand.grid(1:20, 1:20), pch = 1, cex = 0.2)
        }
        else if(input$dir[i]=="R") {
            x <- x+1
            Head[1] <- x
            # plot(expand.grid(1:20, 1:20), pch = 1, cex = 0.2)
        }

        # text(Head[1], Head[2], "H")
        # text(Tail[1], Tail[2], "T")
        # Move tail
        diff <- Head-Tail
        if(sum(abs(diff))>2) {
            # Diagonal, move to follow
            Tail <- Tail + (diff/(abs(diff)))
            grid[Tail[2], Tail[1]] <- 1
            # text(Tail[1], Tail[2], "T")
        }
        else if(diff[1]==2) {
            Tail[1] <- Tail[1]+1
            grid[Tail[2], Tail[1]] <- 1
            # text(Tail[1], Tail[2], "T")
        }
        else if(diff[1]==-2) {
            Tail[1] <- Tail[1]-1
            grid[Tail[2], Tail[1]] <- 1
            # text(Tail[1], Tail[2], "T")
        }
        else if(diff[2]==2) {
            Tail[2] <- Tail[2]+1
            grid[Tail[2], Tail[1]] <- 1
            # text(Tail[1], Tail[2], "T")
        }
        else if(diff[2]==-2) {
            Tail[2] <- Tail[2]-1
            grid[Tail[2], Tail[1]] <- 1
            # text(Tail[1], Tail[2], "T")
        }
        j <- j+1
        # Sys.sleep(3)
    }
}
sum(grid)

# Part 2
grid2 <- matrix(0, nrow = 1000, ncol = 1000)

# Initialise starting position in the middle
x <- 500
y <- 500
grid2[x,y] <- 1
positions <- data.frame(knot=c("H", 1:9), x, y)
# plot(expand.grid(1:20, 1:20), pch = 1, cex = 0.2)
# text(positions[,"x"], positions[,"y"], positions$knot)
# Loop through directions
for(i in 1:nrow(input)) {
    j <- 1
    while(j <= input$num[i]) {
        # Move Head
        if(input$dir[i]=="U") {
            y <- y-1
            positions[1,"y"] <- y
            # plot(expand.grid(1:20, 1:20), pch = 1, cex = 0.2)
        }
        else if(input$dir[i]=="D") {
            y <- y+1
            positions[1,"y"] <- y
            # plot(expand.grid(1:20, 1:20), pch = 1, cex = 0.2)
        }
        else if(input$dir[i]=="L") {
            x <- x-1
            positions[1,"x"] <- x
            # plot(expand.grid(1:20, 1:20), pch = 1, cex = 0.2)
        }
        else if(input$dir[i]=="R") {
            x <- x+1
            positions[1,"x"] <- x
            # plot(expand.grid(1:20, 1:20), pch = 1, cex = 0.2)
        }

        # text(positions[,"x"], positions[,"y"], positions$knot)
        # text(Tail[1], Tail[2], 1:9)
        # Move tail
        for(k in 2:10) {
            diff <- positions[k-1,2:3]-positions[k,2:3]
            if(sum(abs(diff))>2) {
                # Diagonal, move to follow
                positions[k,2:3] <- positions[k,2:3] + (diff/(abs(diff)))

            }
            else if(diff[1]==2) {
                positions[k,2] <- positions[k,2]+1
                # text(positions[,"x"], positions[,"y"], positions$knot)
            }
            else if(diff[1]==-2) {
                positions[k,2] <- positions[k,2]-1
                # text(positions[,"x"], positions[,"y"], positions$knot)
            }
            else if(diff[2]==2) {
                positions[k,3] <- positions[k,3]+1
                # text(positions[,"x"], positions[,"y"], positions$knot)
            }
            else if(diff[2]==-2) {
                positions[k,3] <- positions[k,3]-1
                # text(positions[,"x"], positions[,"y"], positions$knot)
            }

            if(k==10) {
                grid2[positions[k,3], positions[k,2]] <- 1
            }
        }
        j <- j+1
        # text(positions[,"x"], positions[,"y"], positions$knot)
        # Sys.sleep(0.5)
    }
}

sum(grid2)
