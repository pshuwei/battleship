##' Play the battleship game
##'
##' Randomly hides a ship onto the board and it's down to you to find the ship
##' @author Peter Chang
##' @export

print("decide how many rows")

my.rows <- as.numeric(readline(prompt="Enter number: "))

print("decide how many columns")

my.cols <- as.numeric(readline(prompt="Enter number: "))

bracket <- "[?]"

board <- matrix(NA, nrow = my.rows, ncol = my.cols)
colnames(board) <- LETTERS[1:my.cols]
rownames(board) <- 1:my.rows

#making the board
for (i in 1:my.rows) {
 for (j in 1:my.cols) {
  board[i,j] <- bracket
 }
}

print(board, quote = FALSE)

#deciding how big ship will be
if (nrow(board) < 3 | ncol(board) < 3 ) {
 shipsize <- sample(2, 1)
} else{
 shipsize <- sample(2:3, 1)
}

cat("The size of the ship is", shipsize, "units", "\n")

#creating first part of ship
shiprow <- sample(1:my.rows, 1)
shipcol <- sample(LETTERS[1:my.cols], 1)
cat(shipcol, shiprow, shipsize, "\n")

direction <- c("left", "right", "up", "down")

#deciding how to extend the ship
if (shipcol == "A" & shiprow == 1) { #topleft corner
 direction2 <- sample(direction[-c(1,3)], 1)
} else if (shipcol == LETTERS[my.cols] & shiprow == my.rows) { #bottomright
 direction2 <- sample(direction[-c(2,4)], 1)
} else if (shipcol == "A" & shiprow == my.rows) { #bottomleft
 direction2 <- sample(direction[-c(1,4)], 1)
} else if (shipcol == LETTERS[my.cols] & shiprow == 1) { #topright
 direction2 <- sample(direction[-c(2,3)], 1)
} else if (shipcol == "A") {
 direction2 <- sample(direction[-1], 1)
} else if (shipcol == LETTERS[my.cols]) {
 direction2 <- sample(direction[-2], 1)
} else if (shiprow == 1) {
 direction2 <- sample(direction[-3], 1)
} else if (shiprow == my.rows) {
 direction2 <- sample(direction[-4], 1)
} else {
 direction2 <- sample(direction, 1)
}

print(direction2)

#creating next part(s) of ship based on shipsize
if (direction2 == 'left') {
 shipcol2 = LETTERS[which(LETTERS == shipcol) - 1]
 shiprow2 = shiprow
 if (shipsize == 3 & shipcol2 == 'A') {
  shipcol3 = LETTERS[which(LETTERS == shipcol) + 1]
  shiprow3 = shiprow
 } else if (shipsize == 3) {
  shipcol3 = LETTERS[which(LETTERS == shipcol2) - 1]
  shiprow3 = shiprow
 }
} else if (direction2 == 'right') {
 shipcol2 = LETTERS[which(LETTERS == shipcol) + 1]
 shiprow2 = shiprow
 if (shipsize == 3 & shipcol2 == LETTERS[my.cols]) {
  shipcol3 = LETTERS[which(LETTERS == shipcol) - 1]
  shiprow3 = shiprow
 } else if (shipsize == 3) {
  shipcol3 = LETTERS[which(LETTERS == shipcol2) + 1]
  shiprow3 = shiprow
 }
} else if (direction2 == 'up') {
 shipcol2 = shipcol
 shiprow2 = shiprow - 1
 if (shipsize == 3 & shiprow2 == 1) {
  shipcol3 = shipcol
  shiprow3 = shiprow - 1
 } else if (shipsize == 3) {
  shipcol3 = shipcol
  shiprow3 = shiprow + 1
 }
} else if (direction2 == 'down') {
 shipcol2 = shipcol
 shiprow2 = shiprow + 1
 if (shipsize == 3 & shiprow2 == 1) {
  shipcol3 = shipcol
  shiprow3 = shiprow + 1
 } else if (shipsize == 3) {
  shipcol3 = shipcol
  shiprow3 = shiprow - 1
 }
}

if (shipsize == 2) {
 print(c(shipcol, shiprow), quote = FALSE)
 print(c(shipcol2, shiprow2), quote = FALSE)
} else if (shipsize == 3) {
 print(c(shipcol, shiprow), quote = FALSE)
 print(c(shipcol2, shiprow2), quote = FALSE)
 print(c(shipcol3, shiprow3), quote = FALSE)
}

parts_left = shipsize
print("Time to guess where the ship is!")

while(parts_left != 0) {

print("What is your guess?")

my.colguess <- readline(prompt="Enter column letter: ")
my.rowguess <- as.numeric(readline(prompt="Enter row number: "))

if ((my.colguess == shipcol & my.rowguess == shiprow) | 
    (my.colguess == shipcol2 & my.rowguess == shiprow2) | 
    (my.colguess == shipcol3 & my.rowguess == shiprow3)) {
 print("Nice! You got part of the ship!")
 board[my.rowguess,my.colguess] <- "[O]"
 print(board, quote = FALSE)
 parts_left = parts_left - 1
 cat("There are", parts_left, "parts left!", "\n")

} else {
 print("Aw you missed!")
 board[my.rowguess,my.colguess] <- "[X]"
 print(board, quote = FALSE)
 cat("There are still", parts_left, "parts left!", "\n")
}

}

if (parts_left == 0) {
 print("Congrats you win!")
}