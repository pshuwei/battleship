##' Play the battleship game
##'
##' Randomly hides a ship of size 2 or 3 onto a board size of m rows and n columns and it's down to you to find the ship!
##' The ship can only be placed horizontally or vertically, never diagonally.
##' @param rows number of rows for your board
##' @param columns number of columns for your board
##' @return X you missed and no parts of the ship were struck
##' @return O you struck part of the ship
##' @author Peter Chang
##' @example
##' play(5,5) creates a 5x5 board
##' @export

play <- function(rows, columns) {

  stopifnot(is.numeric(rows), is.numeric(columns), rows >1, columns >1, columns <= 26)

  my.rows <- rows

  my.cols <- columns

  cat("Welcome to Battleship!", "\n")

  cat("You have decided to have a board size of", my.rows, "rows and", my.cols, "columns.", "\n")

  bracket <- "[ ]"

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

  #randomly generates how big the ship will be
  cat("Generating ship size...", "\n")

  if (nrow(board) < 3 | ncol(board) < 3 ) {
    shipsize <- 2
  } else{
    shipsize <- sample(2:3, 1)
  }

  cat("The size of the ship is", shipsize, "units", "\n")

  #randomly generating first part of ship
  shiprow <- sample(1:my.rows, 1)
  shipcol <- sample(LETTERS[1:my.cols], 1)

  #cat(shipcol, shiprow, shipsize, "\n")

  direction <- c("left", "right", "up", "down")

  #randomly deciding how to extend the ship in what direction
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

  #print(direction2)

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

  #if (shipsize == 2) {
    #print(c(shipcol, shiprow), quote = FALSE)
    #print(c(shipcol2, shiprow2), quote = FALSE)
  #} else if (shipsize == 3) {
    #print(c(shipcol, shiprow), quote = FALSE)
    #print(c(shipcol2, shiprow2), quote = FALSE)
    #print(c(shipcol3, shiprow3), quote = FALSE)
  #}

  parts_left = shipsize
  cat("There is an enemy ship of size", shipsize, "in the water.", "\n")
  cat("Captain, you must destroy it!", "\n")

  while(parts_left != 0) {

    cat("What is your guess?", "\n")

    #my.colguess <- toupper(readline(prompt="Enter column letter: "))

    my.colguess <- "whatever"
    outside <- TRUE
    options(warn=-1)
    while ((my.colguess %in% LETTERS) == F | outside == TRUE) {
      my.colguess <- toupper(readline(prompt="Enter column letter: "))
      if ((my.colguess %in% LETTERS) == F) {
        cat("That's not a letter!", "\n")
      } else if (which(LETTERS == my.colguess) > my.cols) {
        cat("Your letter is outside the board!")
      } else {
        outside <- FALSE
      }

    }
    options(warn=0)

    #my.rowguess <- as.numeric(readline(prompt="Enter row number: "))

    my.rowguess <- "whatever"
    options(warn=-1)
    while(is.na(my.rowguess)|!is.numeric(my.rowguess) | my.rowguess > my.rows) {
      my.rowguess <- as.numeric(readline(prompt = "Enter row number:"))
      if (is.na(my.rowguess)|!is.numeric(my.rowguess)) {
        cat("That's not a number!")
      } else if (my.rowguess > my.rows) {
        cat("Your guess is outside the board!")
      }
    }
    options(warn=0)

    if (shipsize == 2) {
      if ((my.colguess == shipcol & my.rowguess == shiprow) |
          (my.colguess == shipcol2 & my.rowguess == shiprow2)) {
        cat("Nice! You got part of the ship!", "\n")
        board[my.rowguess,my.colguess] <- "[O]"
        print(board, quote = FALSE)
        parts_left = parts_left - 1
        cat("There are", parts_left, "parts left!", "\n")

      } else {
        cat("Aw you missed!", "\n")
        board[my.rowguess,my.colguess] <- "[X]"
        print(board, quote = FALSE)
        cat("There are still", parts_left, "parts left!", "\n")
      }

    } else if (shipsize == 3) {
      if ((my.colguess == shipcol & my.rowguess == shiprow) |
          (my.colguess == shipcol2 & my.rowguess == shiprow2) |
          (my.colguess == shipcol3 & my.rowguess == shiprow3)) {
        cat("Nice! You got part of the ship!", "\n")
        board[my.rowguess,my.colguess] <- "[O]"
        print(board, quote = FALSE)
        parts_left = parts_left - 1
        cat("There are", parts_left, "parts left!", "\n")

      } else {
        cat("Aw you missed!", "\n")
        board[my.rowguess,my.colguess] <- "[X]"
        print(board, quote = FALSE)
        cat("There are still", parts_left, "parts left!", "\n")
      }

    }

  }

  if (parts_left == 0) {
    cat("Congrats you win!", "\n")
  }
}
