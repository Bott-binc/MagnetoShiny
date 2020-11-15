library(tools)


envelopegapfiller <- function(x, y, nCol = NULL) {
  if (x[1] != 0) { # ensure that we are at the correct starting height
    x <- c(0,x) # creates first point
    y <- c(y[1],y) # matches height of user selected first point
  }
  diffx <- diff(x)
  diffy <- diff(y)
  slopes <- diffy / diffx # slope of the y = mx+b line between each points

  xNumFills <- diff(x) - 1 # how many places we have to fill

  # Extending the points to make continuous ------------------------------------
  newX <- vector()
  newY <- vector()
  for (i in 1:(length(x) - 1)) {
    if (xNumFills[i] != 0) {
      newX <- append(newX, x[i])
      newY <- append(newY, y[i])
      newX <- append(newX, rep(NA, times = xNumFills[i]))
      newY <- append(newY, rep(NA, times = xNumFills[i]))
    }
    else {
      newX <- append(newX, x[i])
      newY <- append(newY, y[i])
    }

  }
  newX <- append(newX, x[length(x)])
  newY <- append(newY, y[length(x)])

  #adding the numbers in the correct place for the X values --------------------
  for (i in 1:length(newX)) {
    if (is.na(newX[i])) {
      newX[i] = newX[i - 1] + 1
    }
  }

  #Matching the length of the nCol specified -----------------------------------

  if (!is.null(nCol)) {
    if (newX[length(newX)] > nCol) {
      outX <- newX[1:nCol] # just removes any data outside of the end length
      outY <- newY[1:nCol]
    }
    else {
      xAdditions <- seq(from = (newX[length(newX)] + 1), to = nCol)
      outX <- c(newX, xAdditions)
      outY <- c(newY, rep(NA, length = length(xAdditions)))
    }
  }
  else{
    outX <- newX
    outY <- newY
  }

  # Start of adding the heights from the slope ---------------------------------
  k <- 0
  indicator = FALSE
  for (i in 1:(length(outY) - sum(xNumFills))) {
    if (i == length(xNumFills)) {
      k <- k + 1
      intersept <- outY[k] - (outX[k]*slopes[length(xNumFills)])
      next
    }
    else if (i > length(xNumFills)) {
      k <- k + 1
      if (!indicator) {
        outY[k] <- outX[k] * slopes[length(xNumFills)] +
          intersept
        indicator <- TRUE
      }else{
        outY[k] <- outY[k - 1]
      }
    }
    else if (xNumFills[i] == 0) {
      k <- k + 1
      intersept <- outY[k] - (outX[k] * slopes[i])
      next
    }
    else{
      for (j in 1:(xNumFills[i] + 1 )) {
        k <- k + 1
        if (j == 1) {
          intersept <- outY[k] - (outX[k] * slopes[i])
        }
        else {
          outY[k] <- outX[k]*slopes[i] + intersept
        }
      }
    }
  }


  return(data.frame(x = outX, y = outY))

}
