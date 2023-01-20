
#' @title repmat
#' @description Extends a matrix or vector repeating it
#' a number of times defined by times
#' @param df A matrix
#' @param times Number of times the matrix is repeated
#' @param bycols Boolean, by default (TRUE) matrices will
#' be added by vertically. If FALSE, matrix will be added
#' horizontally
#' @keywords internal
#' @noRd
#' @return Extended matrix df with columns repeated
repmat <- function(matrix, times, bycols = TRUE){
  arr <- NULL
  for (i in 1:times){
    if (bycols == TRUE) {
      arr <- cbind(arr, matrix)
    } else {
      arr <- rbind(arr, matrix)
    }
  }
  return(arr)
}


#' @title repeach
#' @description Extend a matrix by repeating each column
#' a number of times defined by n_times
#' @param df A matrix
#' @param times Number of times each column is repeated
#' @keywords internal
#' @noRd
#' @return Extended matrix df with columns repeated
repeach <- function(df, times){
  # Repeats each column of a matrix df
  # a number specified by times
  # With a column vector (1 column),
  # it repeats the vector
  df <- as.matrix(df) #If it's a vector
  numrows <- nrow(df)
  numcols <- ncol(df)
  if(numcols == 1){
    arr <- matrix(0, numrows, times)
    for(j in 1:times){
      arr[, j] <- df[, 1]
    }
  }
  else if(numcols > 1){
    arr <- matrix(0, numrows, times * numcols)
    for(j in 1:numcols){
      p <- (j - 1) * times + 1
      q <- (j - 1) * times + times
      arr[, p:q] <- df[, j]
    }
  }
  return(arr)
}
