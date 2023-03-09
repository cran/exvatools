
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


#' @title repeachc
#' @description Extend a matrix by repeating each column
#' a number of times defined by `times`
#' @param df A matrix
#' @param times Number of times each column is repeated
#' @keywords internal
#' @noRd
#' @return Extended matrix with columns repeated
repeachc <- function(df, times = 1){

  df <- as.matrix(df) #If it's a vector
  row_names <- rownames(df)
  numcols <- ncol(df)
  arr <- NULL
  for (j in 1:numcols) {
    for (c in 1:times) {
      arr <- cbind(arr, df[, j, drop = FALSE])
    }
  }
  rownames(arr) <- row_names
  return(arr)
  # if(numcols == 1){
  #   arr <- matrix(0, numrows, times)
  #   for(j in 1:times){
  #     arr[, j] <- df[, 1]
  #   }
  # }
  # else if(numcols > 1){
  #   arr <- matrix(0, numrows, times * numcols)
  #   for(j in 1:numcols){
  #     p <- (j - 1) * times + 1
  #     q <- (j - 1) * times + times
  #     arr[, p:q] <- df[, j]
  #   }
  # }
  # return(arr)
}


#' @title repeachr
#' @description Extend a matrix by repeating each row
#' a number of times defined by `times`
#' @param df A matrix
#' @param times Number of times each row is repeated
#' @keywords internal
#' @noRd
#' @return Extended matrix with rows repeated
repeachr <- function(df, times = 1){

  df <- as.matrix(df) #If it's a vector
  col_names <- colnames(df)
  numrows <- nrow(df)
  arr <- NULL
  for (i in 1:numrows) {
    for (r in 1:times) {
      arr <- rbind(arr, df[i, , drop = FALSE])
    }
  }
  colnames(arr) <- col_names
  return(arr)

}
