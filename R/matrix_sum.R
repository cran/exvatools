#' Diagonalize the sums of each column of a matrix
#'
#' @description
#' Diagonalizes the sums of each column of a matrix.
#' @param df A matrix with named rows and columns.
#' @return A diagonal matrix with the sums of columns in the diagonal.
#' @export
#' @examples
#' wio <- make_wio("wiodtest")
#' diagcs(wio$W %*% wio$Bd)
diagcs <- function(df) {

  # The dimension of the final matrix is defined by the
  # number of columns of the matrix, i.e., a matrix of 3x10
  # will become 10x10, whereas a 10x3 will become 3x3
  if (ncol(df) > 1) {
    row_names <- col_names <- colnames(df)
    arr <- diag(colSums(df))
    dimnames(arr) <- list(row_names, col_names)
  } else {
    row_names <- col_names <- colnames(df)
    arr <- as.matrix(sum(df))
    dimnames(arr) <- list(row_names, col_names)
  }

  return(arr)

}


#' Sum matrix rows and assign name to resulting column
#'
#' Improved version of `rowSums()` for matrix output. The sum of rows is kept
#'   as a column vector with rows names and the resulting column can be
#'   named in the same command.
#' @param df A matrix with named rows and columns.
#' @param col_name String, name to assign to resulting column.
#'
#' @return A column matrix (with rows and column names)
#' @export
#' @examples
#' wio <- make_wio("wiodtest", quiet = TRUE)
#' rsums(wio$Y, "Y")
rsums <- function(df, col_name = NULL) {

  row_names <- rownames(df)
  result <- as.matrix(rowSums(df))
  rownames(result) <- row_names
  if (!is.null(col_name)) {
    colnames(result) <- col_name
  }

  return(result)

}


#' Sum matrix columns and assign name to resulting row
#'
#' Improved version of `colSums()` for matrix output. The sum of columns is
#'   kept as a row vector with column names and the resulting row can be
#'   named in the same command.
#' @param df A matrix with named rows and columns.
#' @param row_name String, name to assign to resulting row.
#'
#' @return A row matrix (with rows and column names)
#' @export
#' @examples
#' wio <- make_wio("wiodtest", quiet = TRUE)
#' csums(wio$Y, "TOTAL_Y")
csums <- function(df, row_name = NULL) {

  col_names <- colnames(df)
  result <- t(as.matrix(colSums(df)))
  if (!is.null(row_name)) {
    rownames(result) <- row_name
  }
  colnames(result) <- col_names
  return(result)

}


#' Sum every nth row of a matrix and name the resulting rows
#'
#' @description
#' Groups a matrix by rows, summing every `Nth` row. Matrix should be multiple
#'   of `N`.
#' @param df A matrix with named rows and columns.
#' @param N Integer, specifying the resulting number or rows.
#' @param row_names String vector of length `N`, with names to assign to the
#'   resulting rows.
#'
#' @return A matrix with `N` rows, where each row is the sum of every `Nth` row
#'   of the original matrix.
#' @export
#' @examples
#' wio <- make_wio("wiodtest", quiet = TRUE)
#' sumnrow(wio$Y, wio$dims$N, paste0("WLD", "_",
#'         gsub("^D", "", wio$names$n_names)))
sumnrow <- function(df, N, row_names = NULL) {

  col_names <- colnames(df)

  result <- sum_every_nth_row(df, N)

  if (!is.null(row_names)) {
    rownames(result) <- row_names
  }
  colnames(result) <- col_names

  return(result)

}


#' Sum every nth column of a matrix and name the resulting columns
#'
#' @description
#' Groups a matrix by columns, summing every Nth column. Matrix should be
#'   multiple of N.
#' @param df A matrix with named rows and columns.
#' @param N Integer, specifying the resulting number of columns.
#' @param col_names String vector of length N, with names to assign to the
#'   resulting columns.
#'
#' @return A matrix with N columns, where each columns is the sum of
#'   every Nth column of the original matrix.
#' @export
#' @examples
#' wio <- make_wio("wiodtest", quiet = TRUE)
#' sumncol(wio$Yfd, wio$dims$FD, paste0("WLD", "_", wio$names$fd_names))
sumncol <- function(df, N, col_names = NULL) {

  row_names <- rownames(df)

  result <- sum_every_nth_col(df, N)

  rownames(result) <- row_names
  if (!is.null(col_names)) {
    colnames(result) <- col_names
  }

  return(result)

}


#' Sum groups of rows of a matrix and name the resulting rows
#'
#' @description
#' Groups a matrix by rows, summing blocks of rows of size n each.
#'   Matrix rows should be multiple of n.
#' @param df A matrix with named rows and columns.
#' @param n Integer, specifying the size of each group.
#' @param row_names String vector of length n, with names to assign to the
#'   resulting rows.
#'
#' @return A matrix where each row is the sum of groups of n rows
#'   of the original matrix.
#' @export
#' @examples
#' wio <- make_wio("wiodtest", quiet = TRUE)
#' sumgrows(wio$Y, wio$dims$N, wio$names$g_names)
sumgrows <- function(df, n, row_names = NULL) {

  col_names <- colnames(df)

  numrows <- nrow(df)
  numcols <- ncol(df)

  if (numrows%%n != 0) stop("Number of rows must be multiple of n")

  # Get number of blocks
  m <- numrows / n

  # Initialize matrix
  arr <- matrix(0, m, numcols)
  # For each block
  for (i in 1:m) {
    # Get initial and final row for the block
    p <- (i - 1) * n + 1
    q <- n * i
    # The row will be the colsums of
    arr[i, ] <- csums(df[p:q, , drop = FALSE])
  }

  dimnames(arr) <- list(row_names, col_names)

  return(arr)

}


#' Sum groups of columns of a matrix and name the resulting columns
#'
#' @description
#' Groups a matrix by columns, by summing blocks of columns of size n each.
#'   Matrix columns should be multiple of n.
#' @param df A matrix with named rows and columns.
#' @param n Integer, specifying the size of each group.
#' @param col_names String vector of length n, with names to assign to the
#'   resulting columns.
#'
#' @return A matrix where each column is the sum of groups of n
#'   columns of the original matrix.
#' @export
#' @examples
#' wio <- make_wio("wiodtest", quiet = TRUE)
#' sumgcols(wio$Yfd, wio$dims$FD, wio$names$g_names)
sumgcols <- function(df, n, col_names = NULL) {

  row_names <- rownames(df)

  numrows <- nrow(df)
  numcols <- ncol(df)

  if (numcols%%n != 0) stop("Number of columns must be multiple of n")

  # Get number of blocks
  m <- numcols / n

  # Initialize matrix
  arr <- matrix(0, numrows, m)
  # For each block
  for (j in 1:m) {
    # Get initial and final column for the block
    p <- (j - 1) * n + 1
    q <- n * j
    # The row will be the colsums of
    arr[, j] <- rsums(df[, p:q, drop = FALSE])
  }

  dimnames(arr) <- list(row_names, col_names)

  return(arr)

}




# ****************************
# Auxiliary, not exported----
# ****************************


#' @title group_cols
#' @description Aggregate colums by country groups
#' i.e. transforms CAN, MEX and USA columns in NAFTA column
#' @param df A matrix with named columns
#' @param group Can be group code (e.g., EU27, NAFTA...), position of columns
#'   (e.g., c(1, 2, 3)) or vector (e.g., c("AUS", "ESP"))
#' @param mode Get only grouped columns ("get") or replace them ("replace")
#' @param group_name Name to be given to group (if null, code group)
#' @param wiotype Source database
#' @keywords internal
#' @noRd
#' @return Aggregated matrix of/with grouped columns
#' @examples
#' \dontrun{
#' group_cols(df, "EU27", "replace")
#' }
group_cols <- function(df, group, mode="get",
                       group_name = NULL, wiotype = "icio2021"){

  # Group can be id (geo_id), position (pgn_exp) (most typical)
  # or vector c("AUS", "ESP")
  # Requires: get_geo_codes

  row_names <- rownames(df)
  col_names <- colnames(df)

  dfinfo <- get_df_info(df)

  if(is.null(group_name)){
    group_name <- group
  }

  # # If group is a geo id i.e. "EU27", "NAFTA", etc
  if (all(is.character(group), length(group) == 1)) {
    # If group is a group code i.e. "EU27", "NAFTA", etc
    # Get the list of countries from dbgeo with function get_geo_codes
    # and find the position in the columns of the df
    pos_cols <- grep(get_geo_codes(group, wiotype, icio_extend = TRUE),
                     col_names)

    # If group is a vector c("AUS", "BEL") etc
  } else if (all(is.character(group), length(group) > 1)) {
    # If group is a vector c("AUS", "BEL") etc
    # Get the position of the vector elements
    # by collapsing to c("AUS|BEL")
    pos_cols <- paste0(group, collapse = "|")
    # before: with  my grepv (grep for vectors)
    # pos_cols <- grepv(group, colnames(df))

    # If group is position (e.g. pgn_exp in icio)
  } else if (all(is.numeric(group), length(group) > 1)) {
    # If is position leave as it is
    pos_cols <- group
  }

  # Sum the values of the columns (by row) in variable cols
  if (dfinfo$dimensions == "cs_c") {
    # For compatibility and lists of countries
    group_colnames <- group_name
    sum_cols <- name(as.matrix(rowSums(df[, pos_cols])),
                     row_names, group_colnames)
  } else if (dfinfo$dimensions == "cs_cs") {
    group_colnames <- paste0(group_name, "_", dfinfo$s2_names)
    sum_cols <- name(sum_every_nth_col(df[, pos_cols], dfinfo$s2),
                     row_names, group_colnames)
  } else {
    stop("Group not found")
  }

  # Modes
  if(mode == "get"){
    df <- sum_cols
    colnames(df) <- group_name
  } else if(mode == "add_last"){
    df <- cbind(df, sum_cols)
    colnames(df) <- c(col_names, group_name)
  } else if(mode == "add_first"){
    df <- cbind(sum_cols, df)
    colnames(df) <- c(group_name, col_names)
  } else if (mode == "replace"){
    df <- df[, -pos_cols]
    df <- cbind(df, sum_cols)
    col_names <- col_names[-pos_cols]
    colnames(df) <- c(col_names, group_colnames)
  }

  # rownames(df) <- row_names

  return(df)

}





#' @title sum_every_nth_row
#' @description Aggregate matrix by summing every nth row
#' Takes an array df and sums its rows for each column
#' so the first row is 1+(n+1) + 2n+1, second is 2+(n+2)+2n+2...etc
#' Useful to sum from country-sector to (sum of countries)-sector
#' @param df A matrix
#' @param n Aggregate every nth row, generally `N` (number of sectors)
#' @keywords internal
#' @noRd
#' @return Aggregated matrix by rows
sum_every_nth_row <- function(df, n){

  df <- as.matrix(df)
  # Get number of rows and columns
  numrows <- nrow(df)
  numcols <- ncol(df)

  if (numrows%%n != 0) stop("Number of rows must be multiple of n")

  # Number of row blocks
  g <- numrows / n

  # Create matrix with n rows (not g!)
  arr <- matrix(0, n, numcols)

  #For each sector
  for(i in 1:n){
    # Initialize a submatrix of 1 row and numcols columns
    # for sector i
    arr[i, ] <- matrix(0, 1, numcols)
    # For each block
    for(k in 1:g){
      # Position of sector i in block k (in the total matrix)
      m <- (k - 1) * n + i
      # Add values of row of sector i in block k
      arr[i, ] <- arr[i, ] + df[m, , drop = FALSE]
    }
  }

  return(arr)

}


#' @title sum_every_nth_col
#' @description Aggregate matrix by suuming every nth column
#' @param df A matrix
#' @param n Aggregate every nth column
#' @keywords internal
#' @noRd
#' @return Aggregated matrix by columns
sum_every_nth_col <- function(df, n){

  # sum_every:nth_col: Takes an array df
  # and sums its cols for each row
  # so the first col is 1+(n+1)+2n+1,
  # second col is 2+(n+2)+2n+2...etc
  # Useful to sum from country-sector
  # to (sum of countries)-sector by columns

  # Get number of rows and columns
  numrows <- nrow(df)
  numcols <- ncol(df)

  if(numcols%%n != 0) stop("Number of rows must be multiple of n")

  # Number of row blocks
  g <- numcols / n

  # Create matrix with n columns (not g!)
  arr <- matrix(0, numrows, n)

  #For each sector/fd
  for(j in 1:n){
    # Initialize a submatrix of numrows rows and 1 column
    # for sector i
    arr[, j] <- matrix(0, numrows, 1)
    # For each block
    for(k in 1:g){
      # Position of sector j in block k (in the total matrix)
      m <- (k - 1) * n + j
      # Add values of column of sector j in block k
      arr[, j] <- arr[, j] + df[, m, drop = FALSE]
    }
  }

  return(arr)

}


#' @title sum_by_groups_of
#' @description Sum rows or columns by groups
#' of size n
#' @param df A matrix
#' @param n Size of the groups to be aggregated
#' @param bycols Boolean. If TRUE, aggregate by columns
#' @keywords internal
#' @noRd
#' @return Aggregated matrix by rows
sum_by_groups_of <- function(df, n, bycols=FALSE){
  # Takes an array df and sums its rows for each column
  # so the first row is 1:n, the secons n+1:2n... etc
  # Useful to sum from country-sector to countries
  numrows <- nrow(df)
  numcols <- ncol(df)
  row_names <- rownames(df)
  col_names <- colnames(df)

  if (bycols==FALSE){
    # Group by rows
    if(numrows%%n != 0) stop("Number of rows must be multiple of n")
    m <- numrows/n
    arr <- matrix(0, m, numcols)
    for(i in 1:m){
      p <- (i-1)*n + 1
      q <- n*i
      for(j in 1:numcols){
        arr[i, j] <- sum(df[p:q, j])
      }
    }
    colnames(arr) <- col_names
  } else {
    # Group by columns
    if(numcols%%n != 0) stop("Number of rows must be multiple of n")
    m <- numcols/n
    arr <- matrix(0, numrows, m)
    for(i in 1:numrows){
      for(j in 1:m){
        p <- (j-1)*n + 1
        q <- n*j
        arr[i, j] <- sum(df[i, p:q])
      }
    }
    rownames(arr) <- row_names
  }
  return(arr)
}



