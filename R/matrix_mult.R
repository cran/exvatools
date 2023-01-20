# ********************************************
# MATRIX TOOLS (NOT REQUIRING GEOSEC INFO)
# ********************************************

#' Diagonalize the sums of columns of a matrix
#'
#' @description
#' Makes a diagonal matrix with the sums of columns of a matrix.
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


#' Multiply a diagonal matrix by another matrix
#'
#' @description
#' Fast multiplication of a diagonal matrix by another matrix, taking
#' taking advantage of the properties of diagonal matrices.
#' @param matrix1 A diagonal matrix.
#' @param matrix2 An ordinary matrix.
#'
#' @return Product of matrix1 and matrix2.
#' @details
#' `dmult()` will turn `matrix1` into a vector and multiply it
#'   horizontally by every rows in `matrix2`. This saves precious computing
#'   time.\
#' The number of rows and columns of the diagonal `matrix1` must be
#'   equal to the number of rows of `matrix1`.
#' @seealso [multd()].
#' @export
#' @examples
#' wio <- make_wio("wiodtest")
#' dmult(wio$W, wio$Bd)
dmult <- function(matrix1, matrix2){

  # row_names <- rownames(matrix2)
  # col_names <- colnames(matrix2)
  arr <- sweep(matrix2, 1, colSums(matrix1), "*")
  # name row and columns
  # dimnames(arr) <- list(row_names, col_names)
  return(arr)

}


#' Multiply a matrix by a diagonal matrix
#' @description
#' Fast multiplication of a matrix by a diagonal matrix, taking advantage
#'  of the properties of diagonal matrices.
#' @param matrix1 An ordinary matrix.
#' @param matrix2 A diagonal matrix.
#'
#' @return The product of matrix1 and matrix2.
#' @details
#' `multd()` will turn `matrix2` into a vector and multiply it
#' horizontally by every row in `matrix1`. This saves precious computing
#' time.\
#' The number of columns of `matrix1` must be equal to the rows and
#' columns of diagonal `matrix2`.
#' @seealso [dmult()].
#' @export
#' @examples
#' wio <- make_wio("wiodtest")
#' multd(wio$B, wio$E)
multd <- function(matrix1, matrix2){

  # row_names <- rownames(matrix2)
  # col_names <- colnames(matrix2)
  arr <- t(sweep(t(matrix1), 1, colSums(matrix2), "*"))
  # name row and columns
  # dimnames(arr) <- list(row_names, col_names)
  return(arr)

}


#' Hadamard product of matrices
#'
#' @description
#' Hadamard product, i.e., element by element product of matrix `df1`
#'   and matrix `df2` (by blocks). Both matrices must be block matrices, and the
#'   number and dimension of blocks in matrix `df1` and `df2` must be
#'   compatible.
#' @param df1 A block matrix with named rows and columns (country/sector)
#' @param df2 A block matrix with named rows and columns (country/sector)
#'
#' @return Hadamard product of the two matrices.
#' @details In a Hadamard product, matrices are multiplied block by block,
#'   i.e., `block (s,r) %*% block(s,r)`.
#' @export
hmult <- function(df1, df2){


  # function(df1, df2, form="cs_cs_by_cs_c", wio_object=NULL)
  # Multiplication block by block
  # Equivalent to do, in cs_cs_by_cs_c
  # conv(matrix1 * bktt(repmat(matrix2, N)), "cs_cs_to_cs_c")
  # or, in cs_cs_by_cs_cs
  # conv(matrix1 * bktt(matrix2), "cs_cs_to_cs_c")
  # Only possible with pairs of block-matrices with
  # same number of countries in rows and columns

  dfinfo1 <- get_df_info(df1)
  dfinfo2 <- get_df_info(df2)

  if  (nrow(df1) != nrow(df2)) {
    stop(paste0("Dimensions of matrices are not compatible.\n",
                "Number of countries in rows of ", deparse(substitute(df1)),
                " is different from that of ", deparse(substitute(df2))))
  }

  # If number of countries in columns of df2 is bigger
  if  (dfinfo1$c2 != dfinfo2$c2) {
    stop(paste0("Dimensions of matrices are not compatible.\n",
                "Number of countries in columns of ", deparse(substitute(df1)),
                " is different from that of ", deparse(substitute(df2))))
  }

  # Dimensions of result are determined by matrix 2
  c1 <- dfinfo2$c1  # G or GX
  c2 <- dfinfo2$c2  # G or GX
  s1 <- N <- dfinfo2$s1  # N
  s2 <- dfinfo2$s2  # N or 1
  dims_df1 <- dfinfo1$dimensions
  dims_df2 <- dfinfo2$dimensions

  # cs_cs by cs_c
  if (all(dims_df1 == "cs_cs", dims_df2 == "cs_c")) {
    arr <- matrix(0, c1 * s1, c2)
    dimnames(arr) <- list(rownames(df2), colnames(df2))
    for(s in 1:c1){
      m <- (s - 1) * N + 1
      n <- (s - 1) * N + N
      for(r in 1:c2){
        p <- (r - 1) * N + 1
        q <- (r - 1) * N + N
        arr[m:n, r] <- df1[m:n, p:q] %*% df2[m:n, r]
      }
    }
    # cs_cs by cs_cs
    # cambiado a sugerencia de JSS
  } else if (all(dims_df1 == "cs_cs", dims_df2 == "cs_cs")) {
    arr <- matrix(0, c1 * s1, c2 * s2)
    dimnames(arr) <- list(rownames(df2), colnames(df2))
    for(s in 1:c1){
      m <- (s - 1) * N + 1
      n <- (s - 1) * N + N
      for(r in 1:c2){
        p <- (r - 1) * N + 1
        q <- (r - 1) * N + N
        arr[m:n, p:q] <- df1[m:n, p:q] %*% df2[m:n, p:q]
      }
    }
  } else {
    stop("Dimensions of matrices are not compatible")
  }

  return(arr)

}

