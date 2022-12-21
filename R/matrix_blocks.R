#' Get block diagonal matrix
#'
#' @description
#' Produces a block diagonal matrix version of block matrix, i.e., a matrix
#'   in which the diagonal blocks are non-zero and the off-diagonal blocks
#'   are zero
#' @param df A block matrix with named rows and columns. Names of countries
#'   and sectors are automatically identified.
#' @return Block diagonal version of the original matrix.
#' @export
#' @examples
#' wio <- make_wio("wiodtest", quiet = TRUE)
#' # Block diagonal version of Y (coincides with Yd)
#' bkd(wio$Y)
bkd <- function(df){

  dfinfo <- get_df_info(df)

  c1 <- dfinfo$c1
  s1 <- dfinfo$s1
  c2 <- dfinfo$c2
  s2 <- dfinfo$s2
  row_names <- rownames(df)
  col_names <- colnames (df)

  # Initialize matrix
  arr <- matrix(0, c1 * s1, c2 * s2)

  # Name rows and columns
  dimnames(arr) <- list(rownames(df), colnames(df))

  for (country in dfinfo$c1_names) {
    prc <- grep(country, row_names)
    pcc <- grep(country, col_names)
    if (all(length(prc) > 0, length(pcc) > 0)) {
      arr[prc, pcc] <- df[prc, pcc]
    }
  }

  # # If must be min(c1, c2) because icio can be non-symmetrical
  # c <- min(c1, c2)
  # for (i in 1:c){
  #   m <- s1 * (i - 1) + 1  # Row from
  #   n <- s1 * (i - 1) + s1  # Row until
  #   p <- s2 * (i - 1) + 1  # Column from
  #   q <- s2 * (i - 1) + s2  # Column until
  #   arr[m:n, p:q] <- df[m:n, p:q]
  # }

  # Correction

  if (dfinfo$is_icio) {
    # MEX
    if (all(dfinfo$rows_have_MX, dfinfo$cols_have_MX)) {
      arr[dfinfo$prMXall, dfinfo$pcMXall] <- df[dfinfo$prMXall, dfinfo$pcMXall]
    } else if (all(dfinfo$rows_have_MX, !dfinfo$cols_have_MX)) {
      arr[dfinfo$prMXall, dfinfo$pcMEX] <- df[dfinfo$prMXall, dfinfo$pcMEX]
    } else if (all(!dfinfo$rows_have_MX, dfinfo$cols_have_MX)) {
      arr[dfinfo$prMEX, dfinfo$pcMXall] <- df[dfinfo$prMEX, dfinfo$pcMEXall]
    }
    # CHN
    if (all(dfinfo$rows_have_CN, dfinfo$cols_have_CN)) {
      arr[dfinfo$prCNall, dfinfo$pcCNall] <- df[dfinfo$prCNall, dfinfo$pcCNall]
    } else if (all(dfinfo$rows_have_CN, !dfinfo$cols_have_CN)) {
      arr[dfinfo$prCNall, dfinfo$pcCHN] <- df[dfinfo$prCNall, dfinfo$pcCHN]
    } else if (all(!dfinfo$rows_have_CN, dfinfo$cols_have_CN)) {
      arr[dfinfo$prCHN, dfinfo$pcCNall] <- df[dfinfo$prCHN, dfinfo$pcCHNall]
    }
  }

  return(arr)

}


#' Get block off-diagonal matrix
#'
#' @description
#' Produces a block off-diagonal matrix version of block matrix, i.e., a matrix
#'   in which the diagonal blocks are zero and the off-diagonal blocks
#'   are non-zero.
#' @param df A block matrix with named rows and columns. Names of countries
#'   and sectors are automatically identified.
#' @return Block off-diagonal version of the original matrix.
#' @export
#' @examples
#' wio <- make_wio("wiodtest", quiet = TRUE)
#' # Block off-diagonal version of Y (coincides with Ym)
#' bkoffd(wio$Y)
bkoffd <- function(df){

  dfinfo <- get_df_info(df)

  c1 <- dfinfo$c1
  s1 <- dfinfo$s1
  c2 <- dfinfo$c2
  s2 <- dfinfo$s2

  row_names <- rownames(df)
  col_names <- colnames(df)

  # Initialize matrix
  arr <- df

  # Name rows and columns
  dimnames(arr) <- list(row_names, col_names)

  for (country in dfinfo$c1_names) {
    prc <- grep(country, row_names)
    pcc <- grep(country, col_names)
    if (all(length(prc) > 0, length(pcc) > 0)) {
      arr[prc, pcc] <- 0
    }
  }

  # # If must be min(c1, c2) because icio can be non-symmetrical
  # c <- min(c1, c2)
  # for (i in 1:c){
  #   m <- s1 * (i - 1) + 1  # Row from
  #   n <- s1 * (i - 1) + s1  # Row until
  #   p <- s2 * (i - 1) + 1  # Column from
  #   q <- s2 * (i - 1) + s2  # Column until
  #   if (dfinfo$c1_names[i] == dfinfo$c2_names[i]) {
  #     arr[m:n, p:q] <- 0
  #   }
  # }

  if (dfinfo$is_icio) {
    # MEX
    if (all(dfinfo$rows_have_MX, dfinfo$cols_have_MX)) {
      arr[dfinfo$prMXall, dfinfo$pcMXall] <- 0
    } else if (all(dfinfo$rows_have_MX, !dfinfo$cols_have_MX)) {
      arr[dfinfo$prMXall, dfinfo$pcMEX] <- 0
    } else if (all(!dfinfo$rows_have_MX, dfinfo$cols_have_MX)) {
      arr[dfinfo$prMEX, dfinfo$pcMXall] <- 0
    }
    # CHN
    if (all(dfinfo$rows_have_CN, dfinfo$cols_have_CN)) {
      arr[dfinfo$prCNall, dfinfo$pcCNall] <- 0
    } else if (all(dfinfo$rows_have_CN, !dfinfo$cols_have_CN)) {
      arr[dfinfo$prCNall, dfinfo$pcCHN] <- 0
    } else if (all(!dfinfo$rows_have_CN, dfinfo$cols_have_CN)) {
      arr[dfinfo$prCHN, dfinfo$pcCNall] <- 0
    }
  }

  return(arr)

}


#' Block transpose matrix
#'
#' Transpose a matrix by blocks, so `block(s,r)` becomes `block(r,s)`, but
#'   elements within each block are not transposed.
#' @param df A block matrix with named rows and columns. Names of countries
#'   and sectors are automatically identified.
#' @return Block transposed version of `df`.
#' @details `bkt()` takes a matrix of `c1 x c2` blocks where each
#'   block has a dimension `s1 x s2` and transposes its blocks. Block
#'   `B21` becomes `B12`, `B31` becomes `B13`, etc., but
#'   blocks are not altered internally. For instance, a matrix with rows
#'   5 exporting countries of 4 sectors each and columns with 3
#'   importing countries with 2 aggregated sectors, i.e., a
#'   (5 x 4) x (3 x 2), matrix will become a (3 x 4) x (5 x 2) matrix. The
#'   rows will now show the importing countries and the sectors they import
#'   from, and the columns will show the the exporting countries and the
#'   sectors they export from.
#' @return A block transposed version of the original matrix.
#' @seealso [bktt()].
#' @export
#' @examples
#' wio <- make_wio("wiodtest", quiet = TRUE)
#' # Matrix Ym (exports of final products)
#' wio$Ym
#' # Block transposed version of Ym (imports of final products)
#' bkt(wio$Ym)
bkt <- function(df){

  dfinfo <- get_df_info(df)

  c1 <- dfinfo$c1
  s1 <- dfinfo$s1
  c2 <- dfinfo$c2
  s2 <- dfinfo$s2
  dimension <- dfinfo$dimensions

  arr <- matrix(0, c2 * s1, c1 * s2)
  for(i in 1:c2){
    m <- (i - 1) * s1 + 1
    n <- i * s1
    for(j in 1:c1){
      p <- (j - 1) * s2 + 1
      q <- j * s2
      r <- (j - 1) * s1 + 1
      s <- j * s1
      t <- (i - 1) * s2 + 1
      v <- i * s2
      # print(paste(m, n, p, q, r, s, t, v))
      # arr[m:n, p:q] <- as.matrix(df[r:s, t:v])
      arr[m:n, p:q] <- df[r:s, t:v]
    }
  }

  # Rename rows and columns, if possible
  if (all(!is.null(rownames(df)), !is.null(colnames(df)))) {
    # Get original names
    row_names <- rownames(df)
    col_names <- colnames(df)
    # Countries: remove "_" followed by any characters
    # Within a range ^ means "not": [^_] means any character not "_"
    # different from ^[_] which means "_" at the beginning of word
    c1_names <- unique(gsub("[_][^_]{1,}", "", row_names))
    c2_names <- unique(gsub("[_][^_]{1,}", "", col_names))
    # sectors: remove any characters not "_" followed by "_"
    s1_names <- unique(gsub("[^_]{1,}[_]", "_", row_names))
    s2_names <- unique(gsub("[^_]{1,}[_]", "_", col_names))
    # If no sector disaggregation, put empty string (length 1)
    if (!substr(s1_names, 1, 1)[1] == "_") {
      s1_names <- ""
    }
    if (!substr(s2_names, 1, 1)[1] == "_") {
      s2_names <- ""
    }
    rownames(arr) <- paste0(rep(c2_names, each=(length(s1_names))), s1_names)
    colnames(arr) <- paste0(rep(c1_names, each=(length(s2_names))), s2_names)
  }

  # Corrections if wio
  if (all(dfinfo$is_icio, dimension == "cs_cs")) {

    # Initial matrix
    prMXall0 <- dfinfo$prMXall
    pcMXall0 <- dfinfo$pcMXall
    prCNall0 <- dfinfo$prCNall
    pcCNall0 <- dfinfo$pcCNall

    # Final matrix (create another dfinfo)
    dfinfo1 <- get_df_info(arr)
    prMXall1 <- dfinfo1$prMXall
    pcMXall1 <- dfinfo1$pcMXall
    prCNall1 <- dfinfo1$prCNall
    pcCNall1 <- dfinfo1$pcCNall

    # MEX
    if (all(length(prMXall0) > 0, length(pcMXall0) > 0,
            length(prMXall0) == length(pcMXall0))) {
      arr[prMXall1, pcMXall1] <- df[prMXall0, pcMXall0]
    }

    # CHN
    prCNall <- dfinfo$prCNall
    pcCNall <- dfinfo$pcCNall
    if (all(length(prCNall0) > 0, length(pcCNall0) > 0,
            length(prCNall0) == length(pcCNall0))) {
      arr[prCNall1, pcCNall1] <- df[prCNall0, pcCNall0]
    }
  }

  # Output
  return(arr)

}


#' Block transpose matrix with transposed blocks
#'
#' @description
#' Block transpose matrix and then transpose each block. `block(s,r)` is
#'   transformed into `block(r,s)` and then internally transposed. This is not
#'   equivalent to directly transpose the matrix.
#' @param df A square block matrix with named rows and columns.
#'   Names of countries and sectors are automatically identified. Unlike
#'   [bkt()], `bktt()` can only be used with square block
#'   matrices with `NxN` blocks (with row and column names in the form
#'   `AUS_01T02`, `AUS_05`, etc.)
#' @return Block transposed version of `df` with elements transposed.
#' @seealso [bkt()].
#' @export
#' @examples
#' wio <- make_wio("wiodtest", quiet = TRUE)
#' # Block-transpose Z and transpose blocks (show first elements only)
#' bktt(wio$Z)[1:6, 1:6]
#' # Note that directly transposing Z produces a different result:
#' t(wio$Z)[1:6, 1:6]
bktt <- function(df){

  dfinfo <- get_df_info(df)

  c1 <- dfinfo$c1
  s1 <- dfinfo$s1
  c2 <- dfinfo$c2
  s2 <- dfinfo$s2
  N <- s1
  dimension <- dfinfo$dimensions

  if (N > 1 && !dimension == "cs_cs") {
    stop("Error in bktt: rows and columns need to be both country/sector")
  }

  row_names <- rownames(df)
  col_names <- colnames(df)

  arr <- matrix(0, c1 * N, c2 * N)
  dimnames(arr) <- list(row_names, col_names)

  for(s in 1:c1){
    m <- (s - 1) * N + 1
    n <- (s - 1) * N + N
    for(r in 1:c2){
      p <- (r - 1) * N + 1
      q <- (r - 1) * N + N
      arr[m:n, p:q] <- t(df[m:n, p:q])
    }
  }

  return(arr)

}


#' Diagonalize blocks of a block matrix
#'
#' @description
#' Diagonalize each block of a block matrix, so sectors of origin become also
#'   sectors of destination. Blocks of dimension `NxN` will remain
#'   `NxN`, but diagonalized, and blocks of dimensions `Nx1` will
#'   be expanded to `NxN` and then diagonalized.
#' @param df A block matrix with named rows and columns.
#' @return Matrix `df` with blocks of dimension `NxN` diagonalized.
#' @export
#' @examples
#' wio <- make_wio("wiodtest", quiet = TRUE)
#' # Normal version of matrix Y
#' wio$Y
#' # Diagonal version (show first columns only)
#' bkdiag(wio$Y)[, 1:6]
bkdiag <- function(df){

  dfinfo <- get_df_info(df)

  c1 <- dfinfo$c1
  s1 <- dfinfo$s1
  c2 <- dfinfo$c2
  s2 <- dfinfo$s2
  dimension <- dfinfo$dimensions

  if (all(s1 == 1, s2 == 1)) {
    return(df)
  }

  # s2 can be 1 or s1, so we choose s1
  arr <- matrix(0, c1 * s1, c2 * s1)
  for(s in 1:c1){
    m <- (s - 1) * s1 + 1
    n <- (s - 1) * s1 + s1
    for(r in 1:c2){
      p <- (r - 1) * s1 + 1
      q <- (r - 1) * s1 + s1
      if (dimension == "cs_c"){
        arr[m:n, p:q] <- diag(df[m:n, r])
      } else if (dimension == "cs_cs"){
        arr[m:n, p:q] <- diag(colSums(df[m:n, p:q]))
      } else if (dimension == "cs") {
        arr[m:n, p:q] <- diag(df[m:n, p:p])
      }
    }
  }

  # Corrections if wio
  if (all(dfinfo$is_icio, dimension == "cs_cs")) {
    # MEX
    prMXall <- dfinfo$prMXall
    pcMXall <- dfinfo$pcMXall
    if (all(length(prMXall) > 0, length(pcMXall) > 0,
            length(prMXall) == length(pcMXall))) {
      arr[prMXall, pcMXall] <- diag(colSums(df[prMXall, pcMXall]))
    }
    # CHN
    prCNall <- dfinfo$prCNall
    pcCNall <- dfinfo$pcCNall
    if (all(length(prCNall) > 0, length(pcCNall) > 0,
            length(prCNall) == length(pcCNall))) {
      arr[prCNall, pcCNall] <- diag(colSums(df[prCNall, pcCNall]))
    }
  }

  # Fix if wio
  # if (all(is_icio, dimension == "cs_cs")){
  #   iciox <- get_icio_xdata(wio)
  #   for (i in 1:length(iciox$codes)){
  #     country_name <- names(iciox$codes[i])
  #     country_codes <- iciox$codes[[i]]
  #     pr_all <- grep(country_codes, row_names)
  #     pc_all <- grep(country_codes, col_names)
  #     # We must check that sectors in rows are
  #     # also present in columns (if one sector has
  #     # MX1, MX2 in rows and only MEX in columns,
  #     # nothing should be done
  #     # If they are both not null (length(NULL) is 0) and
  #     # Have the same size
  #     if (all(length(pr_all) > 0,
  #             length(pc_all) > 0, length(pr_all) == length(pc_all))){
  #       arr[pr_all, pc_all] <- diag(colSums(df[pr_all, pc_all]))
  #     }
  #   }
  # }

  rownames(arr) <- rownames(df)
  if (dimension == "cs_c"){
    colnames(arr) <- paste0(rep(dfinfo$c2_names, each = s1), "_",
                            dfinfo$s1_names)
  } else if (dimension == "cs_cs"){
    colnames(arr) <- colnames(df)
  } else if (dimension == "cs") {
    colnames(arr) <- paste0("WLD_", dfinfo$s1_names)
  }

  # Output
  return(arr)

}


# ****************************
# Internal auxiliary functions
# ****************************


#' Block diagonal matrix with groups
#'
#' @description
#' Produces a block diagonal matrix when columns or rows
#'   are grouped for some countries. For example, rows contain `CAN`, `USA`
#'   and `MEX` columns contain NAFTA. This is an internal function for several
#'   `make_exvadec()` methods.
#' @param df A matrix with named rows and columns.
#' @param list_with_group String vector in which some of the elements
#'   is a group
#' @param group_in_cols Boolean. `TRUE` if the group country is in the
#'   columns, `FALSE` if it is in the rows.
#' @keywords internal
#' @noRd
#' @return A block diagonal version of the original matrix.
bkdx <- function(df, list_with_group, group_in_cols = TRUE) {

  # Get wio_type from parent environment (always a function)
  wio_type <- get("wio_type", envir = parent.frame())

  # Create an empty matrix of the same size
  row_names <- rownames(df)
  col_names <- colnames(df)
  arr <- matrix(0, nrow(df), ncol(df))

  # Name rows and columns
  dimnames(arr) <- list(row_names, col_names)

  for(cou in list_with_group){
    if (group_in_cols) {
      # For row we use disaggregation
      cou_code <- get_geo_codes(cou, wio_type, icio_extend = TRUE)
      pr <- grep(cou_code, row_names)
      # For column, just the name
      pc <- grep(cou, col_names)
    } else {
      # For row we use the name
      pr <- grep(cou, row_names)
      # For column, we get the code with disaggregation, just in case
      cou_code <- get_geo_codes(cou, wio_type, icio_extend = TRUE)
      pc <- grep(cou_code, col_names)
    }
    # We make zero the
    arr[pr, pc] <- df[pr, pc]
  }

  return(arr)

}


#' Block off-diagonal matrix with groups
#'
#' @description
#' Produces a block off-diagonal matrix when columns or rows
#'   are grouped for some countries. For example, rows contain `CAN`, `USA`
#'   and `MEX` columns contain NAFTA. This is an internal function for several
#'   `make_exvadec()`.
#' @param df A matrix with named rows and columns.
#' @param list_with_group String vector in which some of the elements
#'   is a group
#' @param group_in_cols Boolean. `TRUE` if the group country is in the
#'   columns, `FALSE` if it is in the rows.
#' @keywords internal
#' @noRd
#' @return A block off-diagonal version of the original matrix.
bkoffdx <- function(df, list_with_group, group_in_cols = TRUE) {

  # Get wio_type from parent environment (always a function)
  wio_type <- get("wio_type", envir = parent.frame())

  row_names <- rownames(df)
  col_names <- colnames(df)
  for(cou in list_with_group){
    if (group_in_cols) {
      # For row we use disaggregation
      cou_code <- get_geo_codes(cou, wio_type, icio_extend = TRUE)
      pr <- grep(cou_code, row_names)
      # For column, just the name
      pc <- grep(cou, col_names)
    } else {
      # For row we use the name
      pr <- grep(cou, row_names)
      # For column, we get the code
      cou_code <- get_geo_codes(cou, wio_type, icio_extend = TRUE)
      pc <- grep(cou_code, col_names)
    }
    # We make zero the
    df[pr, pc] <- 0
  }
  return(df)
}


