#' get_df_info
#' @description Extracts geographical and sector information from a matrix
#' @param df A matrix with named rows and columns
#' @return A list with several parameters: number of countries and sectors
#'  in rows, number of countries and sectors in columns, names of countries
#'  and sectors, presence of extended countries (`MEX` and `CHN`) and their
#'  positions, identification as `icio`.
#' @keywords internal
#' @noRd
get_df_info <- function(df) {

  row_names <- rownames(df)
  col_names <- colnames(df)

  if (any(is.null(row_names), is.null(col_names))) {
    stop(paste0("Can't get data from rows or columns without names\n",
                "Please name the rows and columns of ",
                deparse(substitute(df))))
  }

  # Countries: remove "_" followed by any characters
  # before I put 2 en c y 3 en s
  c1_names <- unique(gsub("[_][^_]{1,}", "", row_names))
  c2_names <- unique(gsub("[_][^_]{1,}", "", col_names))
  # sectors: remove any characters not "_" followed by "_"
  s1_names <- unique(gsub("[^_]{1,}[_]", "", row_names))
  s2_names <- unique(gsub("[^_]{1,}[_]", "", col_names))

  c1 <- length(c1_names)
  c2 <- length(c2_names)

  # If names of countries and sectors rows have the same length
  if (length(c1_names) == length(s1_names)) {
    # Are they the same vector?
    if (all(c1_names == s1_names)){
      # If yes, set length of sector = 1 (so c*s = c)
      # And set names as empty
      s1 <- 1
      s1_names <- ""
      s1_dim <- ""
    } else {
      # There is only 1 sector
      s1 <- length(s1_names)
      s1_dim <- "s"
    }
  } else {
    s1 <- length(s1_names)
    s1_dim <- "s"
  }

  # If names of countries and sectors cols have the same length
  if (length(c2_names) == length(s2_names)) {
    # Are they the same vector?
    if (all(c2_names == s2_names)){
      # If yes, set length of sector = 1 (so c*s = c)
      # And set names as empty
      s2 <- 1
      s2_names <- ""
      s2_dim <- ""
    } else {
      s2 <- length(s2_names)
      # Is sector names are different, s2_names are fd_names
      if (all(s2_names == s1_names)) {
        s2_dim <- "s"
      } else {
        s2_dim <- "fd"
      }

    }
  } else {
    s2 <- length(s2_names)
    if (length(s2_names) == length(s1_names)) {
      # Same vector?
      if (all(s2_names == s1_names)) {
        s2_dim <- "s"
        # Is sector names are different, s2_names are fd_names
      } else {
        s2_dim <- "fd"
      }
      # Is sector names are different, s2_names are fd_names
    } else {
      s2_dim <- "fd"
    }
  }

  if (c2 == 1) {
    # For example, X
    dimensions <- paste0("c", s1_dim)
  } else {
    dimensions <- paste0("c", s1_dim, "_", "c", s2_dim)
  }


  # if (all(s1 > 1, s2 > 1, s1 == s2)) {
  #   dimensions <- "cs_cs"
  # } else if (all(s1 > 1, s2 > 1, s1 > s2)) {
  #   dimensions <- "cs_cfd"
  # } else if (all(s1 > 1, s2 == 1)) {
  #   dimensions <- "cs_c"
  # } else {
  #   stop("Can't identify the dimensions")
  # }

  rows_have_MX <- ifelse(any(grepl("MX[1-3]", row_names)), TRUE, FALSE)
  rows_have_CN <- ifelse(any(grepl("CN[1-4]", row_names)), TRUE, FALSE)
  cols_have_MX <- ifelse(any(grepl("MX[1-3]", col_names)), TRUE, FALSE)
  cols_have_CN <- ifelse(any(grepl("CN[1-4]", col_names)), TRUE, FALSE)
  is_icio <- ifelse (any(rows_have_MX, rows_have_CN,
                         cols_have_MX, cols_have_CN), TRUE, FALSE)

  dfinfo <- list()
  dfinfo$num_rows <- nrow(df)
  dfinfo$num_cols <- ncol(df)
  dfinfo$c1 <- c1
  dfinfo$s1 <- s1
  dfinfo$c2 <- c2
  dfinfo$s2 <- s2
  dfinfo$c1_names <- c1_names
  dfinfo$s1_names <- s1_names
  dfinfo$c2_names <- c2_names
  dfinfo$s2_names <- s2_names

  # Rows
  if (rows_have_MX) {
    dfinfo$rows_have_MX <- TRUE
    dfinfo$prMEX <- grep("MEX", row_names)
    dfinfo$prMXall <- grep("MX[1-3]", row_names)
  } else {
    dfinfo$rows_have_MX <- FALSE
    if ("MEX" %in% c1_names) {
      dfinfo$prMEX <- grep("MEX", row_names)
    }
  }
  if (rows_have_CN) {
    dfinfo$rows_have_CN <- TRUE
    dfinfo$prCHN <- grep("CHN", row_names)
    dfinfo$prCNall <- grep("CN[1-4]", row_names)
  } else {
    dfinfo$rows_have_CN <- FALSE
    if ("CHN" %in% c1_names) {
      dfinfo$prCHN <- grep("CHN", row_names)
    }
  }
  # Columns
  if (cols_have_MX) {
    dfinfo$cols_have_MX <- TRUE
    dfinfo$pcMEX <- grep("MEX", col_names)
    dfinfo$pcMXall <- grep("MX[1-3]", col_names)
  } else {
    dfinfo$cols_have_MX <- FALSE
    if ("MEX" %in% c2_names) {
      dfinfo$pcMEX <- grep("MEX", col_names)
    }
  }
  if (cols_have_CN) {
    dfinfo$cols_have_CN <- TRUE
    dfinfo$pcCHN <- grep("CHN", col_names)
    dfinfo$pcCNall <- grep("CN[1-4]", col_names)
  } else {
    dfinfo$cols_have_CN <- FALSE
    if ("CHN" %in% c2_names) {
      dfinfo$pcCHN <- grep("CHN", col_names)
    }
  }


  dfinfo$is_icio <- is_icio

  dfinfo$dimensions <- dimensions

  return (dfinfo)
}


#' @title Get data of extended countries from icio
#'
#' @description Get a list of useful data from extended countries
#'   (China and Mexico) in an icio-type wio.
#' @param wio_object A wio object.
#' @keywords internal
#' @noRd
#' @return A list with position and codes of extended countries
#' @examples
#' iciox <- get_icio_data(wio)
get_icio_xdata <- function(wio_object){

  wio <- wio_object
  wio_type <- wio$type
  is_icio <- is.icio(wio_type)

  if(is_icio){

    iciox <- list()
    iciox$pg <- list()
    iciox$pgn <- list()
    iciox$pgd <- list()
    iciox$codes <- list()

    gxn_names <- wio$names$gxn_names
    gn_names <- wio$names$gn_names
    gx_names <- wio$names$gx_names
    g_names <- wio$names$g_names
    n_names <- wio$names$n_names
    fd_names <- wio$names$fd_names
    gfd_names <- wio$names$gfd_names

    iciox$pg$MEX <- grep("MEX", g_names)
    iciox$pg$CHN <- grep("CHN", g_names)
    iciox$pgn$MEX <- grep("MEX", gxn_names)
    iciox$pgn$CHN <- grep("CHN", gxn_names)
    iciox$pgd$MEX <- grep("MEX", gfd_names)
    iciox$pgd$CHN <- grep("CHN", gfd_names)

    if (is.iciolong(wio$type)) {
      iciox$codes$MEX <- "MX1|MX2|MX3"
      iciox$codes$CHN <- "CN1|CN2|CN3|CN4"
      iciox$pg$MXall <- c(grep("MX1|MX2|MX3", gx_names))
      iciox$pg$CNall <- c(grep("CN1|CN2|CN3|CN4", gx_names))
      iciox$pgn$MXall <- c(grep("MX1|MX2|MX3", gxn_names))
      iciox$pgn$CNall <- c(grep("CN1|CN2|CN3|CN4", gxn_names))
    } else if (is.icioshort(wio$type)) {
      iciox$codes$MEX <- "MX1|MX2"
      iciox$codes$CHN <- "CN1|CN2"
      iciox$pg$MXall <- c(grep("MX1|MX2", gx_names))
      iciox$pg$CNall <- c(grep("CN1|CN2", gx_names))
      iciox$pgn$MXall <- c(grep("MX1|MX2", gxn_names))
      iciox$pgn$CNall <- c(grep("CN1|CN2", gxn_names))
    }

  } else{

    stop("iciox can only be created with an icio wio")

  }

  return(iciox)

}


#' name
#' @description Names rows and columns of matrix and returns the matrix.
#'  Equivalent to df <- dimnames(df, list(row_names, col_names))
#' @param df A matrix
#' @param row_names Character vector with names of rows
#' @param col_names Character vector with names of columns
#' @keywords internal
#' @noRd
#' @return The matrix with named rows and columns
name <- function(df, row_names, col_names) {

  dimnames(df) <- list(row_names, col_names)
  return(df)

}















