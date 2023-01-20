#' Make standard world input-output matrices from custom data
#'
#' Creates a list object of class `wio` containing the typical
#'   international input-output matrices in a standardized format, as well as a
#'   list of code names (countries, sectors and demand components) and a list
#'   of dimensions (number of countries, sectors and demand components), using
#'   custom data.
#' @param df A data frame or matrix containing data for intermediate inputs
#'   and final demand.
#' @param g_names A string vector with names of countries.
#' @param n_names A string vector with names of sectors. If missing, sectors
#'   will be `S01`, `S02`, etc. If just one sector, it will be named
#'   `TOTAL`.
#' @param fd_names A string vector with names of final demand components.
#'   If missing, demand components will be `FD1`, `FD2`, etc. If just one,
#'   it will be named `FD`.
#' @param year Integer. If missing, the current year will be used.
#' @param quiet Boolean, if `TRUE`, the function will produce a silent output.
#' @details
#' `make_custom_wio()` creates a `wio` from custom input-output data  provided
#'   as a single matrix of dimension `GxN x GxFD`, i.e., the matrix `Z` of
#'   intermediate inputs (dimension `GxN x GxN`) bound with the matrix `Yfd` of
#'   final demand (dimension `GxN x GxFD`). The matrices of total output `X` and
#'   value added `VA` will be automatically generated, so should not be
#'   included. Data must be exclusively numeric.
#'
#' A string vector with the names of countries is required. Number of countries
#'   will be calculated from this vector. Names for sectors and final demand
#'   components can be provided or will otherwise be automatically
#'   generated. All names must be composed of alphabetic characters (no special
#'   characters are allowed).
#'
#' @return A `wio`object of `wiotype = "custom"`.
#' @export
#'
#' @examples
#' df <- as.data.frame(matrix(c(19:36), nrow = 3))
#' wio <- make_custom_wio(df, g_names = c("C01", "C02", "C03"))
make_custom_wio <- function(df, g_names,
                            n_names = NULL, fd_names = NULL,
                            year = NULL,
                            quiet = FALSE) {

  # Dimensions
  G <- length(g_names)
  N <- nrow(df) / G
  FD <- (ncol(df) - (G * N)) / G
  GX <- G
  GN <- G * N
  GXN <- GX * N
  GFD <- G * FD

  # Check correct dimensions
  if (!nrow(df) == GN) {
    stop("Number of row dimensions should be GxN")
  } else if (!ncol(df) == GN + GFD) {
    stop("Number of row dimensions should be GxN + GxFD")
  }

  # Corrections
  g_names <- gsub("_", ".", g_names)

  if (is.null(n_names)) {
    if (N > 1) {
      n_names <- paste0("S", sprintf("%02d", 1:N))
    } else {
      n_names <- "TOTAL"
    }
  } else {
    n_names <- gsub("_", ".", n_names)
  }

  if (is.null(fd_names)) {
    if (FD > 1) {
      fd_names <- paste0("FD", sprintf("%02d", 1:FD))
    } else {
      fd_names <- "FD"
    }
  } else {
    fd_names <- gsub("_", ".", fd_names)
  }

  if (is.null(year)) {
    year <- as.numeric(substr(Sys.Date(), 1, 4))
  } else {
    if (!nchar(year) == 4) {
      stop("Incorrect year format")
    }
  }

  gx_names <- g_names
  gn_names <- paste0(rep(g_names, each = N), "_", n_names)
  gfd_names <- paste0(rep(g_names, each = FD), "_", fd_names)
  gxn_names <- gn_names

  Z <- as.matrix(df[1:GN, 1:GN])
  Yfd <- as.matrix(df[1:GN, (GN + 1):(GN + GFD)])

  # Group Yfd
  if (FD > 1) {
    Y <- matrix(0, GN, G)
    for(r in 1:G) {
      p <- (r - 1) * FD + 1
      q <- (r - 1) * FD + FD
      Y[, r] <- rowSums(Yfd[, p:q])
    }
  } else {
    Y <- Yfd
  }

  X <- as.numeric(rowSums(Z) + rowSums(Y))
  # X <- as.numeric(df[1:GN, (GN + GFD):(GN + GFD + 1)])

  VA <- as.numeric(X - colSums(Z))

  rownames(Z) <- rownames(Yfd) <- rownames(Y) <- gn_names
  colnames(Z) <- gn_names
  colnames(Yfd) <- gfd_names
  colnames(Y) <- g_names
  names(VA) <- names(X) <- gn_names

  # Matrices A, B, V----
  if (!quiet) {cli::cli_alert_info("Calculating matrices A, B, V")}
  A <- sweep(Z, 2, X, "/")
  A[!is.finite(A)] <- 0
  A <- as.matrix(A)
  B <- solve(diag(GXN) - A)
  B[!is.finite(B)] <- 0
  # Solve uses rows of 1s in rows of 0s to guarantee
  # non-zero determinant
  B <- as.matrix(B)
  V <- VA/X
  V[!is.finite(V)] <- 0
  V <- as.numeric(V)

  # Submatrices of Z, A, B and Y----

  if (!quiet) {cli::cli_alert_info("Calculating submatrices of Z, A, B and Y")}
  Zd <- Ad <- Bd <- matrix(0, GN, GN)
  Yd <- matrix(0, GN, G)
  Zm <- Z
  Am <- A
  Bm <- B
  Ym <- Y

  for(s in 1:G) {
    m <- N*(s - 1) + 1 #Row from
    n <- N*(s - 1) + N #Row to
    pgn_s <- c(m:n)
    pg_s <- s

    Zd[pgn_s, pgn_s] <- Z[pgn_s, pgn_s]
    Ad[pgn_s, pgn_s] <- A[pgn_s, pgn_s]
    Bd[pgn_s, pgn_s] <- B[pgn_s, pgn_s]
    Zm[pgn_s, pgn_s] <- Am[pgn_s, pgn_s] <- Bm[pgn_s, pgn_s] <- 0

    Yd[pgn_s, pg_s] <- Y[pgn_s, pg_s]
    Ym[pgn_s, pg_s] <- 0
  }

  # Local Leontieff matrix Ld----

  if (!quiet) {cli::cli_alert_info("Calculating local Leontieff matrix Ld")}
  Ld <- solve(diag(GN) - Ad)

  # Exports matrices EXGR and E----

  if (!quiet) {cli::cli_alert_info("Calculating export matrices EXGR and E")}

  # We need to group columns of Z
  if (N > 1) {
    Zm_meld <- matrix(0, GN, G)
    ncou <- G
    for(i in 1:ncou) {
      m <- N*(i-1) + 1
      n <- N*(i-1) + N
      for(j in 1:ncou) {
        p <- N*(j-1) + 1
        q <- N*(j-1) + N
        Zm_meld[m:n, j] <- rowSums(Zm[m:n,p:q])
      }
    }
  } else {
    Zm_meld <- Zm
  }

  # EXGR
  EXGR <- Zm_meld + Ym

  # E
  E <- diag(rowSums(EXGR))

  # V_hat
  W <- diag(V)

  # Put V, VA, X as vector matrix (for names)
  V <- as.matrix(V)
  VA <- as.matrix(VA)
  X <- as.matrix(X)


  if (!quiet) {cli::cli_alert_info("Preparing output...")}

  # Create wio object----
  #Create object wio and its names
  # Complete version
  wio <- list(Z, Zd, Zm, A, Ad, Am, B, Bd, Bm, Ld,
              Yfd, Y, Yd, Ym, VA, V, W, X, EXGR, E)
  wio_names <- c("Z","Zd","Zm","A","Ad","Am","B","Bd","Bm","Ld",
                 "Yfd", "Y","Yd","Ym","VA","V","W","X","EXGR","E")

  names(wio) <- wio_names

  #Name rows and columns----
  #
  # Rows (all the same)
  wio <- lapply(wio, "rownames<-", gn_names)

  # Columns (depending on dimension)
  for(var in wio_names) {
    if (var %in% c("Z","Zd","Zm","A","Ad","Am",
                   "B","Bd","Bm","Ld","W","E")) {
      colnames(wio[[var]]) <- gn_names
    } else if (var %in% c("Yfd")) {
      colnames(wio[[var]]) <- gfd_names
    } else if (var %in% c("Y","Yd","Ym","EXGR")) {
      colnames(wio[[var]]) <- g_names
    } else if (var %in% c("VA", "V", "X")) {
      colnames(wio[[var]]) <- var
    }
  }

  # Metadata (dims and names)----
  wio$dims <- list(G, N, FD, GX, GN, GXN, GFD)
  names(wio$dims) <- c("G","N","FD", "GX","GN","GXN", "GFD")

  # Names
  wio$names <- list(g_names, n_names, fd_names, gx_names,
                    gn_names, gxn_names, gfd_names)
  names(wio$names) <- c("g_names","n_names","fd_names",
                        "gx_names", "gn_names", "gxn_names",
                        "gfd_names")

  # Type
  wio$type <- "custom"
  wio$year <- year

  # Class----
  class(wio) <- "wio"

  if (!quiet) {cli::cli_alert_success("Done!")}

  return(wio)

}
