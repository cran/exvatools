#' Make standard world input-output matrices from source files
#'
#' Creates a list object of class `wio` containing the typical
#'   international input-output matrices in a standardized format, as well as a
#'   list of code names (countries, sectors and demand components) and a list
#'   of dimensions (number of countries, sectors and demand components). It
#'   can use source files from well-known databases or internal data (test
#'   data).
#' @param wiotype String specifying the name and edition of the
#'   input-output tables to be used:
#'     * `"icio2021"` for the 2021 edition of the OECD ICIO tables
#'       (1995-2018). Deprecated editions `"icio2018"` (1995-2011)
#'       and `"icio2016"`(2005-2015) remain available for literature
#'       replication purposes.
#'     * `"wiod2016"` for the 2016 edition of the WIOD tables
#'       (2000-2014). The deprecated edition `"wiod2013"` (1995-2011)
#'       remains available for literature replication purposes.
#'     * `"lrwiod2022"` for the 2022 edition of the long-run WIOD
#'       (1965-2000), useful for historical analysis.
#'     * `"iciotest"` for an example of an ICIO-type international
#'       input-output table (disaggregated for `MEX` into `MX1` and `MX2` and
#'       for `CHN` into `CN1` and `CN2`) and `"wiodtest"` for an example of a
#'       WIOD-type international input-output table (not disaggregated). Data
#'       for these tables is not real, but these small input-output tables
#'       are useful for didactic purposes and to check the functionality of
#'       the program.
#' @param src_dir String specifying the source directory where the source file
#'   of the international input-output tables is saved, normally as a zip file
#'   (containing `.csv` files, `.RData` or `.xlsx` files, see Details). In
#'   order for `make_wio()` to work, these zip files should not be renamed.
#'   If `src_dir` is not specified, `make_wio()` will look in the
#'   working directory.
#' @param year Integer specifying reference year. If `NULL` (default),
#'   the last available year of the specified database will be used.
#' @param quiet Boolean, if `TRUE` suppress all status messages. Default
#'   is `FALSE`, i.e., messages are shown.
#' @details
#' `make_wio()` directly unzips and processes the original source files
#'   for the different international input-output tables and returns a list
#'   with the traditional matrices, including the coefficient matrix `A`, the
#'   Leontief global inverse matrix `B`, the Leontief matrix of local inverse
#'   matrices `Ld` and others.
#'
#' Original source files can be obtained in the OECD's
#' [ICIO web page](https://www.oecd.org/sti/ind/inter-country-input-output-tables.htm)
#' or in the University of Groningen's
#' [WIOD web page](https://www.rug.nl/ggdc/valuechain/wiod/).
#'
#' If source files are used, they must be previously downloaded and
#'   placed in an accessible folder in disk, without renaming them. The
#'   following name pattern is expected:
#'   * `ICIO_XXXX-XXXX.zip` for `"icio2021"` (`.csv` files)
#'   * `ICIO2018_XXXX.zip` for `"icio2018"` (`.csv` files)
#'   * `ICIO2016_XXXX.zip` for `"icio2018"` (`.csv` files)
#'   * `WIOTS_in_R.zip` for `"wiod2016"` (`.RData` files)
#'   * `WIOTS_in_EXCEL.zip` for `"wiod2013"` (`.xlsx` files). Requires
#'     package `openxlsx`..
#'   * `lr_wiod_wiot_final_filled.csv` for `"lrwiod2022"`. Requires
#'     packages `data.table` and `reshape2`.
#' The input-output framework follows the traditional demand model of
#'   Leontief (1936), which makes assumptions about the stability of inputs
#'   (and therefore value-added) as a proportion of production. This allows
#'   production and value-added to be expressed as the result of variations
#'   in final demand.\
#' Details about the content of the world input-output object (`wio`)
#'   produced by `make_wio()` can be obtained with the command
#'   `summary(wio_object)`.
#' @seealso [make_custom_wio()]
#'
#' @return A list object of class `wio` including input-output
#'   matrices, dimensions, and names.
#' @export
#' @examples
#' wio <- make_wio("iciotest")
#' summary(wio)
#' \dontrun{
#' # The following examples require the previous download of the source
#' # files in the working directory or in a directory specified by `src_dir`.
#' wio <- make_wio("icio2021", 2018)
#' wio <- make_wio("wiod2016", 2014)
#' wio <- make_wio("wiod2021", 2018, src_dir = "C:/Users/John/R/")
#' }
make_wio <- function(wiotype ="icio2021", year = NULL,
                     src_dir = NULL, quiet = FALSE) {

  # Check arguments----
  list_args <- c(as.list(environment()))
  checked_args <- check_wio_args(list_args)

  # Corrected arguments
  wiotype <- checked_args$wiotype
  year <- checked_args$year
  src_dir <- checked_args$src_dir

  # Get working directory
  wd <- getwd()
  if (is.null(src_dir)) {
    src_dir <- paste0(wd, "/")
  }

  is_icio <- is.icio(wiotype)

  # Extraction of Z, Yfd, Y, X, VA----

  if (wiotype == "icio2021") {
    io <- extract_icio2021(src_dir, year, quiet)
  } else if (wiotype == "icio2018") {
    io <- extract_icio2018(src_dir, year, quiet)
  } else if (wiotype == "icio2016") {
    io <- extract_icio2016(src_dir, year, quiet)
  } else if (wiotype == "iciotest") {
    io <- generate_test_iot(is_icio = TRUE, quiet)
  } else if (wiotype == "wiod2016") {
    io <- extract_wiod2016(src_dir, year, quiet)
  } else if (wiotype == "wiod2013") {
    io <- extract_wiod2013(src_dir, year, quiet)
  } else if (wiotype == "lrwiod2022") {
    io <- extract_lrwiod2022(src_dir, year, quiet)
  } else if (wiotype == "wiodtest") {
    io <- generate_test_iot(is_icio = FALSE, quiet)
  }

  # Get dimensions----
  G <- io$dims$G
  N <- io$dims$N
  FD <- io$dims$FD
  GX <- io$dims$GX
  GN <- io$dims$GN
  GXN <- io$dims$GXN
  GFD <- io$dims$GFD

  # Get io names----
  g_names <- io$names$g_names
  gx_names <- io$names$gx_names
  n_names <- io$names$n_names
  fd_names <- io$names$fd_names
  gn_names <- io$names$gn_names
  gxn_names <- io$names$gxn_names
  gfd_names <- io$names$gfd_names

  # Get year
  year <- io$year

  Z <- io$Z
  Yfd <- io$Yfd
  Y <- io$Y
  VA <- io$VA
  X <- io$X

  # Position of MEX and CHN (icio only)

  if (is_icio) {

    # With sector
    pgn_MEX <- grep("MEX", gxn_names)
    pgn_CHN <- grep("CHN", gxn_names)
    # pgn_MXall <- c(grep("MX[1-3]{1}", gxn_names))
    # pgn_CNall <- c(grep("CN[1-4]{1}", gxn_names))
    if (is.iciolong(wiotype)) {
      pgn_MXall <- c(grep("MX1|MX2|MX3", gxn_names))
      pgn_CNall <- c(grep("CN1|CN2|CN3|CN4", gxn_names))
    } else if (is.icioshort(wiotype)) {
      pgn_MXall <- c(grep("MX1|MX2", gxn_names))
      pgn_CNall <- c(grep("CN1|CN2", gxn_names))
    }

    # Countries only
    pg_MEX <- grep("MEX", g_names)
    pg_CHN <- grep("CHN", g_names)
    # pg_MXall <- c(grep("MX[1-3]{1}", gx_names))
    # pg_CNall <- c(grep("CN[1-4]{1}", gx_names))
    if (is.iciolong(wiotype)) {
      pg_MXall <- c(grep("MX1|MX2|MX3", gx_names))
      pg_CNall <- c(grep("CN1|CN2|CN3|CN4", gx_names))
    } else if (is.icioshort(wiotype)){
      pg_MXall <- c(grep("MX1|MX2", gx_names))
      pg_CNall <- c(grep("CN1|CN2", gx_names))
    }

  }

  # Matrices A, B, V----
  if (!quiet) {cli::cli_alert_info("Calculating matrices A, B, V")}

  A <- sweep(Z, 2, X, "/")
  A[!is.finite(A)] <- 0
  A <- as.matrix(A)
  B <- solve(diag(GXN) - A)
  B[!is.finite(B)] <- 0
  # R solve uses rows of 1s in rows of 0s to guarantee
  # non-zero determinant
  if (is_icio) {
    B[c(pgn_MEX, pgn_CHN), ] <- 0  # Originally 1xN
  }
  B <- as.matrix(B)
  V <- VA/X
  V[!is.finite(V)] <- 0
  V <- as.numeric(V)

  # Submatrices of Z, A, B and Y----

  if (!quiet) {cli::cli_alert_info("Calculating submatrices of Z, A, B and Y")}

  Zd <- Ad <- Bd <- if (is_icio) matrix(0, GXN, GXN) else matrix(0, GN, GN)
  Yd <- if (is_icio) matrix(0, GXN, G) else matrix(0, GN, G)
  Zm <- Z
  Am <- A
  Bm <- B
  Ym <- Y

  for(s in 1:G) {
    # Determine pgn_s and pg_s
    if (is_icio) {
      if (s == pg_MEX) {
        pgn_s <- pgn_MXall
        pg_s <- pg_MEX
      } else if (s == pg_CHN) {
        pgn_s <- pgn_CNall
        pg_s <- pg_CHN
      } else {
        m <- N*(s - 1) + 1 #Row from
        n <- N*(s - 1) + N #Row to
        pgn_s <- c(m:n)
        pg_s <- s
      }
    } else{
      m <- N*(s - 1) + 1 #Row from
      n <- N*(s - 1) + N #Row to
      pgn_s <- c(m:n)
      pg_s <- s
    }

    Zd[pgn_s, pgn_s] <- Z[pgn_s, pgn_s]
    Ad[pgn_s, pgn_s] <- A[pgn_s, pgn_s]
    Bd[pgn_s, pgn_s] <- B[pgn_s, pgn_s]
    Zm[pgn_s, pgn_s] <- Am[pgn_s, pgn_s] <- Bm[pgn_s, pgn_s] <- 0

    Yd[pgn_s, pg_s] <- Y[pgn_s, pg_s]
    Ym[pgn_s, pg_s] <- 0

  }

  # Local Leontief matrix Ld----

  if (!quiet) {cli::cli_alert_info("Calculating local Leontief matrix Ld")}
  Ld <- if (is_icio) solve(diag(GXN) - Ad) else solve(diag(GN) - Ad)
  # Clean empty rows
  if (is_icio) {
    Ld[c(pgn_MEX, pgn_CHN), ] <- 0
  }

  # Exports matrices EXGR and E----

  if (!quiet) {cli::cli_alert_info("Calculating export matrices EXGR and E")}

  # We need to meld columns of Z
  # First we group column sectors
  Zm_meld <- if (is_icio) matrix(0, GXN, GX) else matrix(0, GN, G)
  ncou <- ifelse (is_icio, GX, G)
  for(i in 1:ncou) {
    m <- N * (i - 1) + 1
    n <- N * (i - 1) + N
    for(j in 1:ncou) {
      p <- N * (j - 1) + 1
      q <- N * (j - 1) + N
      Zm_meld[m:n, j] <- rowSums(Zm[m:n,p:q])
    }
  }
  if (is_icio) {
    # Meld
    Zm_meld[, pg_MEX] <- rowSums(Zm_meld[, pg_MXall])
    Zm_meld[, pg_CHN] <- rowSums(Zm_meld[, pg_CNall])
    # and remove the columns
    Zm_meld <- Zm_meld[, -c(pg_MXall, pg_CNall)]
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
  wio <- list(Z, Zd, Zm, A, Ad, Am,
              B, Bd, Bm, Ld,
              Yfd, Y, Yd, Ym,
              VA, V, W, X, EXGR, E)
  wio_names <- c("Z", "Zd", "Zm", "A", "Ad", "Am",
                 "B", "Bd", "Bm", "Ld",
                 "Yfd", "Y", "Yd", "Ym",
                 "VA", "V", "W", "X", "EXGR", "E")

  names(wio) <- wio_names

  #Name rows and columns----

  # Rows (all the same)
  wio <- lapply(wio, "rownames<-", if (is_icio) gxn_names else gn_names)

  # Columns (depending on dimension)
  for(var in wio_names) {
    if (var %in% c("Z", "Zd", "Zm", "A", "Ad", "Am",
                   "B", "Bd", "Bm", "Ld", "W", "E")) {
      colnames(wio[[var]]) <- if (is_icio) gxn_names else gn_names
    } else if (var %in% c("Yfd")) {
      colnames(wio[[var]]) <- gfd_names
    } else if (var %in% c("Y", "Yd", "Ym", "EXGR")) {
      colnames(wio[[var]]) <- g_names
    } else if (var %in% c("VA", "V", "X")) {
      colnames(wio[[var]]) <- var
    }
  }

  # Metadata (dims and names)----
  wio$dims <- list(G, N, FD, GX, GN, GXN, GFD)
  names(wio$dims) <- c("G", "N", "FD", "GX", "GN", "GXN", "GFD")

  # Names
  wio$names <- list(g_names, n_names, fd_names, gx_names,
                    gn_names, gxn_names, gfd_names)
  names(wio$names) <- c("g_names","n_names","fd_names",
                        "gx_names", "gn_names", "gxn_names",
                        "gfd_names")

  # Type
  wio$type <- wiotype

  # Year
  wio$year <- year

  # Class----
  class(wio) <- "wio"

  if (!quiet) {cli::cli_alert_success("Done!")}

  return(wio)

}

