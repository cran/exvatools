#' Extract ICIO 2016 table
#'
#' @param src_dir Character string for source folder of zip file
#' @param year Integer
#' @param quiet Boolean, if `TRUE`, suppress all status messages. Default
#'   is `FALSE`, i.e., messages are shown.
#' @keywords internal
#' @noRd
#' @return List with basic input-output matrices and metadata
extract_icio2016 <- function(src_dir, year = NULL, quiet = FALSE) {

  # Dimensions (initial, before treating discrepancy)
  G <- 64
  GX <- 71
  N <- 34
  GN <- G * N
  GXN <- GX * N
  FD <- 6
  GFD <- G*FD

  # Extraction of data

  if (is.null(year)) {
    year <- 2011
    cli::cli_alert_info(c("Year not specified. Using year {year}"))
  }

  # Name of zip file
  zip_file <- paste0("ICIO2016_", year, ".zip")

  # Check that file exists
  check_wio_source_file(src_dir, zip_file)

  # Name of csv to extract from zip file (important: csv in lowercase)
  csv_file <- paste0("ICIO2016_", year, ".csv")

  # Create temporary file
  tmp <- tempfile()
  tmp <- unz(paste0(src_dir, zip_file), csv_file)

  cli::cli_alert_info(c("Unzipping {.file {zip_file}}..."))

  # Extract data
  df <- utils::read.csv(file = tmp, stringsAsFactors = FALSE)

  # Get row names
  rowx_names <- as.character(df[,1])

  # Remove first column
  df <- df[,-1]

  # Get column names
  colx_names <- colnames(df)

  # Delete csv temporary file
  unlink(tmp)
  rm(tmp)

  # Names
  gx_names <- unique(substr(rowx_names[1:GXN], 1, 3))
  g_names <- gx_names[1:G]
  n_names <- c("C01T05", "C10T14", "C15T16", "C17T19", "C20",
               "C21T22", "C23", "C24", "C25", "C26", "C27",
               "C28", "C29", "C30T33X31", "C31", "C34", "C35",
               "C36T37", "C40T41", "C45", "C50T52", "C55",
               "C60T63", "C64", "C65T67", "C70", "C71", "C72",
               "C73T74", "C75", "C80", "C85", "C90T93", "C95")
  gn_names <- paste0(rep(g_names, each = N), gsub("C", "_", n_names))
  gxn_names <- paste0(rep(gx_names, each = N), gsub("C", "_", n_names))
  fd_names <- c("HFCE", "NPISH", "GGCF", "GFCF", "INVNT", "DIRP")
  gfd_names <- paste0(rep(g_names, each = FD), "_", fd_names)

  # Original (not used)
  # gx_names <- unique(substr(rowx_names[1:GXN], 1, 3))
  # g_names <- gx_names[1:G]
  # n_names <- unique(substr(gxn_names, 5, nchar(gxn_names)))
  # gn_names <- gxn_names[1:GN]
  # gfd_names <- colx_names[(GXN + 1):(GXN + GFD)]
  # fd_names <- unique(substr(gfd_names, 1, nchar(gfd_names) - 4))

  # Basic matrices: Z, Y, X, VA
  if (!quiet) {cli::cli_alert_info("Getting matrices Z, Y, X")}

  Z <- as.matrix(df[1:GXN, 1:GXN])
  VA <- as.numeric(df[(GXN + 1), 1:GXN])
  X <- as.numeric(df[(GXN + 1 + 1), 1:GXN])
  Yfd <- as.matrix(df[1:GXN, (GXN + 1):(GXN + GFD)])
  # Discrepancy
  DISC <- as.matrix(df[1:GXN, (GXN + GFD + 1)])

  X <- rowSums(Z) + rowSums(Yfd) + as.numeric(DISC)
  VA <- X - colSums(Z)

  # Treatment of discrepancy DISC
  if (!quiet) {cli::cli_alert_info("Treating discrepancy...")}

  # First we aggregate FD
  # Unlike later ICIOs, FD is not ordered by country-components
  # (C1_FD1, C1_FD2, C1_FD3...), but by component-countries
  # (FD1_C1, FD1_C2, FD1_C3..)
  # so the aggregation by countries must be different
  Y <- matrix(0, GXN, G)
  for(j in 1:G) {
    Y[, j] <- matrix(0, GXN, 1)
    for(k in 1:FD) {
      m <- (k - 1) * G + j
      Y[, j] <- Y[, j] + Yfd[, m]
    }
  }

  # Then we insert discrepancy before MX1... and CN1...
  Y <- cbind(Y, DISC)
  Y <- rbind(Y[1:GN, ], matrix(0, N, G + 1), Y[(GN + 1):GXN,])

  # Same with Yfd
  Yfd <- cbind(Yfd, DISC)
  Yfd <- rbind(Yfd[1:GN,], matrix(0, N, GFD + 1), Yfd[(GN + 1):GXN, ])

  #Expanded matrix Z with inserted discrepancy
  Zx <- matrix(0, (GX+1) * N, (GX+1) * N)
  # Square
  Zx[1:GN, 1:GN] <- Z[1:GN, 1:GN]
  # Rows MEX CHN (after DISC)
  Zx[(GN + N + 1):(GXN + N), (1:GN)] <- Z[(GN+1):(GXN), (1:GN)]
  #Cols MEX CHN
  Zx[1:GN, (GN + N + 1):(GXN + N)] <- Z[1:GN, (GN + 1):GXN]
  #Intra MEX,CHN
  Zx[(GN + N + 1):(GXN + N),(GN + N + 1):(GXN + N)] <-
    Z[(GN + 1):GXN, (GN + 1):GXN]
  Z <- Zx

  #Expanded VA, X with inserted discrepancy
  VA <- c(VA[1:GN], rep(0,N), VA[(GN + 1):GXN])
  X <- c(X[1:GN], rep(0,N), X[(GN + 1):GXN])

  # Redimension names
  gx_names <- c(gx_names[1:G], "DISC", gx_names[(G + 1):GX])
  g_names <- c(g_names, "DISC")

  # Redimension dims
  G <- G + 1  # 65
  GX <- GX + 1  # 72
  GN <- G * N
  GXN <- GX * N
  GFD <- GFD + 1 # We cannot distribute DISC among FD components

  # Redimension names
  gn_names <- paste0(rep(g_names, each = N), gsub("C", "_", n_names))
  gxn_names <- paste0(rep(gx_names, each = N), gsub("C", "_", n_names))
  gfd_names <- c(gfd_names, "DISC")

  rownames(Z) <- colnames(Z) <- rownames(Y) <- rownames(Yfd) <- gxn_names
  colnames(Y) <- g_names
  colnames(Yfd) <- gfd_names
  names(VA) <- names(X) <- gxn_names

  # Create object
  io <- list(Z, Yfd, Y, VA, X)
  names(io) <- c("Z", "Yfd", "Y", "VA", "X")

  # Metadata: dims
  io$dims <- list(G, N, FD, GX, GN, GXN, GFD)
  names(io$dims) <- c("G","N","FD", "GX","GN","GXN", "GFD")

  # Metadata: names
  io$names <- list(g_names, n_names, fd_names, gx_names,
                   gn_names, gxn_names, gfd_names)
  names(io$names) <- c("g_names","n_names","fd_names",
                       "gx_names", "gn_names", "gxn_names",
                       "gfd_names")

  io$type <- "icio2016"

  io$year <- year

  return(io)

}
