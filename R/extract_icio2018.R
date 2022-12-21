#' Extract ICIO 2018 table
#'
#' @param src_dir Character string for source folder of zip file
#' @param year Integer
#' @param quiet Boolean, if `TRUE`, suppress all status messages. Default
#'   is `FALSE`, i.e., messages are shown.
#' @keywords internal
#' @noRd
#' @return List with basic input-output matrices and metadata
extract_icio2018 <- function(src_dir, year = NULL, quiet = FALSE) {

  # Dimensions
  G <- 65
  GX <- 69
  N <- 36
  GN <- G * N
  GXN <- GX * N
  FD <- 6
  GFD <- G*FD

  # Extraction of data

  # Use last year if not specified
  if (is.null(year)) {
    year <- 2015
    cli::cli_alert_info(c("Year not specified. Using year {year}"))
  }

  # Name of zip file
  zip_file <- paste0("ICIO2018_", year, ".zip")

  # Check that file exists
  check_wio_source_file(src_dir, zip_file)

  # Name of csv to extract from zip file (important: CSV in uppercase)
  csv_file <- paste0("ICIO2018_", year, ".CSV")

  # Create temporary file
  tmp <- tempfile()
  tmp <- unz(paste0(src_dir, zip_file), csv_file)

  if (!quiet) {cli::cli_alert_info(c("Unzipping {.file {zip_file}}..."))}

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
  gxn_names <- rowx_names[1:GXN]
  gn_names <- rowx_names[1:GN]
  gx_names <- substr(gxn_names[seq(1, GXN, N)], 1, 3)
  g_names <- substr(gn_names[seq(1, GN, N)], 1, 3)
  n_names <- paste0("D", substr(rowx_names[1:N], 5,
                                nchar(rowx_names[1:N])))
  gfd_names <- colx_names[(GXN+1):(GXN+GFD)]
  fd_names <- unique(substr(gfd_names, 5, nchar(gfd_names)))

  # Basic matrices: Z, Y, X, VA
  if (!quiet) {cli::cli_alert_info("Getting matrices Z, Y, X")}

  Z <- as.matrix(df[1:GXN,1:GXN])
  Yfd <- as.matrix(df[1:GXN, (GXN+1):(GXN+GFD)])
  rownames(Yfd) <- gxn_names

  # Aggregation of Y
  Y <- matrix(0, GXN, G)
  for(r in 1:G) {
    p <- (r - 1) * FD + 1
    q <- (r - 1) * FD + FD
    Y[,r] <- rowSums(Yfd[,p:q])
  }
  rownames(Y) <- gxn_names
  colnames(Y) <- g_names

  X <- as.numeric(rowSums(Z) + rowSums(Y))
  VA <- as.numeric(X - colSums(Z))
  names(X) <- names(VA) <- gxn_names

  # TAXSUB <- as.matrix(df[(GXN+1):(GXN+G),1:GXN])
  # VA <- as.numeric(df[(GXN+G+1),1:GXN])
  # X <- as.numeric(df[(GXN+G+1+1),1:GXN])

  # Create object
  io <- list(Z, Yfd, Y, VA, X)
  names(io) <- c("Z", "Yfd", "Y", "VA", "X")

  # Metadata (dims and names)
  io$dims <- list(G, N, FD, GX, GN, GXN, GFD)
  names(io$dims) <- c("G","N","FD", "GX","GN","GXN", "GFD")

  io$names <- list(g_names, n_names, fd_names, gx_names,
                    gn_names, gxn_names, gfd_names)
  names(io$names) <- c("g_names","n_names","fd_names",
                       "gx_names", "gn_names", "gxn_names",
                       "gfd_names")

  io$type <- "icio2018"

  io$year <- year

  return(io)

}
