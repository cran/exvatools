#' Extract ICIO 2021 table
#'
#' @param src_dir Character string for source folder of zip file
#' @param year Integer
#' @keywords internal
#' @noRd
#' @return List with basic input-output matrices and metadata
extract_icio2021 <- function(src_dir, year = NULL, quiet = FALSE) {

  # Dimensions
  G <- 67
  GX <- 71
  N <- 45
  GN <- G * N
  GXN <- GX * N
  FD <- 6
  GFD <- G * FD

  # Use last year if year not specified
  if (is.null(year)) {
    year <- 2018
    cli::cli_alert_info(c("Year not specified. Using year {year}"))
  }

  # Name of zip file
  if (year %in% c(1995:1999)) {
    zip_file <- "ICIO_1995-1999.zip"
  } else if (year %in% c(2000:2004)) {
    zip_file <- "ICIO_2000-2004.zip"
  } else if (year %in% c(2005:2009)) {
    zip_file <- "ICIO_2005-2009.zip"
  } else if (year %in% c(2010:2014)) {
    zip_file <- "ICIO_2010-2014.zip"
  } else if (year %in% c(2015:2018)) {
    zip_file <- "ICIO_2015-2018.zip"
  } else {
    stop(paste0("Year ", year, " is not available"))
  }

  # Check that file exists
  check_wio_source_file(src_dir, zip_file)

  # Name of csv to extract from zip file
  csv_file <- paste0("ICIO2021_", year, ".csv")

  # Create temporary directory and use data.table
  td <- tempdir()

  if (!quiet) {cli::cli_alert_info(c("Unzipping {.file {csv_file}}..."))}
  utils::unzip(paste0(src_dir, zip_file), file = csv_file, exdir = td)
  filepath <- paste0(td, "\\", csv_file)
  # For OS compatibility
  filepath <- gsub("\\\\", "/", filepath)

  # Extract data
  df <- data.table::fread(filepath, stringsAsFactors = FALSE)


  # Alternative: Create temporary file and use read.csv (much slower)
  # tmp <- tempfile()
  # tmp <- unz(paste0(src_dir, zip_file), csv_file)
  # print(paste0("Unzipping ", csv_file, "..."))
  #
  # # Extract data
  # df <- utils::read.csv(file = tmp, stringsAsFactors = FALSE)

  # Get row names
  rowx_names <- as.character(df[[1]])

  # Remove first column of names
  df <- df[,-1]

  # Get column names
  colx_names <- colnames(df)

  # Delete temporary directory
  unlink(td)
  rm(td)

  # Names
  gxn_names <- rowx_names[1:GXN]
  gn_names <- rowx_names[1:GN]
  gx_names <- substr(gxn_names[seq(1, GXN, N)], 1, 3)
  g_names <- substr(gn_names[seq(1, GN, N)], 1, 3)
  n_names <- paste0("D", substr(rowx_names[1:N], 5, nchar(rowx_names[1:N])))
  gfd_names <- colx_names[(GXN + 1):(GXN + GFD)]
  fd_names <- unique(substr(gfd_names, 5, nchar(gfd_names)))
  rownames(df) <- rowx_names

  # Basic matrices: Z, Y, X, VA
 if (!quiet) { cli::cli_alert_info("Getting matrices Z, Y, X")}

  Z <- as.matrix(df[1:GXN, 1:GXN])
  rownames(Z) <- gxn_names

  # Y with FD components
  Yfd <- as.matrix(df[1:GXN, (GXN+1):(GXN+GFD)])
  rownames(Yfd) <- gxn_names

  # Aggregation of Yfd
  Y <- matrix(0, GXN, G)
  for(r in 1:G) {
    p <- (r - 1)*FD + 1
    q <- (r - 1)*FD + FD
    Y[,r] <- rowSums(Yfd[,p:q])
  }
  rownames(Y) <- gxn_names
  colnames(Y) <- g_names

  # X and VA
  X <- as.numeric(rowSums(Z) + rowSums(Y))
  VA <- as.numeric(X - colSums(Z))
  names(X) <- names(VA) <- gxn_names

  # TAXSUB <- as.matrix(df[(GXN + 1):(GXN + G), 1:GXN])
  # rownames(TAXSUB) <- rowx_names[(GXN + 1):(GXN + G)]
  # VA <- as.numeric(df[(GXN + G + 1), 1:GXN])
  # X <- as.numeric(df[(GXN + G + 1 + 1), 1:GXN])

  # Create io object
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

  io$type <- "icio2021"

  io$year <- year

  return(io)

}
