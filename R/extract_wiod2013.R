#' Extract WIOD 2013 table
#'
#' Extract basic matrices from source data and prepares it for
#' processing with `make_wio`
#' @param src_dir Character string for source folder of zip file
#' @param year Integer
#' @param quiet Boolean, if `TRUE`, suppress all status messages. Default
#'   is `FALSE`, i.e., messages are shown.
#' @keywords internal
#' @noRd
#' @return List with basic input-output matrices and metadata
extract_wiod2013 <- function(src_dir, year = NULL, quiet = FALSE) {

  # Dimensions
  G <- 41
  GX <- 41
  N <- 35
  GN <- G * N
  GXN <- GX * N
  FD <- 5
  GFD <- G * FD

  # Extraction of data

  # Use last year if not specified
  if (is.null(year)) {
    year <- 2011
    cli::cli_alert_info(c("Year not specified. Using year {year}"))
  }

  # Name of zip file
  zip_file <- "WIOTS_in_EXCEL.zip"

  # Check that file exists
  check_wio_source_file(src_dir, zip_file)

  # Name of xlsx file to extract from zip file
  if (year %in% c(1995:2007)) {
    xlsx_file <- paste0("WIOT", sprintf('%02d', year %% 100),
                        "_ROW_Apr12.xlsx")
  } else if (year %in% c(2008:2011)) {
    xlsx_file <- paste0("WIOT", sprintf('%02d', year %% 100),
                        "_ROW_Sep12.xlsx")
  }

  # Create temporary directory
  td <- tempdir()
  if (!quiet) {cli::cli_alert_info(c("Unzipping {.file {xlsx_file}}..."))}

  utils::unzip(paste0(src_dir, zip_file), file = xlsx_file, exdir = td)
  filepath <- paste0(td, "\\", xlsx_file)
  # For OS compatibility
  filepath <- gsub("\\\\", "/", filepath)

  # Extract data
  if (!quiet) {cli::cli_alert_info("Extracting data from xlsx file,
                                   please wait...")}
  # Since R 4.2.1 readxl is too slow. Now we use openxlsx

  df <- openxlsx::read.xlsx(filepath,
                            rows = c(7:1449),
                            cols = c(5:1645),
                            colNames = FALSE)
  if (!quiet) {cli::cli_alert_success("Extraction completed")}

  # Remove temporary directory
  unlink(td)
  rm(td)

  # Names
  g_names <- c("AUS", "AUT", "BEL", "BGR", "BRA", "CAN", "CHN", "CYP",
               "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GBR",
               "GRC", "HUN", "IDN", "IND", "IRL", "ITA", "JPN", "KOR",
               "LTU", "LUX", "LVA", "MEX", "MLT", "NLD", "POL", "PRT",
               "ROU", "RUS", "SVK", "SVN", "SWE", "TUR", "TWN", "USA",
               "ROW")
  n_names <- c("C01T05", "C10T14", "C15T16", "C17T18", "C19",
               "C20", "C21T22", "C23", "C24", "C25", "C26",
               "C27T28", "C29", "C30T33", "C34T35", "C36T37",
               "C40T41", "C45", "C50", "C51", "C52", "C55",
               "C60", "C61", "C62", "C63", "C64", "C65T67",
               "C70", "C71T74", "C75", "C80", "C85",
               "C90T93", "C95T97")
  fd_names <- c("HFCE", "NPISH", "GGFC", "GFCF", "INVNT")
  gn_names <- paste0(rep(g_names, each = N), gsub("C", "_", n_names))
  gfd_names <- paste0(rep(g_names, each = FD), "_", fd_names)
  gx_names <- g_names
  gxn_names <- gn_names

  # Original
  # fd_names <- c("HFCE", "NPISH", "GGFC", "GFCF", "INVNT")
  # n_names <- paste0("C", sprintf("%02d", c(1:35)))
  # row_xtra <- c("TIC", "TLS", "CIF", "DPA", "PDT", "VA",
  # "ITM", "OUTPUT")
  # rownames(df) <- c(gn_names, row_xtra)
  # colnames(df) <- c(gn_names, gf_names, "OUTPUT")

  # For standarization


  # Basic matrices: Z, Y, X, VA
  if (!quiet) {cli::cli_alert_info("Getting matrices Z, Y, X")}
  Z <- as.matrix(df[1:GN, 1:GN])
  Yfd <- as.matrix(df[1:GN, (GN+1):(GN+GFD)])
  # Aggregation of Y
  Y <- matrix(0, GN, G)
  for(r in 1:G) {
    p <- (r - 1)*FD + 1
    q <- (r - 1)*FD + FD
    Y[,r] <- rowSums(Yfd[,p:q])
  }
  rownames(Z) <- rownames(Yfd) <- rownames(Y) <- colnames(Z) <- gn_names
  colnames(Y) <- g_names
  colnames(Yfd) <- gfd_names

  # X and VA
  X <- as.numeric(rowSums(Z) + rowSums(Y))
  VA <- as.numeric(X - colSums(Z))
  rownames(Y) <- gn_names
  colnames(Y) <- g_names

  # Not used
  # X <- as.numeric(df[1:GN, GN+GFD+1])
  # X <- as.numeric(wiot[(GN+8), (INI+1):(INI+GN)])
  # VA <- as.numeric(wiot[(GN+6),(INI+1):(INI+GN)])

  # Create io object
  io <- list(Z, Yfd, Y, VA, X)
  names(io) <- c("Z", "Yfd", "Y", "VA", "X")

  # Metadata (dimnesions)
  io$dims <- list(G, N, FD, GX, GN, GXN, GFD)
  names(io$dims) <- c("G","N","FD", "GX","GN","GXN", "GFD")

  # Metadata (names)
  io$names <- list(g_names, n_names, fd_names, gx_names,
                    gn_names, gxn_names, gfd_names)
  names(io$names) <- c("g_names","n_names","fd_names",
                       "gx_names", "gn_names", "gxn_names",
                       "gfd_names")

  io$type <- "wiod2013"

  io$year <- year

  return(io)

}
