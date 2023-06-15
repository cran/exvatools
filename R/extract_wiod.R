#' Extract WIOD table
#'
#' Extract basic matrices from source data and prepares it for
#' processing with `make_wio`
#' @param edition Character string with WIOD edition (default = last edition)
#' @param src_dir Character string for source folder of zip file
#' @param year Integer
#' @param quiet Boolean, if `TRUE`, suppress all status messages. Default
#'   is `FALSE`, i.e., messages are shown.
#' @keywords internal
#' @noRd
#' @return List with basic input-output matrices and metadata
extract_wiod <- function(edition = "wiod2016", src_dir,
                         year = NULL, quiet = FALSE) {

  # Default year
  if (edition == "wiod2016") {
    last_year <- 2014
  } else if (edition == "wiod2013") {
    last_year <- 2011
  }

  # Use last year if year not specified
  if (is.null(year)) {
    year <- last_year
    cli::cli_alert_info(c("Year not specified. Using year {year}"))
  }

  # Dimensions and file names
  # ************
  # wiod2016
  # ************
  if (edition == "wiod2016") {
    # Dimensions
    G <- 44
    GX <- 44
    N <- 56
    FD <- 5
    INI <- 5
    ROWX <- 5
    # Name of zip file
    zip_file <- "WIOTS_in_R.zip"
    # Name of rdata_file to extract from zip file
    file_name <- paste0("WIOT", year, "_October16_ROW.RData")
    g_names <- c("AUS", "AUT", "BEL", "BGR", "BRA", "CAN", "CHE",
                 "CHN", "CYP", "CZE", "DEU", "DNK", "ESP", "EST",
                 "FIN", "FRA", "GBR", "GRC", "HRV", "HUN", "IDN",
                 "IND", "IRL", "ITA", "JPN", "KOR", "LTU", "LUX",
                 "LVA", "MEX", "MLT", "NLD", "NOR", "POL", "PRT",
                 "ROU", "RUS", "SVK", "SVN", "SWE", "TUR", "TWN",
                 "USA", "ROW")
    n_names <- c("D01", "D02", "D03", "D05T09", "D10T12", "D13T15",
                 "D16", "D17", "D18", "D19", "D20", "D21", "D22",
                 "D23", "D24", "D25", "D26", "D27", "D28", "D29",
                 "D30", "D31T32", "D33", "D35", "D36", "D37T39",
                 "D41T43", "D45", "D46", "D47", "D49", "D50",
                 "D51", "D52", "D53", "D55T56", "D58", "D59T60",
                 "D61", "D62T63", "D64", "D65", "D66", "D68",
                 "D69T70", "D71", "D72", "D73", "D74T75", "D77T82",
                 "D84", "D85", "D86T88", "D90T96", "D97T98", "D99")
    fd_names <- c("HFCE", "NPISH", "GGFC", "GFCF", "INVNT")
  # ************
  # wiod2013
  # ************
  } else if (edition == "wiod2013") {
    # Dimensions
    G <- 41
    GX <- 41
    N <- 35
    FD <- 5
    # Name of zip file
    zip_file <- "WIOTS_in_EXCEL.zip"
    # Name of xlsx file to extract from zip file
    if (year %in% c(1995:2007)) {
      file_name <- paste0("WIOT", sprintf('%02d', year %% 100),
                          "_ROW_Apr12.xlsx")
    } else if (year %in% c(2008:2011)) {
      file_name <- paste0("WIOT", sprintf('%02d', year %% 100),
                          "_ROW_Sep12.xlsx")
    }
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
  }

  # Derived dimensions and names
  GN <- G * N
  GXN <- GX * N
  GFD <- G * FD
  gx_names <- g_names
  gn_names <- paste0(rep(g_names, each = N), gsub("[C|D]", "_", n_names))
  gxn_names <- paste0(rep(gx_names, each = N), gsub("[C|D]", "_", n_names))
  gfd_names <- paste0(rep(g_names, each = FD), "_", fd_names)

  # Check that file exists
  check_wio_source_file(src_dir, zip_file)

  # Get temporary directory
  td <- tempdir()

  # Unzip file in temporary folder
  if (!quiet) {cli::cli_alert_info(c("Unzipping {.file {file_name}}..."))}
  utils::unzip(paste0(src_dir, zip_file), file = file_name, exdir = td)
  filepath <- paste0(td, "\\", file_name)
  # For OS compatibility
  filepath <- gsub("\\\\", "/", filepath)

  # Get df
  # ************
  # wiod2016
  # ************
  if (edition == "wiod2016") {
    # The load function creates an object called wiot,
    # in the environment (load() does not allow another name)
    # To avoid a 'wiot' temporarily being in the environment,
    # we will use a small function
    load_rdata_file <- function(rdata_path) {
      load(rdata_path)
      df <- get("wiot")
      return(df)
    }
    # Now we import it as:
    df <- load_rdata_file(filepath)
    df <- as.matrix(df[, -c(1:5)])
  # ************
  # wiod2013
  # ************
  } else if (edition == "wiod2013") {
    if (!quiet) {cli::cli_alert_info("Extracting data from xlsx file,
                                   please wait...")}
    # Since R 4.2.1 readxl is too slow. Now we use openxlsx
    df <- openxlsx::read.xlsx(filepath,
                              rows = c(7:1449),
                              cols = c(5:1645),
                              colNames = FALSE)
    if (!quiet) {cli::cli_alert_success("Extraction completed")}
  }

  # Delete temporary file
  unlink(filepath)

  # Basic matrices: Z, Y, X, VA
  if (!quiet) { cli::cli_alert_info("Getting matrices Z, Y, X")}

  # Z
  Z <- as.matrix(df[1:GXN, 1:GXN])

  # Y with FD components
  Yfd <- as.matrix(df[1:GXN, (GXN + 1):(GXN + GFD)])

  # Group Yfd
  Y <- matrix(0, GN, G)
  for(r in 1:G) {
    p <- (r - 1) * FD + 1
    q <- (r - 1) * FD + FD
    Y[,r] <- rowSums(Yfd[, p:q])
  }

  # X and VA
  X <- as.numeric(rowSums(Z) + rowSums(Y))
  VA <- as.numeric(X - colSums(Z))

  # Name all matrices
  rownames(Z) <- colnames(Z) <- rownames(Y) <- rownames(Yfd) <- gxn_names
  colnames(Y) <- g_names
  colnames(Yfd) <- gfd_names
  names(VA) <- names(X) <- gxn_names

  # Create io object
  io <- list(Z, Yfd, Y, VA, X)
  names(io) <- c("Z", "Yfd", "Y", "VA", "X")

  # Metadata (dimensions)
  io$dims <- list(G, N, FD, GX, GN, GXN, GFD)
  names(io$dims) <- c("G","N","FD", "GX","GN","GXN", "GFD")

  # Metadata (names)
  io$names <- list(g_names, n_names, fd_names, gx_names,
                   gn_names, gxn_names, gfd_names)
  names(io$names) <- c("g_names","n_names","fd_names",
                       "gx_names", "gn_names", "gxn_names",
                       "gfd_names")

  io$type <- edition

  io$year <- year

  return(io)

}
