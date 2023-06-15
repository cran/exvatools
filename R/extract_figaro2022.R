#' Extract FIGARO 2022 table
#'
#' Extract basic matrices from source data and prepares it for
#' processing with `make_wio`
#' @param src_dir Character string for source folder
#' @param year Integer
#' @param version Character string with version (`"industry"` or `"product"`)
#' @param quiet Boolean, if `TRUE`, suppress all status messages. Default
#'   is `FALSE`, i.e., messages are shown.
#' @keywords internal
#' @noRd
#' @return List with basic input-output matrices and metadata
extract_figaro2022 <- function(src_dir, year = NULL,
                                version = "industry", quiet = FALSE) {

  # Dimensions
  G <- 46
  GX <- 46
  N <- 64
  GN <- G * N
  GXN <- GX * N
  FD <- 5
  GFD <- G * FD

  # Use last year if year not specified
  if (is.null(year)) {
    year <- 2020
    cli::cli_alert_info(c("Year not specified. Using year {year}"))
  }

  # Extraction of data
  if (version == "industry") {
    csv_file <- paste0("matrix_eu-ic-io_ind-by-ind_", year, ".csv")
  } else if (version == "product") {
    csv_file <- paste0("matrix_eu-ic-io_prod-by-prod_", year, ".csv")
  }

  # Check that file exists
  check_wio_source_file(src_dir, csv_file)

  # If exists
  filepath <- paste0(src_dir, csv_file)

  # Extract data
  if (!quiet) {cli::cli_alert_info("Extracting data from {csv_file}...")}
  df <- data.table::fread(filepath, stringsAsFactors = FALSE)

  # Remove first column of names
  df <- df[, -1]

  # Names
  # ROW appears as FIGW1 after FI
  g_names <- c("ARG", "AUT", "AUS", "BEL", "BGR", "BRA", "CAN", "CHE",
               "CHN", "CYP", "CZE", "DEU", "DNK", "EST", "ESP", "FIN",
               "ROW", "FRA", "GBR", "GRC", "HRV", "HUN", "IDN", "IRL",
               "IND", "ITA", "JPN", "KOR", "LTU", "LUX", "LVA", "MLT",
               "MEX", "NLD", "NOR", "POL", "PRT", "ROU", "RUS", "SAU",
               "SWE", "SVN", "SVK", "TUR", "USA", "ZAF")

  n_names <- c("D01", "D02", "D03", "D05T09", "D10T12", "D13T15",
               "D16", "D17", "D18", "D19", "D20", "D21", "D22", "D23",
               "D24", "D25", "D26", "D27", "D28", "D29", "D30", "D31T32",
               "D33", "D35", "D36", "D37T39", "D41T43", "D45", "D46",
               "D47", "D49", "D50", "D51", "D52", "D53", "D55T56",
               "D58", "D59T60", "D61", "D62T63", "D64", "D65", "D66",
               "D68", "D69T70", "D71", "D72", "D73", "D74T75", "D77",
               "D78", "D79", "D80T82", "D84", "D85", "D86", "D87T88",
               "D90T92", "D93", "D94", "D95", "D96", "D97T98", "D99")

  # P3_S13, P3_S14, P3_S15, P51G, P5M
  fd_names <- c("GGFC", "HFCE", "NPISH", "GFCF", "INVNT")

  gn_names <- paste0(rep(g_names, each = N), gsub("D", "_", n_names))
  gfd_names <- paste0(rep(g_names, each = FD), "_", fd_names)
  gxn_names <- gn_names
  gx_names <- g_names

  # Basic matrices: Z, Y, X, VA
  if (!quiet) { cli::cli_alert_info("Getting matrices Z, Y, X")}

  Z <- as.matrix(df[1:GXN, 1:GXN])
  rownames(Z) <- colnames(Z) <- gxn_names

  # Y with FD components
  Yfd <- as.matrix(df[1:GXN, (GXN+1):(GXN+GFD)])
  rownames(Yfd) <- gxn_names

  # Aggregation of Yfd
  Y <- matrix(0, GXN, G)
  for(r in 1:G) {
    p <- (r - 1) * FD + 1
    q <- (r - 1) * FD + FD
    Y[, r] <- rowSums(Yfd[,p:q])
  }
  rownames(Y) <- gxn_names
  colnames(Y) <- g_names

  # X and VA
  X <- as.numeric(rowSums(Z) + rowSums(Y))
  VA <- as.numeric(X - colSums(Z))
  names(X) <- names(VA) <- gxn_names

  # Tax less subsidies (D21X31)
  # TAXSUB <- as.matrix(df[(GXN + 1):(GXN + 1), 1:GXN])
  # Direct purchases abroad (OP_RES)
  # OPRES <- as.matrix(df[(GXN + 2):(GXN + 2), 1:GXN])
  # Non-resident purchases in territory (OP_NRES)
  # OPNRES <- as.matrix(df[(GXN + 3):(GXN + 3), 1:GXN])
  # Compensation of employees (D1)
  # COE <- as.matrix(df[(GXN + 4):(GXN + 4), 1:GXN])
  # Other net taxes on production (D29X39)
  # ONTP <- as.matrix(df[(GXN + 5):(GXN + 5), 1:GXN])
  # Gross operating surplus (B2A3G)
  # GOS <- as.matrix(df[(GXN + 6):(GXN + 6), 1:GXN])

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

  # Extraction of data
  if (version == "industry") {
    io$type <- "figaro2022i"
  } else if (version == "product") {
    io$type <- "figaro2022p"
  }

  io$year <- year

  return(io)

}

