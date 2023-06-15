#' Extract Long-run WIOD 2012 table
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
extract_lrwiod2022 <- function(src_dir, year = NULL, quiet = FALSE) {

  # Uses reshape2 and data.table

  # Dimensions
  G <- 26
  GX <- 26
  N <- 23
  FD <- 4
  GN <- G * N
  GXN <- GX * N
  GFD <- G * FD

  # Extraction of data
  csv_file <- "lr_wiod_wiot_final_filled.csv"

  # Check that file exists
  check_wio_source_file(src_dir, csv_file)

  # If exists
  filepath <- paste0(src_dir, csv_file)

  # Start extraction
  if (!quiet) {cli::cli_alert_info("Extracting data from {csv_file}...")}
  # data.table::fread is much much faster that utils::read.csv
  df_total <- data.table::fread(filepath, stringsAsFactors = FALSE)

  if (is.null(year)) {
    year <- 2000
    cli::cli_alert_info(c("Year not specified. Using year {year}"))
  }

  df <- df_total[df_total$year == year, ]

  cli::cli_alert_info("Reshaping...")
  # Rows are ok, but columns are disordered
  # We suppress a message not relevant
  suppressMessages(
    df <- reshape2::dcast(df, row_country + row_id + row_isic3 ~
                            col_country + col_isic3, value.var = "value")
    )

  # Get full row and column names
  row_names <- paste0(df$row_country, "_", df$row_isic3)
  col_names <- colnames(df)

  # Remove 2 first columns
  df <- df[, -c(1:3)]
  rownames(df) <- row_names

  # And fix the col_names
  col_names <- col_names[4:length(col_names)]

  # Names
  g_names <- c("AUS", "AUT", "BEL", "BRA", "CAN", "CHN", "DEU", "DNK",
               "ESP", "FIN", "FRA", "GBR", "GRC", "HKG", "IND", "IRL",
               "ITA", "JPN", "KOR", "MEX", "NLD", "PRT", "SWE", "TWN",
               "USA", "xROW")
  n_names <- c("AtB", "C", "D15t16", "D17t19", "D21t22", "D23", "D24",
               "D25", "D26", "D27t28", "D29", "D30t33", "D34t35", "Dnec",
               "E", "F", "G", "H", "I60t63", "I64", "J", "K", "LtQ")
  xtra_row_names <- c("xTOT_xII", "xTOT_xVA", "xTOT_xTXSP", "xTOT_xSD",
                      "xTOT_xPURNR",  "xTOT_xPURR",
                      "xTOT_xMRG_int", "xTOT_xGO")
  va_name <- c("xTOT_xVA")
  # FD names: added to each country, not at the end
  fd_names <- c("xCONS_h", "xCONS_g", "xGFCF", "xINV")
  xtra_col_names <- c("xTOT_xX", "xTOT_xM", "xTOT_xMRG_int", "xTOT_xGO")
  exgr_name <- c("xTOT_xX")
  imgr_name <- c("xTOT_xM")
  x_name <- c("xTOT_xGO")

  # This should be the normal order (with FD after all countries)
  # col_order <- c(paste0(rep(g_names, each = N), "_", n_names),
  #                paste0(rep(g_names, each = FD), "_", fd_names),
  #                xtra_col_names)

  # This is the real order (with FD after each country)
  # It can be seen in the Excel version source files
  col_order <- c(paste0(rep(g_names, each = N+FD), "_",
                        c(n_names, fd_names)),
                 xtra_col_names)
  # This reordering is not really necessary, but is just to
  # recover the exact original input-output table in wide form
  # (for the fun of it)
  df <- df[, col_order]

  # Basic matrices: Z, Y, X, VA and names
  # There is a problem: FD elements are put in columns just after
  # intermediate products of each country. We need to rebuild the
  # matrices using grep to extract intermediates-only and
  # final-demand-only
  if (!quiet) {cli::cli_alert_info("Getting matrices Z, Y, X")}
  Z <- as.matrix(df[c(1:GN),
                    grep(paste0("_", n_names, collapse = "|"),
                         col_names)])
  Yfd <- as.matrix(df[c(1:GN),
                      grep(paste0("_", fd_names, collapse = "|"),
                           col_names)])

  # We aggregate Yfd
  Y <- matrix(0, GN, G)
  for(r in 1:G) {
    p <- (r - 1)*FD + 1
    q <- (r - 1)*FD + FD
    Y[, r] <- rowSums(Yfd[,p:q])
  }

  X <- as.numeric(rowSums(Z) + rowSums(Y))

  # X <- as.numeric(df[c(1:GN), c(x_name)])
  # EXGR <- as.numeric(dat3[c(1:GN), c(exgr_name)])
  # IMGR <- as.numeric(dat3[c(1:GN), c(imgr_name)])
  # VA value is really similar to sum(Z) + sym(Yfd), but we make sure it
  # coincides. So, instead of
  # VA <- as.numeric(dat3[c(va_name),
  #                  grep(paste0("_", n_names, collapse = "|"),
  #                            col_names)])
  # We use
  VA <- X - colSums(Z)

  # Now we correct names to standardize wio
  # First we replace xROW by ROW
  g_names <- gsub("xROW", "ROW", g_names)

  # We rename sectors as C01, C02...
  # Alternative: n_names <- paste0("C", sprintf("%02d", c(1:N)))
  # But we use standard
  n_names <- c("C01T05", "C10T14", "C15T16", "C17T19",
               "C21T22", "C23", "C24", "C25", "C26", "C27T28",
               "C29", "C30T33", "C34T35", "C36T37", "C40T41",
               "C45", "C50T52", "C55", "C60T63", "C64", "C65T67",
               "C70T74", "C75T99")
  gn_names <- paste0(rep(g_names, each = N), gsub("C", "_", n_names))
  fd_names <- c("HFCE", "GGFC", "GFCF", "INVNT")
  gfd_names <- paste0(rep(g_names, each = FD), "_", fd_names)
  gxn_names <- gn_names
  gx_names <- g_names

  # Apply names
  rownames(Z) <- colnames(Z) <- gn_names
  rownames(Yfd) <- gn_names
  colnames(Yfd) <- gfd_names
  rownames(Y) <- gn_names
  colnames(Y) <- g_names
  names(VA) <- gn_names
  names(X) <- gn_names

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

  io$type <- "lrwiod2022"

  io$year <- year

  return(io)

}
