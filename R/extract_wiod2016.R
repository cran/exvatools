#' Extract WIOD 2016 table
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
extract_wiod2016 <- function(src_dir, year = NULL, quiet = FALSE) {

  # Dimensions
  G <- 44
  GX <- 44
  N <- 56
  GN <- G * N
  GXN <- GX * N
  FD <- 5
  GFD <- G * FD
  INI <- 5
  ROWX <- 5

  # Extraction of data

  # Use last year if not specified
  if (is.null(year)) {
    year <- 2014
    cli::cli_alert_info(c("Year not specified. Using year {year}"))
  }

  # Name of zip file
  zip_file <- "WIOTS_in_R.zip"

  # Check that file exists
  check_wio_source_file(src_dir, zip_file)

  # Name of rdata_file to extract from zip file
  rdata_file <- paste0("WIOT", year, "_October16_ROW.RData")

  # Create temporary file
  tmp <- tempfile()
  tmp <- unz(paste0(src_dir, zip_file), rdata_file)
  if (!quiet) {cli::cli_alert_info(c("Unzipping {.file {rdata_file}}..."))}

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
  df <- load_rdata_file(tmp)

  # Remove temporary rda file
  unlink(tmp)
  rm(tmp)

  # Standard
  g_names <- df$Country[seq(1, GN, N)]
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
  gn_names <- paste0(rep(g_names, each = N), gsub("D", "_", n_names))
  gx_names <- g_names
  gxn_names <- gn_names
  gfd_names <- paste0(rep(g_names, each = FD), "_", fd_names)

  # Original (not used)
  # g_names <- df$Country[seq(1, GN, N)]
  # n_names <- df$IndustryCode[1:N]
  # n_text <- df$IndustryDescription[1:N]
  # fd_names <- c("CONS_h",	"CONS_np",	"CONS_g",	"GFCF",	"INVEN")
  # gn_names <- paste0(rep(g_names, each = N), "_", n_names)
  # gxn_names <- gn_names
  # gfd_names <- paste0(rep(g_names, each = FD), "_", fd_names)


  # Basic matrices: Z, Y, X, VA
  if (!quiet) {cli::cli_alert_info("Getting matrices Z, Y, X")}
  Z <- as.matrix(df[1:GN, (INI + 1):(INI + GN)])
  Yfd <- as.matrix(df[1:GN, (INI + GN + 1):(INI + GN + GFD)])
  Y <- matrix(0, GN, G)
  for(r in 1:G) {
    p <- (r - 1)*FD + 1
    q <- (r - 1)*FD + FD
    Y[,r] <- rowSums(Yfd[,p:q])
  }
  rownames(Z) <- rownames(Yfd) <- rownames(Y) <- colnames(Z) <- gn_names
  colnames(Y) <- g_names
  colnames(Yfd) <- gfd_names

  X <- as.numeric(rowSums(Z) + rowSums(Y))
  VA <- as.numeric(X - colSums(Z))

  # Original (not used)
  # X <- as.numeric(df[(GN + 8), (INI + 1):(INI + GN)])
  # II_fob <- as.numeric(df[(GN + 1), (INI + 1):(INI + GN)])
  # TXSP <- as.numeric(df[(GN + 2), (INI + 1):(INI + GN)])
  # EXP_adj <- as.numeric(df[(GN + 3), (INI + 1):(INI + GN)])
  # PURR <- as.numeric(df[(GN + 4), (INI + 1):(INI + GN)])
  # PURNR <- as.numeric(df[(GN + 5), (INI + 1):(INI + GN)])
  # VA <- as.numeric(df[(GN + 6), (INI + 1):(INI + GN)])
  # IntTTM <- as.numeric(df[(GN + 7), (INI + 1):(INI + GN)])
  # X <- as.numeric(df[(GN + 8), (INI + 1):(INI + GN)])
  # names(II_fob) <- names(TXSP) <- names(EXP_adj) <-
  #   names(PURR) <- names(PURNR) <- names(VA) <-
  #   names(IntTTM) <- names(X) <- gn_names

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

  io$type <- "wiod2016"

  io$year <- year

  return(io)

}
