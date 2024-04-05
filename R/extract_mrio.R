#'
#' Extract basic matrices from source data and prepares it for
#' processing with `make_wio`
#' @param edition Character string with edition
#' @param src_dir Character string for source folder
#' @param year Integer
#' @param quiet Boolean, if `TRUE`, suppress all status messages. Default
#'   is `FALSE`, i.e., messages are shown.
#' @keywords internal
#' @noRd
#' @return List with basic input-output matrices and metadata
extract_mrio <- function(edition = "mrio2023", src_dir,
                           year = NULL, quiet = FALSE) {

  if (edition %in% c("mrio2023", "mrio2023k")) {

    # Dimensions
    G <- 63
    GX <- 63
    N <- 35
    GN <- G * N
    GXN <- GX * N
    FD <- 5
    GFD <- G * FD

    # Use last year if year not specified
    if (is.null(year)) {
      year <- 2022
      cli::cli_alert_info(c("Year not specified. Using year {year}"))
    }

    # xlsx_names
    if (edition == "mrio2023") {
      if (year == 2000) {
        xlsx_file <- "ADB-MRIO-2000_Mar2022-3.xlsx"
      } else if (year == 2007) {
        xlsx_file <- "ADB-MRIO-2007.xlsx"
      } else if (year %in% c(2008:2016)) {
        xlsx_file <- paste0("ADB-MRIO-", year, "_Mar2022", ".xlsx")
      } else if (year %in% c(2017:2019)) {
        xlsx_file <- paste0("ADB-MRIO62-", year, "_Dec2022", ".xlsx")
      } else if (year %in% c(2020:2022)) {
        xlsx_file <- paste0("ADB-MRIO62-", year, "_June2023", ".xlsx")
      } else {
        stop(paste0("Year ", year, " is not available"))
      }
    } else if (edition== "mrio2023k") {
      if (year == 2000) {
         xlsx_file <- "ADB-MRIO-2000-at-constant-2010-prices.xlsx"
      } else if (year %in% c(2007:2009, 2011:2022))
         xlsx_file <- paste0("ADB MRIO ", year,
                             ", at constant 2010 prices", ".xlsx")
      else {
        stop(paste0("Year ", year, " is not available"))
      }
    }

    # Names
    g_names <- c("AUS", "AUT", "BEL", "BGR", "BRA", "CAN", "CHE", "CHN", "CYP",
                 "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GBR", "GRC",
                 "HRV", "HUN", "IDN", "IND", "IRL", "ITA", "JPN", "KOR", "LTU",
                 "LUX", "LVA", "MEX", "MLT", "NLD", "NOR", "POL", "PRT", "ROU",
                 "RUS", "SVK", "SVN", "SWE", "TUR", "TWN", "USA", "BGD", "MYS",
                 "PHL", "THA", "VNM", "KAZ", "MNG", "LKA", "PAK", "FJI", "LAO",
                 "BRN", "BTN", "KGZ", "KHM", "MDV", "NPL", "SGP", "HKG", "ROW")

    n_names <- c("C01T05", "C10T14", "C15T16", "C17T18", "C19",
                 "C20", "C21T22", "C23", "C24", "C25", "C26",
                 "C27T28", "C29", "C30T33", "C34T35", "C36T37",
                 "C40T41", "C45", "C50", "C51", "C52", "C55",
                 "C60", "C61", "C62", "C63", "C64", "C65T67",
                 "C70", "C71T74", "C75", "C80", "C85",
                 "C90T93", "C95T97")

    # n_names <- c("D01T03", "D05T09", "D10T12", "D13T14", "D15", "D16",
    #              "D17T18P58", "D19", "D20T21", "D22", "D23", "D24T25",
    #              "D26T27", "D28", "D29T30", "D31T33", "D35T39", "D41T43",
    #              "D45", "D46", "D47", "D55T56", "D49", "D50", "D51", "D52",
    #              "D53P61", "D64T66", "D68", "D58T60P62T63P69T82",
    #              "D84", "D85", "D86T88", "D90T96", "D97T98")

    fd_names <- c("GGFC", "HFCE", "NPISH", "GFCF", "INVNT")

    gn_names <- paste0(rep(g_names, each = N), gsub("^C", "_", n_names))
    gfd_names <- paste0(rep(g_names, each = FD), "_", fd_names)
    gxn_names <- gn_names
    gx_names <- g_names

    # Check that file exists
    check_wio_source_file(src_dir, xlsx_file)

    # If exists
    filepath <- paste0(src_dir, xlsx_file)

    if (!quiet) {cli::cli_alert_info(c("Importing {.file {xlsx_file}}..."))}

    df <- openxlsx::read.xlsx(filepath,
                              rows = c(8:2220),
                              cols = c(5:2525),
                              colNames = FALSE,
                              na.strings = " ")

  } else if (edition %in% c("mrio2023x")) {

    # Dimensions
    G <- 73
    GX <- 73
    N <- 35
    GN <- G * N
    GXN <- GX * N
    FD <- 5
    GFD <- G * FD

    # Use last year if year not specified
    if (is.null(year)) {
      year <- 2022
      cli::cli_alert_info(c("Year not specified. Using year {year}"))
    }

    # xlsx_names
    if (year == 2017) {
      xlsx_file <- "ADB-MRIO-2017_Dec2022-2.xlsx"
    } else if (year %in% c(2018:2019)) {
      xlsx_file <- paste0("ADB-MRIO-", year, "_Dec2022", ".xlsx")
    } else if (year %in% c(2020:2022)) {
      xlsx_file <- paste0("ADB-MRIO-", year, "_June2023", ".xlsx")
    } else {
      stop(paste0("Year ", year, " is not available"))
    }

    # Names
    g_names <- c("AUS", "AUT", "BEL", "BGR", "BRA", "CAN", "CHE", "CHN", "CYP",
                 "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GBR", "GRC",
                 "HRV", "HUN", "IDN", "IND", "IRL", "ITA", "JPN", "KOR", "LTU",
                 "LUX", "LVA", "MEX", "MLT", "NLD", "NOR", "POL", "PRT", "ROU",
                 "RUS", "SVK", "SVN", "SWE", "TUR", "TWN", "USA", "BGD", "MYS",
                 "PHL", "THA", "VNM", "KAZ", "MNG", "LKA", "PAK", "FJI", "LAO",
                 "BRN", "BTN", "KGZ", "KHM", "MDV", "NPL", "SGP", "HKG", "ARG",
                 "COL", "ECU", "ARM", "GEO", "EGY", "KWT", "SAU", "ARE", "NZL",
                 "ROW")


    n_names <- c("C01T05", "C10T14", "C15T16", "C17T18", "C19",
                 "C20", "C21T22", "C23", "C24", "C25", "C26",
                 "C27T28", "C29", "C30T33", "C34T35", "C36T37",
                 "C40T41", "C45", "C50", "C51", "C52", "C55",
                 "C60", "C61", "C62", "C63", "C64", "C65T67",
                 "C70", "C71T74", "C75", "C80", "C85",
                 "C90T93", "C95T97")

    fd_names <- c("GGFC", "HFCE", "NPISH", "GFCF", "INVNT")

    gn_names <- paste0(rep(g_names, each = N), gsub("^C", "_", n_names))
    gfd_names <- paste0(rep(g_names, each = FD), "_", fd_names)
    gxn_names <- gn_names
    gx_names <- g_names

    # Check that file exists
    check_wio_source_file(src_dir, xlsx_file)

    # If exists
    filepath <- paste0(src_dir, xlsx_file)

    if (!quiet) {cli::cli_alert_info(c("Importing {.file {xlsx_file}}..."))}

    df <- openxlsx::read.xlsx(filepath,
                              rows = c(8:2562),
                              cols = c(5:2924),
                              colNames = FALSE,
                              na.strings = " ")
  }

  # Convert NA in 0 (at least in 2016 there is NA)
  df[is.na(df)] <- 0

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

  # Type and year
  io$type <- edition
  io$year <- year

  return(io)

}
