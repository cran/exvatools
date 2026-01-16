##'
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
extract_mrio <- function(edition = "mrio2024", src_dir,
                                               year = NULL, quiet = FALSE) {

  # ******************
  #  MRIO 62 COUNTRIES ----
  # ******************

  if (edition %in% c("mrio2025", "mrio2024", "mrio2023",
                     "mrio2024k", "mrio2023k")) {

    # Dimensions----
    G <- 63
    GX <- 63
    N <- 35
    GN <- G * N
    GXN <- GX * N
    FD <- 5
    GFD <- G * FD

    # ******************
    #  Current prices----
    # ******************

    ## ************
    ## MRIO 2025 --------
    ## ************

    if (edition == "mrio2025") {
      # Use last year if year not specified
      if (is.null(year)) {
        year <- 2024
        cli::cli_alert_info(c("Year not specified. Using year {year}"))
      }
      if (year == 2000) {
        xlsx_file <- "ADB-MRIO-2000_Mar2022-3.xlsx"
      } else if (year == 2007) {
        xlsx_file <- "ADB-MRIO-2007.xlsx"
      } else if (year %in% c(2008:2016)) {
        xlsx_file <- paste0("ADB-MRIO-", year, "_Mar2022", ".xlsx")
      } else if (year %in% c(2017)) {
        xlsx_file <- paste0("ADB-MRIO62-", year, "-August 2025-62", ".xlsx")
      } else if (year %in% c(2018:2020)) {
        xlsx_file <- paste0("ADB-MRIO62-", year, "_September 2024", ".xlsx")
      }else if (year %in% c(2021)) {
        xlsx_file <- paste0("ADB-MRIO62-", year, "_August 2024", ".xlsx")
      } else if (year %in% c(2022:2023)) {
        xlsx_file <- paste0("ADB-MRIO62-", year, "_July 2025", ".xlsx")
      } else if (year %in% c(2024)) {
        xlsx_file <- paste0("ADB-MRIO-", year, "_August 2025-62", ".xlsx")
      } else {
        stop(paste0("Year ", year, " is not available"))
      }

    ## ************
    ## MRIO 2024 --------
    ## ************
    } else if (edition == "mrio2024") {
      # Use last year if year not specified
      if (is.null(year)) {
        year <- 2023
        cli::cli_alert_info(c("Year not specified. Using year {year}"))
      }
      if (year == 2000) {
        xlsx_file <- "ADB-MRIO-2000_Mar2022-3.xlsx"
      } else if (year == 2007) {
        xlsx_file <- "ADB-MRIO-2007.xlsx"
      } else if (year %in% c(2008:2016)) {
        xlsx_file <- paste0("ADB-MRIO-", year, "_Mar2022", ".xlsx")
      } else if (year %in% c(2017:2020)) {
        xlsx_file <- paste0("ADB-MRIO62-", year, "_September 2024", ".xlsx")
      } else if (year %in% c(2021:2022)) {
        xlsx_file <- paste0("ADB-MRIO62-", year, "_August 2024", ".xlsx")
      } else if (year %in% c(2023)) {
        xlsx_file <- paste0("ADB-MRIO62-", year, "_September 2024", ".xlsx")
      }
      else {
        stop(paste0("Year ", year, " is not available"))
      }

    ## ******************
    ## MRIO 2023 ----
    ## ******************

    } else if (edition == "mrio2023") {
      # Use last year if year not specified
      if (is.null(year)) {
        year <- 2022
        cli::cli_alert_info(c("Year not specified. Using year {year}"))
      }
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

    # *********************
    # Constant prices ----
    # *********************

    ## ***************
    ## MRIO 2024k & 2025k----
    ## ***************

    } else if (edition %in% c("mrio2025k", "mrio2024k")) {
      # Use last year if year not specified
      if (is.null(year)) {
        year <- 2023
        cli::cli_alert_info(c("Year not specified. Using year {year}"))
      }
      # Years
      if (year == 2000) {
        xlsx_file <- "ADB-MRIO-2000-at-constant-2010-prices.xlsx"
      } else if (year %in% c(2007:2009, 2011:2016)) {
        xlsx_file <- paste0("ADB MRIO ", year,
                            ", at constant 2010 prices", ".xlsx")
      } else if (year %in% c(2017:2018)) {
        xlsx_file <- paste0("ADB MRIO ", year,
                            ", at constant 2010 prices_Oct 2024", ".xlsx")
      } else if (year %in% c(2019)) {
        xlsx_file <- paste0("ADB MRIO ", year,
                            ", at constant 2010 prices_Jan 2025", ".xlsx")
      } else if (year %in% c(2020)) {
        xlsx_file <- paste0("ADB MRIO ", year,
                            ", at constant 2010 prices_Oct 2024", ".xlsx")
      } else if (year %in% c(2021:2022)) {
        xlsx_file <- paste0("ADB MRIO ", year,
                            ", at constant 2010 prices_Sept 2024", ".xlsx")
      } else if (year %in% c(2023)) {
        xlsx_file <- paste0("ADB-MRIO ", year,
                            ", at constant 2010 prices_Oct 2024", ".xlsx")
      } else {
        stop(paste0("Year ", year, " is not available"))
      }

    ## ********************
    ## MRIO 2023k----
    ## ********************

    } else if (edition== "mrio2023k") {
      # Use last year if year not specified
      if (is.null(year)) {
        year <- 2022
        cli::cli_alert_info(c("Year not specified. Using year {year}"))
      }
      if (year == 2000) {
        xlsx_file <- "ADB-MRIO-2000-at-constant-2010-prices.xlsx"
      } else if (year %in% c(2007:2009, 2011:2022)) {
        xlsx_file <- paste0("ADB MRIO ", year,
                            ", at constant 2010 prices", ".xlsx")
      } else {
        stop(paste0("Year ", year, " is not available"))
      }
    }

    # ************
    # Names ----
    # ************

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

  # Open Excel file----
    # df <- openxlsx::read.xlsx(filepath,
    #                           rows = c(8:2220),
    #                           cols = c(5:2525),
    #                           colNames = FALSE,
    #                           na.strings = " ")
    # Note 28/08/2025
    # There are some problems with direct xlsx reading
    # e.g. non numeric characters or negative numbers turned into
    # positives (2) for -2.03 transformed into 2.03
    #  It is better to import everything and then transform to numeric
    #  (Minor chages in rows)
    df <- openxlsx::read.xlsx(filepath)
    df <- as.data.frame(lapply(df[6:2218, 5:2525], as.numeric))

  # *********************
  # MRIO 73 COUNTRIES ----
  # *********************

  } else if (edition %in% c("mrio2025x", "mrio2024x", "mrio2023x")) {

    # Dimensions----
    G <- 73
    GX <- 73
    N <- 35
    GN <- G * N
    GXN <- GX * N
    FD <- 5
    GFD <- G * FD

    # ****************
    ## MRIO 2025x----
    # ****************

    if (edition == "mrio2025x") {
      # Use last year if year not specified
      if (is.null(year)) {
        year <- 2024
        cli::cli_alert_info(c("Year not specified. Using year {year}"))
      }
      # xlsx_names
      if (year %in% c(2017)) {
        xlsx_file <- paste0("ADB-MRIO-", year, "_August 2025", ".xlsx")
      } else if (year %in% c(2018)) {
        xlsx_file <- paste0("ADB-MRIO-", year, "_September 2024", ".xlsx")
      } else if (year %in% c(2019)) {
        xlsx_file <- paste0("ADB-MRIO-", year, "_December 2024", ".xlsx")
      } else if (year %in% c(2020)) {
        xlsx_file <- paste0("ADB-MRIO-", year, "_September 2024", ".xlsx")
      } else if (year %in% c(2021)) {
        xlsx_file <- paste0("ADB-MRIO-", year, "_August 2024", ".xlsx")
      } else if (year %in% c(2022)) {
        xlsx_file <- paste0("ADB-MRIO72-", year, "_July 2025", ".xlsx")
      } else if (year %in% c(2023)) {
        xlsx_file <- paste0("ADB-MRIO72-", year, "_July 2025", ".xlsx")
      } else if (year %in% c(2024)) {
        xlsx_file <- paste0("ADB-MRIO72-", year, "_August 2025", ".xlsx")
      } else {
        stop(paste0("Year ", year, " is not available"))
      }

    # ****************
    ## MRIO 2024x----
    # ****************
    } else if (edition == "mrio2024x") {
      # Use last year if year not specified
      if (is.null(year)) {
        year <- 2023
        cli::cli_alert_info(c("Year not specified. Using year {year}"))
      }
      # xlsx_names
      if (year %in% c(2017:2018)) {
        xlsx_file <- paste0("ADB-MRIO-", year, "_September 2024", ".xlsx")
      } else if (year %in% c(2019)) {
        xlsx_file <- paste0("ADB-MRIO-", year, "_December 2024", ".xlsx")
      } else if (year %in% c(2020)) {
        xlsx_file <- paste0("ADB-MRIO-", year, "_September 2024", ".xlsx")
      } else if (year %in% c(2021:2022)) {
        xlsx_file <- paste0("ADB-MRIO-", year, "_August 2024", ".xlsx")
      } else if (year %in% c(2023)) {
        xlsx_file <- paste0("ADB-MRIO-", year, "_September 2024", ".xlsx")
      } else {
        stop(paste0("Year ", year, " is not available"))
      }

    # ************
    # MRIO 2023x----
    # ************
    } else if (edition == "mrio2023x") {
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

    }

    # Names----
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

    # Open Excel files----
    # Check that file exists
    check_wio_source_file(src_dir, xlsx_file)

    # If exists
    filepath <- paste0(src_dir, xlsx_file)

    if (!quiet) {cli::cli_alert_info(c("Importing {.file {xlsx_file}}..."))}

    # df <- openxlsx::read.xlsx(filepath,
    #                           rows = c(8:2562),
    #                           cols = c(5:2924),
    #                           colNames = FALSE,
    #                           na.strings = " ")
    #  (See note above)
    df <- openxlsx::read.xlsx(filepath)
    df <- as.data.frame(lapply(df[6:2560, 5:2924], as.numeric))

    # *********************
    # MRIO 75 COUNTRIES ----
    # *********************

  } else if (edition %in% c("mrio2025xx")) {

    # Dimensions----
    G <- 75
    GX <- 75
    N <- 35
    GN <- G * N
    GXN <- GX * N
    FD <- 5
    GFD <- G * FD

    # ****************
    ## MRIO 2025xx----
    # ****************

    if (edition == "mrio2025xx") {
      # Use last year if year not specified
      if (is.null(year)) {
        year <- 2024
        cli::cli_alert_info(c("Year not specified. Using year {year}"))
      }
      # xlsx_names
      if (year %in% c(2022)) {
        xlsx_file <- paste0("ADB-MRIO-", year, "-July 2025", ".xlsx")
      } else if (year %in% c(2023)) {
        xlsx_file <- paste0("ADB-MRIO-", year, "-July 2025", ".xlsx")
      } else if (year %in% c(2024)) {
        xlsx_file <- paste0("ADB-MRIO-", year, "-August 2025", ".xlsx")
      }else {
        stop(paste0("Year ", year, " is not available"))
      }

    }

    # Names----
    g_names <- c("AUS", "AUT", "BEL", "BGR", "BRA", "CAN", "CHE", "CHN", "CYP",
                 "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GBR", "GRC",
                 "HRV", "HUN", "IDN", "IND", "IRL", "ITA", "JPN", "KOR", "LTU",
                 "LUX", "LVA", "MEX", "MLT", "NLD", "NOR", "POL", "PRT", "ROU",
                 "RUS", "SVK", "SVN", "SWE", "TUR", "TWN", "USA", "BGD", "MYS",
                 "PHL", "THA", "VNM", "KAZ", "MNG", "LKA", "PAK", "FJI", "LAO",
                 "BRN", "BTN", "KGZ", "KHM", "MDV", "NPL", "SGP", "HKG", "ARG",
                 "COL", "ECU", "ARM", "GEO", "EGY", "KWT", "SAU", "ARE", "NZL",
                 "TJK", "UZB", "ROW")


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

    # Open Excel files----
    # Check that file exists
    check_wio_source_file(src_dir, xlsx_file)

    # If exists
    filepath <- paste0(src_dir, xlsx_file)

    if (!quiet) {cli::cli_alert_info(c("Importing {.file {xlsx_file}}..."))}

    # df <- openxlsx::read.xlsx(filepath,
    #                           rows = c(8:2640),
    #                           cols = c(5:3005), # until DKO
    #                           colNames = FALSE,
    #                           na.strings = " ")
    df <- openxlsx::read.xlsx(filepath)
    df <- as.data.frame(lapply(df[6:2638, 5:3005], as.numeric))

  }

  # ****************
  # CALCULATION OF MATRICES----
  # *********************

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
