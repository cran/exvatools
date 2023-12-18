#' Extract ICIO table
#'
#' Extract basic matrices from source data and prepares it for
#' processing with `make_wio`
#' @param edition Character string with ICIO edition (default = last edition)
#' @param src_dir Character string for source folder of zip file
#' @param year Integer
#' @param quiet Boolean, if `TRUE`, suppress all status messages. Default
#'   is `FALSE`, i.e., messages are shown.
#' @keywords internal
#' @noRd
#' @return List with basic input-output matrices and metadata
extract_icio <- function(edition = "icio2023", src_dir,
                         year = NULL, quiet = FALSE) {

  # Default year
  if (edition %in% c("icio2023", "icio2023s")) {
    last_year <- 2020
  } else if (edition == "icio2021") {
    last_year <- 2018
  } else if (edition == "icio2018") {
    last_year <- 2015
  } else if (edition == "icio2016") {
    last_year <- 2011
  }

  # Use last year if year not specified
  if (is.null(year)) {
    year <- last_year
    cli::cli_alert_info(c("Year not specified. Using year {year}"))
  }

  # Dimensions and file names
  # ************
  # ICIO 2023
  # ************
  if (edition == "icio2023") {
    G <- 77
    GX <- 81
    N <- 45
    FD <- 6
    # Name of zip file
    if (year %in% c(1995:2000)) {
      zip_file <- "ICIO-1995-2000-extended.zip"
    } else if (year %in% c(2001:2005)) {
      zip_file <- "ICIO-2001-2005-extended.zip"
    } else if (year %in% c(2006:2010)) {
      zip_file <- "ICIO-2006-2010-extended.zip"
    } else if (year %in% c(2011:2015)) {
      zip_file <- "ICIO-2011-2015-extended.zip"
    } else if (year %in% c(2016:2020)) {
      zip_file <- "ICIO-2016-2020-extended.zip"
    } else {
      stop(paste0("Year ", year, " is not available"))
    }
    # Zip contains a zip file
    zip2_file <- paste0(year, ".zip")
    csv_file <- paste0(year, ".csv")
    g_names <- c("ARG", "AUS", "AUT", "BEL", "BGD", "BGR", "BLR", "BRA",
                 "BRN", "CAN", "CHE", "CHL", "CHN", "CIV", "CMR", "COL",
                 "CRI", "CYP", "CZE", "DEU", "DNK", "EGY", "ESP", "EST",
                 "FIN", "FRA", "GBR", "GRC", "HKG", "HRV", "HUN", "IDN",
                 "IND", "IRL", "ISL", "ISR", "ITA", "JOR", "JPN", "KAZ",
                 "KHM", "KOR", "LAO", "LTU", "LUX", "LVA", "MAR", "MEX",
                 "MLT", "MMR", "MYS", "NGA", "NLD", "NOR", "NZL", "PAK",
                 "PER", "PHL", "POL", "PRT", "ROU", "RUS", "SAU", "SEN",
                 "SGP", "SVK", "SVN", "SWE", "THA", "TUN", "TUR", "TWN",
                 "UKR", "USA", "VNM", "ZAF", "ROW")
    gx_names <- c(g_names, "MX1", "MX2", "CN1", "CN2")
    n_names <- c("D01T02", "D03", "D05T06", "D07T08", "D09", "D10T12",
                 "D13T15", "D16", "D17T18", "D19", "D20", "D21", "D22",
                 "D23", "D24", "D25", "D26", "D27", "D28", "D29", "D30",
                 "D31T33", "D35", "D36T39", "D41T43", "D45T47", "D49",
                 "D50", "D51", "D52", "D53", "D55T56", "D58T60",
                 "D61", "D62T63", "D64T66", "D68", "D69T75", "D77T82",
                 "D84", "D85", "D86T88", "D90T93", "D94T96", "D97T98")
    fd_names <- c("HFCE", "NPISH", "GGFC", "GFCF", "INVNT", "DIRPA")
  # ************
  # ICIO 2023s
  # ************
  } else if (edition == "icio2023s") {
    G <- 77
    GX <- 77
    N <- 45
    FD <- 1
    # Name of zip file
    if (year %in% c(1995:2000)) {
      zip_file <- "ICIO-1995-2000-small.zip"
    } else if (year %in% c(2001:2005)) {
      zip_file <- "ICIO-2001-2005-small.zip"
    } else if (year %in% c(2006:2010)) {
      zip_file <- "ICIO-2006-2010-small.zip"
    } else if (year %in% c(2011:2015)) {
      zip_file <- "ICIO-2011-2015-small.zip"
    } else if (year %in% c(2016:2020)) {
      zip_file <- "ICIO-2016-2020-small.zip"
    } else {
      stop(paste0("Year ", year, " is not available"))
    }
    # Zip contains a zip file
    zip2_file <- paste0(year, ".SML", ".zip")
    # csv_file
    csv_file <- paste0(year, ".SML", ".csv")
    g_names <- c("ARG", "AUS", "AUT", "BEL", "BGD", "BGR", "BLR", "BRA",
                 "BRN", "CAN", "CHE", "CHL", "CHN", "CIV", "CMR", "COL",
                 "CRI", "CYP", "CZE", "DEU", "DNK", "EGY", "ESP", "EST",
                 "FIN", "FRA", "GBR", "GRC", "HKG", "HRV", "HUN", "IDN",
                 "IND", "IRL", "ISL", "ISR", "ITA", "JOR", "JPN", "KAZ",
                 "KHM", "KOR", "LAO", "LTU", "LUX", "LVA", "MAR", "MEX",
                 "MLT", "MMR", "MYS", "NGA", "NLD", "NOR", "NZL", "PAK",
                 "PER", "PHL", "POL", "PRT", "ROU", "RUS", "SAU", "SEN",
                 "SGP", "SVK", "SVN", "SWE", "THA", "TUN", "TUR", "TWN",
                 "UKR", "USA", "VNM", "ZAF", "ROW")
    gx_names <- g_names
    n_names <- c("D01T02", "D03", "D05T06", "D07T08", "D09", "D10T12",
                 "D13T15", "D16", "D17T18", "D19", "D20", "D21", "D22",
                 "D23", "D24", "D25", "D26", "D27", "D28", "D29", "D30",
                 "D31T33", "D35", "D36T39", "D41T43", "D45T47", "D49",
                 "D50", "D51", "D52", "D53", "D55T56", "D58T60",
                 "D61", "D62T63", "D64T66", "D68", "D69T75", "D77T82",
                 "D84", "D85", "D86T88", "D90T93", "D94T96", "D97T98")
    fd_names <- c("FD")
  # ************
  # ICIO 2021
  # ************
  } else if (edition == "icio2021") {
    G <- 67
    GX <- 71
    N <- 45
    FD <- 6
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
    csv_file <- paste0("ICIO2021_", year, ".csv")
    g_names <- c("AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CRI", "CZE",
                 "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL",
                 "IRL", "ISR", "ITA", "JPN", "KOR", "LVA", "LTU", "LUX",
                 "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN",
                 "ESP", "SWE", "CHE", "TUR", "GBR", "USA", "ARG", "BGD",
                 "BLR", "BRA", "BRN", "BGR", "KHM", "CMR", "CHN", "CIV",
                 "HRV", "CYP", "EGY", "HKG", "IND", "IDN", "JOR", "KAZ",
                 "LAO", "MYS", "MLT", "MAR", "MMR", "NGA", "PAK", "PER",
                 "PHL", "ROU", "RUS", "SAU", "SEN", "SGP", "ZAF", "TWN",
                 "THA", "TUN", "UKR", "VNM", "ROW")
    gx_names <- c(g_names, "MX1", "MX2", "CN1", "CN2")
    n_names <- c("D01T02", "D03", "D05T06", "D07T08", "D09", "D10T12",
                 "D13T15", "D16", "D17T18", "D19", "D20", "D21", "D22",
                 "D23", "D24", "D25", "D26", "D27", "D28", "D29", "D30",
                 "D31T33", "D35", "D36T39", "D41T43", "D45T47", "D49",
                 "D50", "D51", "D52", "D53", "D55T56", "D58T60",
                 "D61", "D62T63", "D64T66", "D68", "D69T75", "D77T82",
                 "D84", "D85", "D86T88", "D90T93", "D94T96", "D97T98")
    fd_names <- c("HFCE", "NPISH", "GGFC", "GFCF", "INVNT", "DIRPA")
  # ************
  # ICIO 2018
  # ************
  } else if (edition == "icio2018") {
    G <- 65
    GX <- 69
    N <- 36
    FD <- 6
    zip_file <- paste0("ICIO2018_", year, ".zip")
    csv_file <- paste0("ICIO2018_", year, ".CSV") # Uppercase
    g_names <- c("AUS", "AUT", "BEL", "CAN", "CHL", "CZE", "DNK",
                 "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL",
                 "IRL", "ISR", "ITA", "JPN", "KOR", "LVA", "LTU",
                 "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT",
                 "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR",
                 "USA", "ARG", "BRA", "BRN", "BGR", "KHM", "CHN",
                 "COL", "CRI", "HRV", "CYP", "IND", "IDN", "HKG",
                 "KAZ", "MYS", "MLT", "MAR", "PER", "PHL", "ROU",
                 "RUS", "SAU", "SGP", "ZAF", "TWN", "THA", "TUN",
                 "VNM", "ROW")
    gx_names <- c(g_names, "MX1", "MX2", "CN1", "CN2")
    n_names <- c("D01T03", "D05T06", "D07T08", "D09", "D10T12",
                 "D13T15", "D16", "D17T18", "D19", "D20T21", "D22",
                 "D23", "D24", "D25", "D26", "D27", "D28", "D29", "D30",
                 "D31T33", "D35T39", "D41T43", "D45T47", "D49T53",
                 "D55T56", "D58T60", "D61", "D62T63", "D64T66", "D68",
                 "D69T82", "D84", "D85", "D86T88", "D90T96", "D97T98")
    fd_names <- c("HFCE", "NPISH", "GGFC", "GFCF", "INVNT", "DIRPA")
  # ************
  # ICIO 2016
  # ************
  } else if (edition == "icio2016") {
    G <- 64
    GX <- 71
    N <- 34
    FD <- 6
    zip_file <- paste0("ICIO2016_", year, ".zip")
    csv_file <- paste0("ICIO2016_", year, ".csv")
    g_names <- c("AUS", "AUT", "BEL", "CAN", "CHL", "CZE", "DNK",
                 "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL",
                 "IRL", "ISR", "ITA", "JPN", "KOR", "LVA", "LUX",
                 "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK",
                 "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA",
                 "ARG", "BGR", "BRA", "BRN", "CHN", "COL", "CRI",
                 "CYP", "HKG", "HRV", "IDN", "IND", "KHM", "LTU",
                 "MLT", "MYS", "MAR", "PER", "PHL", "ROU", "RUS",
                 "SAU", "SGP", "THA", "TUN", "TWN", "VNM", "ZAF",
                 "ROW")
    gx_names <- c(g_names, "MX1", "MX2", "MX3", "CN1", "CN2", "CN3", "CN4")
    n_names <- c("C01T05", "C10T14", "C15T16", "C17T19", "C20",
                 "C21T22", "C23", "C24", "C25", "C26", "C27",
                 "C28", "C29", "C30T33X31", "C31", "C34", "C35",
                 "C36T37", "C40T41", "C45", "C50T52", "C55",
                 "C60T63", "C64", "C65T67", "C70", "C71", "C72",
                 "C73T74", "C75", "C80", "C85", "C90T93", "C95")
    fd_names <- c("HFCE", "NPISH", "GGCF", "GFCF", "INVNT", "DIRPA")
  }

  # Derived dimensions and names
  GN <- G * N
  GXN <- GX * N
  # FD <- 6
  GFD <- G * FD
  gn_names <- paste0(rep(g_names, each = N), gsub("[C|D]", "_", n_names))
  gxn_names <- paste0(rep(gx_names, each = N), gsub("[C|D]", "_", n_names))
  gfd_names <- paste0(rep(g_names, each = FD), "_", fd_names)

  # Check that file exists
  check_wio_source_file(src_dir, zip_file)

  # Get temporary directory and use data.table
  td <- tempdir()

  if (!quiet) {cli::cli_alert_info(c("Unzipping {.file {csv_file}}..."))}

  # In case zip contained another zip
  if (edition %in% c("icio2023", "icio2023s")) {
    utils::unzip(paste0(src_dir, zip_file), file = zip2_file, exdir = td)
    zip2path <- paste0(td, "\\", zip2_file)
    # For OS compatibility
    zip2path <- gsub("\\\\", "/", zip2path)
    utils::unzip(zip2path, file = csv_file, exdir = td)
    unlink(zip2path)
  } else {
    utils::unzip(paste0(src_dir, zip_file), file = csv_file, exdir = td)
  }

  # Path to csv file
  filepath <- paste0(td, "\\", csv_file)
  # For OS compatibility
  filepath <- gsub("\\\\", "/", filepath)

  # Extract data
  df <- data.table::fread(filepath, stringsAsFactors = FALSE)

  # Get row names
  rowx_names <- as.character(df[[1]])

  # Remove first column of names
  df <- df[, -1]

  # Convert to matrix
  df <- as.matrix(df)

  # Get column names
  colx_names <- colnames(df)

  # Delete temporary file
  unlink(filepath)
  # rm(td)

  # Basic matrices: Z, Y, X, VA
  if (!quiet) { cli::cli_alert_info("Getting matrices Z, Y, X")}

  Z <- as.matrix(df[1:GXN, 1:GXN])
  rownames(Z) <- gxn_names

  # Y with FD components
  Yfd <- as.matrix(df[1:GXN, (GXN + 1):(GXN + GFD)])
  rownames(Yfd) <- gxn_names

  # Grouping Yfd and calculating Y, X, VA
    # *****************************************
  # icio2016 (special case with discrepancy)
  # *****************************************
  if (edition == "icio2016") {

    # Discrepancy
    if (!quiet) {cli::cli_alert_info("Treating discrepancy...")}
    DISC <- as.matrix(df[1:GXN, (GXN + GFD + 1)])

    X <- rowSums(Z) + rowSums(Yfd) + as.numeric(DISC)
    VA <- X - colSums(Z)
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
    VA <- c(VA[1:GN], rep(0, N), VA[(GN + 1):GXN])
    X <- c(X[1:GN], rep(0, N), X[(GN + 1):GXN])

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
  # *********************
  # Rest of cases
  # *********************
  } else {

    # Aggregation of Yfd
    Y <- matrix(0, GXN, G)
    for(r in 1:G) {
      p <- (r - 1)*FD + 1
      q <- (r - 1)*FD + FD
      # Check case FD=1
      if (p == q) {
        Y[, r] <- Yfd[, p]
      } else {
        Y[, r] <- rowSums(Yfd[, p:q])
      }

    }
    rownames(Y) <- gxn_names
    colnames(Y) <- g_names

    # X and VA
    X <- as.numeric(rowSums(Z) + rowSums(Y))
    VA <- as.numeric(X - colSums(Z))
    names(X) <- names(VA) <- gxn_names

  }

  # Name all matrices
  rownames(Z) <- colnames(Z) <- rownames(Y) <- rownames(Yfd) <- gxn_names
  colnames(Y) <- g_names
  colnames(Yfd) <- gfd_names
  names(VA) <- names(X) <- gxn_names

  # Create object io
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

  io$type <- edition

  io$year <- year

  return(io)

}
