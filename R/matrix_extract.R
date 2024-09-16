#' Get extraction matrix.
#'
#' Creates a global extraction matrix `Anots` of an exporter and its inverse
#'   `Bnots`.
#' @param wio A class `wio` object
#' @param exporter String, code of country or country group
#' @param perim String: `"country"` for country perspective and
#'   `"WLD"` for world perspective.
#' @param partner String: code of country or country group
#'   for bilateral perspectives (only with country).
#' @param sector Character string: code of sector or sector group
#'   for sector perspectives (only with country).
#' @param inverse Boolean, if `TRUE` returns the global inverse extraction
#'   matrix `Bnots`, if `FALSE` just the global extraction matrix `Anots`.
#' @return The global (inverse) extraction matrix of the specified exporter.
#' @export
get_xmatrix <- function(wio, exporter, perim = "country",
                        partner = "WLD", sector = "TOTAL",
                        inverse = TRUE) {

  # Exporter has to be calculated even in world perspective
  # because Ld might change if it is a group
  pgn_exp <- grep(get_geo_codes(exporter, wio$type, TRUE),
                  wio$names$gxn_names)

  # This is only to check group
  pg_exp <- grep(get_geo_codes(exporter, wio$type, TRUE),
                 wio$names$g_names)
  is_group <- ifelse(length(pg_exp) > 1, TRUE, FALSE)

  # Verifications
  if (perim %in% c("WLD", "world")) {
    if (!partner == "WLD") {
      stop("World perspective is not compatible with bilateral dimensions")
    }
    if (!sector == "TOTAL") {
      stop("World perspective is not compatible with sector dimensions")
    }
  }
  # Verification
  if (!perim %in% c("country", "WLD", "world")) {
    stop("Basic perimeter can only be 'country' or 'WLD'")
  }

  # ******************
  # World perspective
  # ******************
  if (perim %in% c("WLD", "world")) {

    if (is_group) {
      # Anots is Ad corrected
      Anots <- wio$A - set_zero(wio$Am, pgn_exp, pgn_exp, wio$type)
    } else {
      # Anots is Ad
      Anots <- wio$Ad
    }

  } else if (perim == "country") {

    # *************************
    # Country perspective
    # *************************
    if (all(partner == "WLD", sector == "TOTAL")) {

      Anots <- set_zero(wio$A, pgn_exp, -pgn_exp)

    # **********************
    # Bilateral perspective
    # **********************
    } else if (all(!partner == "WLD", sector == "TOTAL")) {

      pgn_par <- grep(get_geo_codes(partner, wio$type, TRUE),
                      wio$names$gxn_names)
      Anots <- set_zero(wio$A, pgn_exp, pgn_par)

    # *********************
    # Sector perspective
    # *********************
    } else if (all(partner == "WLD", !sector == "TOTAL")) {

      geo_codes <- unlist(strsplit(get_geo_codes(exporter, wio$type, TRUE),
                                   "[|]"))
      sec_codes <- unlist(strsplit(get_sec_codes(sector, wio$type, TRUE),
                                   "[|]"))
      geosec_codes <- paste0(rep(geo_codes, each = length(pg_exp)),
                             sec_codes)
      geosec_codes <- paste0(geosec_codes, collapse = "|")

      pgn_geosec <- grep(geosec_codes, wio$names$gxn_names)

      # Faltaba esto
      Anots <- set_zero(wio$A, pgn_geosec, -pgn_exp)

    # ********************************
    #  Bilateral-sector perspective
    # ********************************
    } else if (all(!partner == "WLD", !sector == "TOTAL")) {

      pgn_par <- grep(get_geo_codes(partner, wio$type, TRUE),
                      wio$names$gxn_names)

      geo_codes <- unlist(strsplit(get_geo_codes(exporter, wio$type, TRUE),
                                   "[|]"))
      sec_codes <- unlist(strsplit(get_sec_codes(sector, wio$type, TRUE),
                                   "[|]"))
      geosec_codes <- paste0(rep(geo_codes, each = length(pg_exp)),
                             sec_codes)
      geosec_codes <- paste0(geosec_codes, collapse = "|")

      pgn_geosec <- grep(geosec_codes, wio$names$gxn_names)

      Anots <- set_zero(wio$A, pgn_geosec, pgn_par)

    }

  }

  # **************
  # Output
  # *************
  if (inverse == TRUE) {
    Bnots <- solve(diag(wio$dims$GXN) - Anots)
    return(Bnots)
  } else {
    return(Anots)
  }

}


#' Make Anots
#'
#' Makes a global extraction matrix of an exporter
#' @param wio A class `wio` object
#' @param exporter Character string: code of country or country group
#' @param ... Additional parameters
#' @keywords internal
#' @noRd
#' @return A global extraction matrix of exporter
make_Anots <- function(wio, exporter, ...) {

  Anots <- get_xmatrix(wio, exporter, ..., inverse = FALSE)

  return(Anots)

}


#' Make Bnots
#'
#' Makes an inverse global extraction matrix of an exporter
#' @param wio A class wio object
#' @param exporter Character string: code of country or country group
#' @param ... Additional parameters
#' @keywords internal
#' @noRd
#' @return An inverse global extraction matrix of exporter
make_Bnots <- function(wio, exporter, ...) {

  Bnots <- get_xmatrix(wio, exporter, ..., inverse = TRUE)

  return(Bnots)

}


#' Make global Bnots
#'
#' Makes an inverse global extraction matrix for all countries
#' @param wio A class wio object
#' @param ... Rest of parameters from make_Bnots
#' @keywords internal
#' @noRd
#' @return A Bnots matrix
make_global_Bnots <- function(wio, ...) {

  gxn_names <- wio$names$gxn_names

  gBnots <- matrix(0, wio$dims$GXN, wio$dims$GXN)

  gBnots <- name(gBnots, gxn_names, gxn_names)

  cli::cli_progress_bar("Please wait...",
                        type = "iterator",
                        total = wio$dims$G)

  for (country in wio$names$g_names) {

    cli::cli_progress_update()

    Bnots <- make_Bnots(wio, country, ...)
    pgn_cou <- grep(get_geo_codes(country, wio$type, icio_extend = TRUE),
                    gxn_names)
    gBnots[, pgn_cou] <- Bnots[, pgn_cou, drop = FALSE]

  }

  return(gBnots)

}


#' Set to zero specific rows and columns of a matrix
#'
#' @description
#' Sets to zero specific rows and columns of a matrix, to
#'   include and exclude specific geographical and sector effects.
#' @param df A matrix with named rows and columns.
#' @param orig A vector of integers with position of rows or a list of strings
#'   with codes of country and sector of origin.
#' @param dest A vector of integers with position of columns or a list of
#'   strings with codes of country and sector of destination.
#' @param wiotype String, type of `wio`. Required if origin or destination
#'   is specified with lists of codes.
#' @param invert Boolean: FALSE (default) to set to zero the specified
#'  countries and sectors, or TRUE to set to zero the non-specified countries
#'  and sectors.
#' @return The same matrix with specific rows and columns set to zero.
#' @export
#' @examples
#' wio <- make_wio("wiodtest")
#' # Set to zero Spanish exports of intermediates of manufacturing to
#' # non EU27 countries (for any sector of destination) in the coefficient
#' # matrix A
#' set_zero(wio$A, list("ESP", "MANUF"), list("NONEU27", "TOTAL"), "wiodtest")
#' # Set to zero Spanish exports of intermediates (extraction matrix of Spain)
#' set_zero(wio$A, list("ESP", "TOTAL"), list("WLDxESP", "TOTAL"), "wiodtest")
set_zero <- function(df, orig = NULL, dest = NULL, wiotype = NULL,
                     invert = FALSE){

  #
  # df <- Vt_Bt
  # orig <- list("USA", "MANUF")
  # dest <- list("CHN", "all")
  # wiotype <- "iciotest"
  # invert <- FALSE
  #
  row_names <- rownames(df)
  col_names <- colnames(df)
  if (any(is.null(row_names), is.null(col_names))) {
    stop("Matrices without dimension names cannot be set to zero")
  }

  # ************************
  # Vectors with positions
  # *************************
  # If orig or dest are numeric (and the other null)
  # Then is a vector of positions
  if (all(any(is.numeric(orig), is.null(orig)),
          any(is.numeric(dest), is.null(dest)))) {

    if (all(is.null(orig), is.null(dest))) {
      df <- df
    } else if (all(!is.null(orig), is.null(dest))) {
      df[orig, ] <- 0
    } else if (all(is.null(orig), !is.null(dest))) {
      df[, dest] <- 0
    } else if (all(!is.null(orig), !is.null(dest))) {
      df[orig, dest] <- 0
    }

  # **********************************
  # Lists with origin and destination
  # **********************************
  # If orig or dest are lists (and the other null)
  } else if (all(any(is.list(orig), is.null(orig)),
                 any(is.list(dest), is.null(dest)))) {

    # If there is no wiotype, no way to know position of countries/sectors
    if (is.null(wiotype)) {
      stop(paste0("The use of geographical or sector codes require the\n",
                  "specification of the argument 'wiotype'"))
    }


    # *********************
    # Check origin (rows)
    # *********************

    # There should always be geo as orig
    ogeo <- orig[[1]]
    if (length(orig) > 1) {
      osec <- orig[[2]]
    } else{
      osec <- "TOTAL"
    }

    if (all(ogeo == "WLD", osec == "TOTAL")) {
      # ogeo_codes <- NULL
      # osec_codes <- NULL
      pgr <- c(1:length(row_names))
    } else if (all(!ogeo == "WLD", osec == "TOTAL")) {
      ogeo_codes <- get_geo_codes(ogeo, wiotype, icio_extend = TRUE)
      # osec_codes <-NULL
      pgr <- grep(ogeo_codes, row_names, invert = invert)
    } else if (all(ogeo == "WLD", !osec == "TOTAL")) {
      # ogeo_codes <- NULL
      osec_codes <- get_sec_codes(osec, wiotype, remove_letter = TRUE)
      pgr <- grep(osec_codes, row_names, invert = invert)
    } else if (all(!ogeo == "WLD", !osec == "TOTAL")) {
      ogeo_codes <- get_geo_codes(ogeo, wiotype, icio_extend = TRUE)
      ogeo_codes <- strsplit(ogeo_codes, "[|]")[[1]]
      num_geo <- length(ogeo_codes)
      osec_codes <- get_sec_codes(osec, wiotype, remove_letter = TRUE)
      osec_codes <- strsplit(osec_codes, "[|]")[[1]]
      num_sec <- length(osec_codes)
      srch <- paste0(rep(ogeo_codes, each = num_sec), osec_codes)
      srch <- paste(srch, collapse = "|")
      # print(srch)
      pgr <- grep(srch, row_names, invert = invert)
    }

    # ****************************
    # Check destination (columns)
    # ****************************

    # There should always be geo as dest
    dgeo <- dest[[1]]
    if (length(dest) > 1) {
      dsec <- dest[[2]]
    } else{
      dsec <- "TOTAL"
    }


    if (all(dgeo == "WLD", dsec == "TOTAL")) {

      pgc <- c(1:length(col_names))
    } else if (all(!dgeo == "WLD", dsec == "TOTAL")) {
      dgeo_codes <- get_geo_codes(dgeo, wiotype, icio_extend = TRUE)
      pgc <- grep(dgeo_codes, col_names, invert = invert)
    } else if (all(dgeo == "WLD", !dsec == "TOTAL")) {
      dsec_codes <- get_sec_codes(dsec, wiotype, remove_letter = TRUE)
      pgc <- grep(dsec_codes, col_names, invert = invert)
    } else if (all(!dgeo == "WLD", !dsec == "TOTAL")) {
      dgeo_codes <- get_geo_codes(dgeo, wiotype, icio_extend = TRUE)
      dgeo_codes <- strsplit(dgeo_codes, "[|]")[[1]]
      num_geo <- length(dgeo_codes)
      dsec_codes <- get_sec_codes(dsec, wiotype, remove_letter = TRUE)
      dsec_codes <- strsplit(dsec_codes, "[|]")[[1]]
      num_sec <- length(dsec_codes)
      srch <- paste0(rep(dgeo_codes, each = num_sec), dsec_codes)
      srch <- paste(srch, collapse = "|")
      pgc <- grep(srch, col_names, invert = invert)
    }

    # print(pgr)
    # print(pgc)

    # Check in case sectors (other than TOTAL) are included in rows or columns
    # with no sectors (e.g., matrix Y)
    if (length(pgr) == 0) {
      stop(paste0(dsec, " is not available in row names"))
    } else if (length(pgc) == 0) {
      stop(paste0(dsec, " is not available in column names"))
    }

    df[pgr, pgc] <- 0

  }

  return(df)

}
