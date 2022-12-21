#' Value added induced by final demand
#'
#' @description Details of both geographical and sector origin of the
#'   VA incorporated in exports induced by final demand. Equivalent to the
#'   OECD's Origin of Value added in Final Demand (`FDVA_BSCI`), but with much
#'   more flexible geographical and sector options.
#' @param wio_object A `wio` object
#' @param va_type String character with the type of VA induced (VA domestically
#'   absorbed `"VAD"` or exported `"VAX"`) or the equivalent inducing VA
#'   (domestic final demand `"DFD"` or foreign final demand
#'   `"FFD"`). That is, `"VAD"` and `"DFD"` will produce the
#'   same result, and so will `"VAX"` and `"FFD"`. Default is both,
#'   i.e. `"TOTAL"` VA o total demand.
#' @param geo_orig String character with code of country or country group
#'   generating value added, i.e., exporter. Default is `"all"`)
#' @param sec_orig String character with code of sector or sector group
#'   generating value added. Default: `"all"`)
#' @param geo_fd String character with code of country (or country group)
#'   of final demand (inducing the generation of VA)
#' @param sec_fd String character with code of sector (or sector group)
#'   of final demand (inducing the generation of VA)
#' @param intra Boolean for inclusion of intra-regional exports
#'   (default: `FALSE`)
#' @return Matrix with source and destination of value added.
#' @export
#' @examples
#' wio <- make_wio("iciotest")
#' # Get USA's total VA in services induced by China's manufacturing
#' get_va_fd(wio, geo_orig = "USA", sec_orig = "SRVWC",
#'          geo_fd = "CHN", sec_fd = "MANUF")
#' # Get world VA exported (VAX), i.e., world VA induced by the rest of
#' # the world not domestically absorbed
#' get_va_fd(wio, "VAX", "WLD", "TOTAL", "WLD", "TOTAL")
get_va_fd <- function(wio_object, va_type ="TOTAL",
                      geo_orig = "WLD", sec_orig = "TOTAL",
                      geo_fd = "WLD", sec_fd = "TOTAL",
                      intra = FALSE) {

  # get_geo_codes, get_sec_codes, meld,
  # Remember: fdva is read from right to left:
  # The final demand of sector sec_fd in country geo_fd
  # induces the VA exported from sector sec_orig of country geo_orig
  # In VBY the exporter is always the origin of VA (unlike in VBE where
  # both are specified)

  # Check class----
  wio <- check_object(wio_object, "wio")

  # Check icio----
  wio_type <- wio$type
  is_icio <- is.icio(wio_type)

  # Dimensions----
  G <- wio$dims$G
  N <- wio$dims$N
  GX <- wio$dims$GX
  GN <- G * N
  GXN <- GX*N

  # Names----
  gxn_names <- wio$names$gxn_names
  gn_names <- wio$names$gn_names
  gx_names <- wio$names$gx_names
  g_names <- wio$names$g_names
  n_names <- wio$names$n_names

  # Position of geo_orig (exporter)
  # Remember: in VBY, the exporter is the origin of VA
  intra <- FALSE
  # We consider "WLD" not as a group, but as the sum of all indiviudal
  # countries (so we can obtain world's VAX or VAD). IN that case, we need
  # to include intra-trade (otherwise all content would be domestic)
  if (geo_orig == "WLD") {
    intra <- TRUE
  }
  if (geo_orig == "all") {
    geo_orig <- wio$names$g_names
  }

  # Matrix VB----
  VB <- dmult(wio$W, wio$B)

  # Final demand
  Y <- wio$Y

  # Specific country of final demand----
  # We make the columns of countries not FD as 0
  if(!geo_fd == "all"){
    pg_imp <- grep(get_geo_codes(geo_fd, wio_type), g_names)
    Y[, -pg_imp] <- 0
  }

  # Specific sector or final demand----
  # We make the rows of sectors not origin as 0 (for all countries)
  if(!sec_fd == "all"){
    pg_sec_fd <- grep(get_sec_codes(sec_fd, wio_type, remove_letter = TRUE),
                       gxn_names)
    Y[-pg_sec_fd, ] <- 0
  }


  # Calculation of VBY----
  VBY <- meld(VB %*% Y)


  # Position of exporter = origin of VA
  # Do not expand icio, as matrix is already melded
  # Normally we would not need it, as exporter (origin) will
  # be defined by get_data(), but we need to consider groups
  # of countries (e.g., EU27) to include intra-exports.
  pgn_exp <- grep(get_geo_codes(geo_orig, wio_type), gn_names)
  pg_exp <- grep(get_geo_codes(geo_orig, wio_type), g_names)
  is_group <- ifelse(length(pg_exp) > 1, TRUE, FALSE)


  # VA domestic or VA exported

  # Total value added
  if (va_type %in% c("TOTAL", "TVA", "VAT", "TFD")) {
    VBY <- VBY
  # Value Added Domestic (VA induced by Domestic Final Demand)
  } else if(va_type %in% c("VAD", "DFD", "DXD", "DOM", "DVA", "DC")) {
    VBYm <- bkoffd(VBY)
    if (intra == FALSE) {
      VBYm[pgn_exp, pg_exp] <- 0
    }
    VBYd <- VBY - VBYm
    VBY <- VBYd
  # Value Added Exported (VA induced Foreign Final Demand)
  } else if (va_type %in% c("VAX", "FFD", "FVA", "FC")) {
    VBYm <- bkoffd(VBY)
    if (intra == FALSE) {
      VBYm[pgn_exp, pg_exp] <- 0
    }
    VBY <- VBYm
  }

  rownames(VBY) <- gn_names
  colnames(VBY) <- g_names


  # Output----
  fdva <- list(VBY)
  fdva_names <- "VBY"
  names(fdva) <- fdva_names

  fdva$va_type <- va_type
  # fdva$exporter <- geo_orig
  # fdva$orig_geo <- orig_geo
  fdva$sec_orig <- sec_orig
  fdva$intra <- intra

  fdva$dims <- wio$dims
  fdva$names <- wio$names
  fdva$source <- wio$type
  fdva$year <- wio$year

  class(fdva) <- "exvadec"


  result <- get_data(fdva, "VBY",
                     exporter = geo_orig,
                     sector = sec_orig)

  return(result)

}



