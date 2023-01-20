#' Detailed origin and final absorption of value added in gross exports
#'
#' @description Get exports in terms of final absorption by origin of
#'   value added and final destination. It combines a [make_exvadir()]
#'   command and a [get_data()] command to obtain a result equivalent to
#'   that of the OECD's Gross Exports by Origin of Value Added and
#'   Final destination (`FD_EXGR_VA`, `FD_EXGRFNL_VA` and `FD_EXGRINT_VA`),
#'   but with much more flexible geographical and sector options.
#' @param wio_object An object of class `wio`.
#' @param va_type String for domestic content (`"DC"`), foreign content
#'   (`"FC"`) or total content (`"TC"`) from the perspective of the exporter.
#'   As origin of value added is specified, this is normally redundant, but
#'   in the case of exporter `"WLD"`, the domestic and foreign content is
#'   considered as the sum of domestic/foreign contents of all individual
#'   countries. For groups (such as `"EU27"`) domestic/foreign means value
#'   added from within/outside the group.
#' @param flow_type String specifying the type of flow in terms of absorption.
#'   It can be total gross exports (`"EXGRY"`), exports of final products
#'   (`"EXGRY_FIN"`) or exports of intermediates (`"EXGRY_INT"`).
#' @param geo_orig Character string with code of country or country group
#'   of origin of value added.
#' @param geo_export Character string with code of exporting country or
#'   country group.
#' @param sec_export Character string with code of exporting sector or
#'   sector group. Combinations (with `"|"`) and exceptions (with `"x"`)
#'   are allowed.
#' @param geo_fd String character with code of country or country group
#'   of final destination of exports
#' @param as_numeric Boolean. If `TRUE` (default), returns a numeric
#'   value, vector or matrix instead of a data frame (default for
#'   [get_data()]).
#'
#' @return A matrix, vector or data frame with data of exports
#' @export
#' @examples
#' # What part of French value added exported as US final intermediate
#' # manufactures ends up absorbed by Spain?
#' wio <- make_wio("iciotest")
#' get_va_exgry(wio, flow_type = "EXGRY_INT", geo_orig = "FRA",
#'              geo_export = "USA", sec_export = "MANUF", geo_fd = "ESP")
get_va_exgry <- function(wio_object,
                         va_type = "TC",
                         flow_type = "EXGRY", geo_orig = "WLD",
                         geo_export, sec_export = "TOTAL",
                         geo_fd = "WLD", as_numeric = TRUE) {

  # Check class
  wio <- check_object(wio_object, "wio")

  # If exporter is WLD, we consider it a sum of individual countries,
  # and include intra-exports (intra = TRUE in make_exvadir)
  # Otherwise, if WLD was a group, all VA would be domestic VA
  include_intra <- ifelse(geo_export == "WLD", TRUE, FALSE)

  exvadir <- make_exvadir(wio,
                          va_type = va_type,
                          flow_type = flow_type,
                          exporter = geo_export,
                          orig_geo = geo_orig,
                          sec_orig = "TOTAL",
                          intra = include_intra)
  result <- get_data(exvadir, var = va_type,
                     exporter = geo_orig,
                     sector = sec_export,
                     importer = geo_fd)

  if (as_numeric) {
    result <- as.numeric(result)
  }

  return(result)

}
