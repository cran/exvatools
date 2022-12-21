#' Detailed origin and destination of value added in gross exports
#'
#' @description Origin of value added in gross exports. It combines a
#'   [make_exvadir()] command and a [get_data()] command to obtain
#'   a result equivalent to the OECD's Origin of Value added in Gross Exports
#'   `EXGR_BSCI`, but with much more flexible geographical and sector options.
#' @param wio_object An object of class `wio`.
#' @param va_type Character string specifying the output as domestic
#'   content (`"DC"`), foreign content (`"FC"`) or total
#'   content (`"TC"`) from the perspective of the exporter.
#'   As origin of value added is specified, this is normally redundant, but in
#'   the case of exporter `"WLD"`, the domestic and foreign content is
#'   considered as the sum of domestic/foreign contents of all individual
#'   countries. For groups (such as `"EU27"`) domestic/foreign means value
#'   added from within/outside the group.
#' @param geo_orig Character string with code of country or country group
#'   of origin of value added
#' @param sec_orig Character string with code of sector or sector group
#'   of origin of value added. Combinations (with `"|"`) and exceptions
#'   (with `"x"`) are allowed.
#' @param geo_export Character string with code of exporting country or
#'   country group.
#' @param sec_export Character string with code of exporting sector or
#'   sector group. Combinations (with `"|"`) and exceptions (with `"x"`)
#'   are allowed.
#' @param as_numeric Boolean specifying whether to return a numeric value or
#'   matrix (`TRUE`, default) or a data frame (default for
#'   [get_data()]).
#' @return A matrix, vector or data frame with export value added data.
#' @export
#' @examples
#' wio <- make_wio("iciotest")
#' # Exports of manufactures of Spain using foreign VA from France
#' get_va_exgr(wio, "FC", "FRA", "TOTAL", "ESP", "MANUF")
get_va_exgr <- function(wio_object, va_type = "FC", geo_orig = "all",
                         sec_orig = "TOTAL", geo_export,
                         sec_export = "TOTAL", as_numeric = TRUE) {

  # Check class----
  wio <- check_object(wio_object, "wio")

  # If exporter is WLD, we consider it a sum of individual countries,
  # and include intra-exports (intra = TRUE in make_exvadir)
  # Otherwise, if WLD was a group, all VA would be domestic VA
  include_intra <- ifelse(geo_export == "WLD", TRUE, FALSE)

  exvadir <- make_exvadir(wio,
                          exporter = geo_export,
                          va_type = va_type,
                          flow_type = "EXGR",
                          orig_geo = geo_orig,
                          sec_orig = sec_orig,
                          intra = include_intra)
  result <- get_data(exvadir, var = va_type,
                     exporter = geo_orig,
                     sector = sec_export)

  if (as_numeric) {
    result <- as.numeric(result)
  }

  return(result)

}
