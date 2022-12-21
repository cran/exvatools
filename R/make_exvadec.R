#' Decomposition of value added in exports using different methodologies
#'
#' Calculates the decomposition of value added in exports of a country
#' or a group of countries according to different methodologies.
#'
#' @param wio_object An object of class `wio` (standardized world
#'   input-output table) obtained using [make_wio()].
#' @param exporter String with a country or a country group code
#'   (e.g., `"USA"`, code{"NAFTA"}, etc.). The default `"all"`
#'   produces the decomposition of value added in exports for all
#'   available individual countries.
#' @param method A string specifying the export VA decomposition method:
#'   * `"bm_src"`: Borin and Mancini, source-based (2019) (default).
#'   * `"bm_snk"`: Borin and Mancini, sink-based (2019).
#'   * `"wwz"`: Wang et al. (2013).
#'   * `"kww"`: Koopman et al. (2014).
#'   * `"kww"`: Miroudot and Ye (2021)
#'   * `"oecd"`: OECD (not properly a decomposition).
#' @param output Type of matrices in output:
#'   * `"standard"` (default): Shows the domestic content (`DC`),
#'     domestic value added (`DVA`), the domestic double counting
#'     (`DDC`), the foreign content (`FC`), the foreign value
#'     added (`FVA`) and the foreign double counting (`DDC`). The
#'     value added exported (`VAX`) is also produced in most cases, and
#'     additional indicators are provided in specific cases.
#'   * `"terms"`: Shows the basic decomposition terms, whose sum gives
#'     the value of gross exports. The number and specification of terms
#'     follows the standard in the economic literature: 12 in the Borin and
#'     Mancini (2019) decompositions (source and sink), 16 in the Wang et al.
#'     (2013) decomposition, 9 in the Koopman et al. (2014) decomposition
#'     and just 4 in the Miroudot and Ye (2021) decomposition (as the latter
#'     does not expand value added in terms of final absorption, like the
#'     rest). \
#'     For the `"wwz"` (Wang et al. ,2013) decomposition there is an
#'     additional `"terms2"` option with an alternative arrangement of
#'     terms, and for the `"oecd"` decomposition there is an
#'     additional `"tiva"` decomposition with many TiVA basic indicators.
#' @param quiet Boolean, if `TRUE` suppress all status messages. Default
#'   is `FALSE`, i.e., messages are shown.
#' @param ... Additional parameters for the Miroudot and Ye (2021)
#'   decomposition. These can be:
#'   * `perim` String, for general perimeter of value added.
#'     Default is `"country"`, but can also be `"WLD"` (world). In
#'     this case, all flows that cross the border of *any* country
#'     more than once will be considered as double counting (unlike in the
#'     country perspective, where flows are considered as double counting
#'     only when they *exit* the border of *the exporting country*
#'     more than once).
#'   * `partner` String, for bilateral perimeter of value added.
#'     Default is `"WLD"`, but it can also be any country or
#'     country group code (e.g. `"USA"` or `"EU27"`). In this case
#'     (bilateral perspective), all flows that cross the bilateral
#'     perimeter more than once will be considered as double counting.
#'   * `sector` String, for sector perimeter of value added.
#'     Default is `"TOTAL"`, but it can also be any sector or
#'     sector group code (e.g. `"MANUF"`. In this case (sector
#'     perspective), all flows that cross the sector perimeter more than
#'     once will be considered as double counting. The bilateral and sector
#'     perspectives can be combined in a bilateral-sector perspective.
#'   Please note that, when using the world perspective (`perim = "WLD"`)
#'     and the terms output (`output = "terms"`, the foreign double
#'     counting will be automatically divided in two elements.
#' @return A list object of class `exvadec` with several matrices
#'   plus metadata.
#' @references
#'    * Borin, A., & Mancini, M. (2019). Measuring What Matters in Global
#'      Value Chains and Value-Added Trade (Policy Research Working Paper
#'      No. 8804). World Bank.
#'    * Koopman, R., Wang, Z., & Wei, S.-J. (2014). Tracing Value-Added and
#'      Double Counting in Gross Exports. American Economic Review, 104(2),
#'      459–494.
#'    * Miroudot, S., & Ye, M. (2021). Decomposing Value Added in Gross
#'      Exports. Economic Systems Research, 33(1), 67–87.
#'    * Wang, Z., Wei, S.-J., & Zhu, K. (2013). Quantifying International
#'      Production Sharing at the Bilateral and Sector Levels (NBER Working
#'      Paper No. 19677). National Bureau of Economic Research, Inc.
#' @export
#' @examples
#' # Create a test wio
#' wio <- make_wio("iciotest")
#' # Make Borin and Mancini (2019) source decomposition for Spain
#' exvadec <- make_exvadec(wio, exporter = "ESP", method = "bm_src")
#' # Make Wang et al. (2013) decomposition for all countries
#' # expressed in the traditional 16 terms
#' exvadec <- make_exvadec(wio, method = "wwz", output = "terms")
make_exvadec <- function(wio_object, exporter = "all",
                         method = "bm_src", output = "standard",
                         quiet = FALSE, ...){

  # Check wio
  wio <- check_object(wio_object, "wio")

  # Check ordinary arguments
  list_args <- c(as.list(environment()))
  # Check potential Miroudot-Ye arguments
  my_args <- list(...)
  # Returns TRUE if ok
  checked_args <- check_exvadec_args(list_args, my_args)


  # Select method, exporter and output----
  if (quiet == FALSE) {
    if (method == "bm_src") {
      exvadec <- make_exvadec_bm_src(wio, exporter = exporter,
                                     output = output)
    } else if (method == "bm_snk") {
      exvadec <- make_exvadec_bm_snk(wio, exporter = exporter,
                                     output = output)
    } else if (method == "wwz") {
      exvadec <- make_exvadec_wwz(wio, exporter = exporter,
                                  output = output)
    } else if (method == "kww") {
      exvadec <- make_exvadec_kww(wio, exporter = exporter,
                                  output = output)
    } else if (method == "my") {
      exvadec <- make_exvadec_my(wio, exporter = exporter,
                                  output = output, ...)
    } else if (method == "oecd") {
      exvadec <- make_exvadec_oecd(wio, exporter = exporter,
                                   output = output)
    }
  } else if (quiet == TRUE) {
    if (method == "bm_src") {
      exvadec <- suppressMessages(make_exvadec_bm_src(wio, exporter = exporter,
                                     output = output))
    } else if (method == "bm_snk") {
      exvadec <- suppressMessages(make_exvadec_bm_snk(wio, exporter = exporter,
                                     output = output))
    } else if (method == "wwz") {
      exvadec <- suppressMessages(make_exvadec_wwz(wio, exporter = exporter,
                                  output = output))
    } else if (method == "kww") {
      exvadec <- suppressMessages(make_exvadec_kww(wio, exporter = exporter,
                                                   output = output))
    } else if (method == "my") {
      exvadec <- suppressMessages(make_exvadec_my(wio, exporter = exporter,
                                                   output = output, ...))
    } else if (method == "oecd") {
      exvadec <- suppressMessages(make_exvadec_oecd(wio, exporter = exporter,
                                   output = output))
    }
  }

  # Output----
  return(exvadec)

}
