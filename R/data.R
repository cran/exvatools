#' ICIO-type input-output table example data
#'
#' An example of an ICIO-type input-output table, with rows for `MEX` and `CHN`
#' disaggregated into `MX1` and `MX2` and `CN1` and `CN2`, respectively.
#'
#' @format A matrix of 30 by 42, composed of two sub-matrices:
#'   * Sub-matrix `Z`, intermediate inputs: 30 by 30 (10 countries with 3
#'     sectors each). 4 of those 10 are the extensions of `CHN` and `MEX`).
#'   * Sub-matrix `Yfd`, final demand: 30 by 12 (10 countries with 3 sectors
#'     each in rows, 6 countries by 2 demand components each in columns).
#' @source Data were randomly generated with an uniform distribution.
"iciotest_data"


#' WIOD-type input-output table example data
#'
#' An example of a WIOD-type input-output table.
#'
#' @format A matrix of 18 by 30, composed of two sub-matrices:
#'   * Sub-matrix `Z`, intermediate inputs: 18 by 18 (10 countries with 3
#'     sectors each).
#'   * Sub-matrix `Yfd`, final demand: 18 by 12 (6 countries with 3 sectors
#'     each in rows, 6 countries by 2 demand components each in columns).
#' @source Data were randomly generated with an uniform distribution.
"wiodtest_data"
