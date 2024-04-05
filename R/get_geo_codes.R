#' Get the ISO3 codes of standard country groups
#'
#' @description
#' Gets the ISO3 codes of standard country groups available for the
#'   different input-output tables. The resulting format can be used to
#'   extract elements of a matrix using `grep`.
#' @param geo_id String, country group id. Available `geo_ids` for a
#'   specific input-output table can be obtained with the command
#'   [info_geo()].
#' @param wiotype String, type of input-output table.
#' @param icio_extend Boolean. If `TRUE` and the input-output table
#'   is of type `icio` extended, codes will also include the extended elements
#'   for China (`CN1`, `CN2`) and Mexico (`MX1`, `MX2`).
#' @return Codes of country/countries ready to grep, e.g. `AUS|ARG|BEL`
#' @export
#' @examples
#' # Get the codes of EU27 countries
#' get_geo_codes("EU27", "icio2023")
#' # Gets the codes for NAFTA and extends MEX to MX1|MX2
#' get_geo_codes("NAFTA", "icio2023", icio_extend = TRUE)
get_geo_codes <- function(geo_id, wiotype="icio2023", icio_extend=FALSE){

  # Get string vector with grep-ready code of a country
  # or a group of countries included in dbgeo: EU27, NONEU27, NAFTA
  # An exception is allowed if introduced by lowercase x,
  # e.g. EU27xESP means all EU27 codes except ESP,
  # WLDxESP is world except ESP (rest of the world for Spain)
  # Useful for instance when analyzing VA from Spain (domestic)
  # and foreign from the rest of the EU

  # Check is geo_id is a variable defined in the environment
  # i.e., if geo_id is varname <- c("AUS|FRA|ITA")
  # If it is, pass this
  # if(exists(deparse(substitute(geo_id)))){
  #
  # }

  # Load dbgeo object
  # load("dbgeo.rda")
  # dbgeo <- exvatools:::dbgeo

  # Check if geo_id includes exception
  is_exception <- FALSE
  # Split string by x and check length
  geo_txt <- unlist(strsplit(geo_id, "x"))
  # If length is >1, there is an exception
  if (length(geo_txt) > 1) {
    is_exception <- TRUE
    # The geo_id to check against dbgeo is the first part
    geo_id <- geo_txt[1]
    # and the exception is the second
    exception_id <- geo_txt[2]
  } else {
    # If there is no exception
    geo_id <- geo_id
  }

  # It can be a combination "ESP|FRA" or "EU27|USA...
  geo_ids <- unlist(strsplit(geo_id, "[|]"))
  geo_codes <- NULL
  for (g_id in geo_ids) {
    geo_codes <- c(geo_codes, get_geo_code(g_id, wiotype))
  }
  # Now join them
  geo_codes <- paste0(geo_codes, collapse = "|")

  # Correct if exception
  if(is_exception){
    # It is easier to transform back in vector and later paste again rather
    # than replace the character | (sometimes might appears at the beginning
    # or the end). We split, using [] so "|" is considered a normal character
    # (alternatively: "\\|")
    geo_codes <- unlist(strsplit(geo_codes, "[|]"))
    # Treat the exception
    exception_codes <- NULL
    except_ids <- unlist(strsplit(exception_id, "[|]"))
    # We run each exception, in case some is a group e.g. WLDxEU27
    for (except_id in except_ids) {
      if (wiotype == "custom") {
        exception_codes <- c(exception_codes, except_id)
      } else {
        exception_codes <- c(exception_codes, get_geo_code(except_id, wiotype))
      }
    }
    # Now we split the codes
    exception_codes <- unlist(strsplit(exception_codes, "[|]"))
    # We remove the exception from the original vector
    geo_codes <- geo_codes[!geo_codes %in% exception_codes]
    # and join again with paste and collapse, ready to grep
    geo_codes <- paste(geo_codes, collapse = "|")
  }

  # Extend codes of CHN and MEX in icio
  # Does not replace, but add  CHN by CN1|CN2 and MEX by MX1|MX2
  if (icio_extend == TRUE){
    if(grepl("MEX", geo_codes, fixed = TRUE)){
      if (is.iciolong(wiotype)) {
        geo_codes <- paste0(geo_codes, "|", "MX1|MX2|MX3")
      } else if (is.icioshort(wiotype)) {
        geo_codes <- paste0(geo_codes, "|", "MX1|MX2")
      }
    }
    if (grepl("CHN", geo_codes, fixed = TRUE)){
      if (is.iciolong(wiotype)) {
        geo_codes <- paste0(geo_codes, "|", "CN1|CN2|CN3|CN4")
      } else if (is.icioshort(wiotype)) {
        geo_codes <- paste0(geo_codes, "|", "CN1|CN2")
      }
    }
  }

  return(geo_codes)

}


#' Get individual geographical code
#'
#' @description Auxiliary function for get_geo_codes()
#' @param geo_id String with a country or country group code
#' @param wiotype Source database
#' @keywords internal
#' @noRd
#' @return string with codes
get_geo_code <- function(geo_id, wiotype = "icio2023") {

  # If it is a custom wio
  if (wiotype == "custom") {
    # Put the geo_id directly
    geo_codes <- geo_id
    # If it is a standard wiotype
  } else {
    # Get equivalent wiotype
    wiotypeq <- dbqv[dbqv$id == wiotype, ][["eqv_id_geo"]]
    # Get column in database0
    code_column <- paste0("codes", "_", wiotypeq)
    basic_column <- paste0("basic", "_", wiotypeq)
    # and find code
    db <- dbgeo[dbgeo[[basic_column]] >= 0, ]
    geo_codes <- db[db$id == geo_id, ][[code_column]]
  }

  # If code is not found in the database, length(geo_codes) will be 0
  if (length(geo_codes) == 0){
    cli::cli_abort(c(paste0("Sorry, ", cli::col_red("{geo_id}"),
                            " is not a valid country code id",
                            " for '{wiotype}'."),
                     paste0("Please review the available country codes ",
                            "with info_geo('{wiotype}').")))
  }

  return(geo_codes)

}
