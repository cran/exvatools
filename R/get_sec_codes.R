#' Get the ISO3 codes of standard sector groups
#'
#' @description
#' Gets the ISO3 codes of standard sector groups available for the
#'   different input-output tables. The resulting format can be used to
#'   extract elements of a matrix using `grep`.
#' @param sector_id String, sector or sector group code. Available
#'   `sector_ids` can be obtained with the command
#'   [info_sec()].
#' @param wiotype String, type of input-output database.
#' @param remove_letter Boolean. If `TRUE`, the initial letter from
#'   the sector code will be removed: `D20` or `C20` will
#'   become `_20`. This is needed to grep rows and columns, as
#'   country-sector naming follows the pattern `AUS_01T02`, i.e.,
#'   without the initial letter `D` or `C`.
#' @return Codes of sector ready to grep, e.g. `_01|_02|_03`.
#' @export
#' @examples
#' # Get sector codes for manufactures in the icio2023 database.
#' get_sec_codes("MANUF", "icio2023")
#' # Get sector codes for services (including construction)
#' get_sec_codes("SRVWC", "icio2023")
#' # Get sector codes for manufacturing, removing the first letter so
#' # the result can be used with `grep` to select specific sectors from
#' # a matrix
#' get_sec_codes("MANUF", "icio2023", remove_letter = TRUE)
get_sec_codes <- function(sector_id, wiotype = "icio2023",
                          remove_letter = FALSE){

  # Requires: dbsec
  # As sectors and codes change with editions
  # We need to include wiotype as parameter
  # load("dbsec.rda")
  # dbsec <- exvatools:::dbsec

  # Check if sector_id includes exception
  is_exception <- FALSE

  # Split string by x and check length
  sector_txt <- unlist(strsplit(sector_id, "x"))

  # If length is > 1, there is an exception
  if(length(sector_txt) > 1){
    is_exception <- TRUE
    # The sec_id to check against dbgeo is the first part
    sector_id <- sector_txt[1]
    # and the exception is the second
    exception_id <- sector_txt[2]
  } else {
    # If there is no exception
    sector_id <- sector_id
  }

  # It can be a combination "AGF|MANUF" or a basic code "D30", "D31T33"...
  # or a combination of both "AGD|D30"
  sec_ids <- unlist(strsplit(sector_id, "[|]"))
  sec_codes <- NULL
  for (sec_id in sec_ids) {
    sec_codes <- c(sec_codes, get_sec_code(sec_id, wiotype))
  }
  # Now join them
  sec_codes <- paste0(sec_codes, collapse = "|")

  if(is_exception){
    # It is easier to transform back in vector and later paste again rather
    # than replace the character | (sometimes might appears at the beginning
    # or the end). We split, using [] so "|" is considered a normal character
    # (alternatively: "\\|")
    sec_codes <- unlist(strsplit(sec_codes, "[|]"))
    #Now we treat the exception
    exception_codes <- NULL
    except_ids <- unlist(strsplit(exception_id, "[|]"))
    # We run each exception, in case some is a group e.g. WLDxEU27
    for (except_id in except_ids) {
      exception_codes <- c(exception_codes, get_sec_code(except_id, wiotype))
    }
    # Now we split the codes
    exception_codes <- unlist(strsplit(exception_codes, "[|]"))
    # We remove the exception from the original vector
    sec_codes <- sec_codes[!sec_codes %in% exception_codes]
    # and join again with paste and collapse, ready to grep
    sec_codes <- paste(sec_codes, collapse = "|")
  }

  if(remove_letter){
    if (is.rev4(wiotype)) {
      sec_codes <- gsub("[D]", "_", sec_codes)
    } else if(is.rev3(wiotype)) {
      sec_codes <- gsub("[C]", "_", sec_codes)
    } else if (wiotype == "custom") {
      sec_codes <- gsub("[D]", "_", sec_codes)
    }
  }

  return(sec_codes)

}


#' Get individual sector code
#'
#' @description Auxiliary function for get_sec_codes()
#' @param sector_id A sector or sector group code
#' @param wiotype Source database
#' @keywords internal
#' @noRd
#' @return string with codes
get_sec_code <- function(sector_id, wiotype = "icio2023") {

  # If it is a custom wio
  if (wiotype == "custom") {
    # Put the sector_id directly
    sec_codes <- sector_id
    # If it is a standard wiotype
  } else {
    # Get column in database
    code_column <- paste0("codes", "_", wiotype)
    basic_column <- paste0("basic", "_", wiotype)
    # and find code
    db <- dbsec[dbsec[[basic_column]] >= 0, ]
    sec_codes <- db[db$id == sector_id,][[code_column]]
  }

  # If code is not found in the database, length(sec_codes) will be 0
  if (length(sec_codes) == 0){
    # Is has the form of a direct sector code like D03T05?
    if (is_sector_code(sector_id)) {
      # # Get column of basic codes
      # code_basic <- paste0("basic", "_", wiotype)
      # Get list of basic codes (D01, D02...)
      basic_codes <- dbsec[dbsec[[basic_column]] == 1, ][[code_column]]
      # If code exists, set as value
      if (sector_id %in% basic_codes) {
        sec_codes <- sector_id
        # Error if not
      } else {
        cli::cli_abort(c(paste0("Sorry, ", cli::col_red("{sector_id}"),
                                " is not a valid sector code id",
                                " for '{wiotype}'."),
                         paste0("Please review the available sector codes ",
                                "with info_sec('{wiotype}').")))
      }
      #If it is not a sector code, throw error
    } else {
      cli::cli_abort(c(paste0("Sorry, ", cli::col_red("{sector_id}"),
                              " is not a valid sector code id",
                              " for '{wiotype}'."),
                       paste0("Please review the available sector codes ",
                              "with info_sec('{wiotype}').")))
    }
  }

  return(sec_codes)

}


#' is_sector_code
#'
#' Checks if text is a sector code of the form
#' D20, D20T23, C35, C25|C35, etc
#'
#' @param txt A code string
#'
#' @keywords internal
#' @noRd
#' @return A boolean, TRUE if it is a sector code
#'
#' @examples
#' is_sector_code("MANUF")
#' is_sector_code("C20T25")
#' is_sector_code("D23T25|D30T45")
is_sector_code <- function(txt){
  ltxt <- nchar(txt)
  lcode <- attr(regexpr("([CD0-9T|]+)", txt), "match.length")
  if (ltxt == lcode){
    is_code <- TRUE
  } else{
    is_code <- FALSE
  }
  return(is_code)
}
