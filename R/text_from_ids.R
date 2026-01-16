#' Get a descriptive text from a series of id codes
#'
#' Gives a descriptive text of id codes from a vector of id codes, like
#' those in rows or columns of `exvatools` objects.
#'
#' @param id_list String or string vector of ids (in rows, columns, etc)
#' @param type Type of id: `"sec"` for sector ids, `"geo"` for country or
#'   country group ids, `"va"` for value-added component ids.
#' @param lang Character string for the language of the descriptive text:
#'   `"eng"` for English (default) and `"es"` for Spanish.
#' @param ver Version: `"short"` (default) or `"long"`.
#' @param wiotype Type of `wio` (which determines the codes and text)
#'
#' @return A string vector with text
#' @export
#' @examples
#' txt_from_ids("MANUF")
#' txt_from_ids("MANUF", lang = "es")
txt_from_ids <- function(id_list, type = "sec", lang = "en",
                         ver = "short", wiotype = "icio2023"){
  # Initialize
  txt_list <- NULL

  for (id in id_list) {
    # *************************
    # SEC CODE
    # ***********************
    if (type == "sec") {
      wiotypeq <- dbqv[dbqv$id == wiotype, ][["eqv_id_sec"]]
      # Get name of country (needed as length varies: may be NAFTA)
      cou <- strsplit(id, "_", fixed = TRUE)[[1]][1]
      if (cou == id) {
        # Do nothing: use id
        sec_id <- id
      } else {
        # Use second part _code
        sec_id <- strsplit(id, "_", fixed = TRUE)[[1]][2]
        if (is_sector_code(sec_id)) {
          if (is.rev4(wiotype)) {
            sec_id <- paste0("D", sec_id)
          } else if (is.rev3(wiotype)) {
            sec_id <- paste0("C", sec_id)
          }
        }
      }
      # Now we have MANUF or D03T05
      if (is_sector_code(sec_id)) {
        search_column <- paste0("codes", "_", wiotypeq)
        text_column <- paste0("txt", "_", ver, "_", lang)
        basic_column <- paste0("basic", "_", wiotypeq)
        db <- dbsec[dbsec[[basic_column]] > 0, ]
      } else {
        search_column <- "id"
        text_column <- paste0("txt", "_", ver, "_", lang)
        basic_column <- paste0("basic", "_", wiotypeq)
        db <- dbsec[dbsec[[basic_column]] >= 0, ]
      }
      # We search
      txt <- db[db[[search_column]] == sec_id, ][[text_column]]
      if (identical(txt, character(0))) {
        txt_list <- c(txt_list, "NA")
      } else{
        txt_list <- c(txt_list, txt)
      }
      # *************************
      # GEO CODE
      # ***********************
    } else if (type == "geo") {
      wiotypeq <- dbqv[dbqv$id == wiotype, ][["eqv_id_geo"]]
      geo_id <- id
      search_column <- "id"
      text_column <- paste0("txt", "_", lang)
      basic_column <- paste0("basic", "_", wiotype)
      db <- dbgeo[dbgeo[[basic_column]] >= 0, ]
      # Search
      txt <- db[db[[search_column]] == geo_id, ][[text_column]]
      if (identical(txt, character(0))) {
        txt_list <- c(txt_list, "NA")
      } else{
        txt_list <- c(txt_list, txt)
      }
      # *************************
      # VA CODE
      # ***********************
    } else if (type == "va") {
      va_id <- id
      search_column <- "id"
      text_column <- paste0("txt", "_", lang)
      # This is not very exact
      db <- dbva[!duplicated(dbva$id), ]
      # Search
      txt <- db[db[[search_column]] == va_id, ][[text_column]]
      if (identical(txt, character(0))) {
        txt_list <- c(txt_list, "NA")
      } else{
        txt_list <- c(txt_list, txt)
      }

    }

    # End loop
  }

  return(txt_list)

}
