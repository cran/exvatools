#' Show available countries and country groups in a specific Input-Output table
#'
#' @param wiotype Character string specifying the world input-output database.
#' @param lang Character string for the language of the descriptive text:
#'   `"eng"` for English (default) and `"es"` for Spanish.
#'
#' @return Prints country codes and descriptive text in the console.
#' @export
#' @examples
#' info_geo("icio2021")
info_geo <- function(wiotype = "icio2021", lang = "en"){

  # Select column with included elements
  # (basic_icio2021, basic_wiod2016, etc)
  # Elements will have 1 (individual) or 0 (group).
  basic <- paste0("basic", "_", wiotype)

  # Get included elements
  db <- dbgeo[dbgeo[basic] >= 0, ]

  # Put NA as empty character
  db[is.na(db)] <- ""

  # Select individual sectors (basic = 1)
  db_indiv <- db[db[basic] == 1, ]
  # Select groups
  db_group <- db[db[basic] == 0, ]

  # List of individual countries
  # It will appear as ESP (Spain) USA (United States), etc
  # altghough depending of language

  # Select language description
  txt_lang <- paste0("txt", "_", lang)

  list_indiv <- paste0(db_indiv$id, " (", db_indiv[[txt_lang]], ")")



  list_group <- paste0(db_group$id, " (", db_group[[txt_lang]], ")")

  # WIO
  txt_wio <- dbtyp[dbtyp$id == wiotype, ][["txt_en"]]


  # Start printing
  cr <- "\n"
  txt <- cr
  txt <- paste0(txt, hline(75), cr)
  txt <- paste0(txt, " ", txt_wio, cr)
  txt <- paste0(txt, hline(75), cr, cr)
  txt <- paste0(txt, "Individual countries:", cr)
  txt <- paste0(txt, vecwrap(list_indiv), cr, cr)
  txt <- paste0(txt, "Groups of countries:", cr)
  txt <- paste0(txt, vecwrap(list_group), cr, cr)

  # Print to console
  cat(txt)

}


#' Show available sectors and sector groups included in a specific Input-Output table
#'
#' @param wiotype Character string specifying the world input-output database
#' @param lang Character string for the language of the descriptive text:
#'   `eng` for English (default) and `es` for Spanish.
#'
#' @return Prints ids, sector codes and descriptive text
#' @export
#'
#' @examples
#' info_sec("icio2021")
info_sec <- function(wiotype = "icio2021", lang = "en"){

  # Select column with included elements
  # (basic_icio2021, basic_wiod2016, etc)
  # Elements will have 1 (individual) or 0 (group).
  basic <- paste0("basic", "_", wiotype)

  # Get included elements
  db <- dbsec[dbsec[basic] >= 0, ]

  # Put NA as empty character
  db[is.na(db)] <- ""

  # Select individual sectors (basic = 1)
  db_indiv <- db[db[basic] == 1, ]
  # Select groups
  db_group <- db[db[basic] == 0, ]


  # Select column of codes
  codes <- paste0("codes_", wiotype)
  # Reorder (importanto for older codes)
  db_indiv <- db_indiv[order(db_indiv[[codes]]), ]

  # Select column of notes
  notes <- paste0("notes_", wiotype)

  # Select column of description (short, for the moment)
  # with language
  txt_lang <- paste0("txt_short", "_", lang)

  list_indiv <- paste0(cli::col_red(db_indiv$id), ": ",
                       db_indiv[[codes]],
                       " (",
                       db_indiv[[txt_lang]],
                       ")")

  list_group <- paste0(cli::col_red(db_group$id), ": ",
                       db_group[[notes]],
                       " (",
                       db_group[[txt_lang]],
                       ")")

  # WIO
  txt_wio <- dbtyp[dbtyp$id == wiotype, ][["txt_en"]]

  # Start printing
  cr <- "\n"
  txt <- cr
  txt <- paste0(txt, hline(75), cr)
  txt <- paste0(txt, " ", txt_wio, cr)
  txt <- paste0(txt, hline(75), cr, cr)
  txt <- paste0(txt, "Individual sectors:", cr)
  txt <- paste0(txt, vecwrap(list_indiv), cr, cr)
  txt <- paste0(txt, "Sector groups:", cr)
  txt <- paste0(txt, vecwrap(list_group), cr, cr)

  # Print to console
  cat(txt)

}