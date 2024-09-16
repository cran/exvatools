#' Get a summary decomposition of value added in exports
#'
#' Detail from an `exvadec` decomposition of a country by sector and
#' by destination
#'
#' @param exvadec_object An `exvadec` object created by
#'   [make_exvadec()].
#' @param exporter A character string with the code for the exporting country
#' @param sector Character code of sector
#' @param importer Character code of importer
#'
#' @return Print result to console
#' @export
#' @examples
#' wio <- make_wio("wiodtest", quiet = TRUE)
#' exvadec <- make_exvadec(wio, quiet = TRUE)
#' get_exvadec_bkdown(exvadec, "ESP", "MANUF")
get_exvadec_bkdown <- function(exvadec_object, exporter = "WLD",
                            sector = "TOTAL", importer = "WLD"){

  # Check arguments
  list_args <- c(as.list(environment()))
  checked_args <- check_exvadec_bkdown_args(list_args)

  # Validated arguments
  exvadec <- checked_args$exvadec_object
  exporter <- checked_args$exporter
  sector <- checked_args$sector
  importer <- checked_args$importer

  # exvadec <- exvadec_object

  # exporter <- "ESP"
  # sector <- "TOTAL"
  # importer <- "WLD"


  method <- exvadec$method
  output <- exvadec$output


  # Carriage return
  cr <- "\n"


  # *********************
  # TABLE WITH DATA----
  # *********************

  # Extract indicators for specified method and output
  # from database
  df <- dbva[dbva$method == method & dbva$output == output, ]

  # Get total exports
  EXGR <- get_data(exvadec,  var = "EXGR", exporter = exporter,
                   sector = sector, importer = importer)

  # Create a basic data frame with three columns:
  # id code, level and description

  # Initialize
  row_labels <- NULL
  tbl_values <- NULL
  term <- 0
  for (i in seq_along(df[["id"]])){

    var_code <- df[i, ][["id"]]
    var_level <- df[i, ][["level"]]
    var_txt <- df[i, ][["txt_en"]]

    # print(paste0("Extracting data from ", var_code))

    # value <- round(get_data(exvadec, var_code, exporter = exporter,
    #                         sector = sector, importer = importer), 2)
    # valuesh <- round(100 * value/EXGR, 2)

    # Let us keep full decimals
    value <- get_data(exvadec, var_code, exporter = exporter,
                      sector = sector, importer = importer)
    valuesh <- 100 * value/EXGR

    # Text will depend on the type of output
    # basic/Standard show   XXXXXXXXXXXXXXXXX (XXX)  XXX.XX  XX.XX
    # Terms will show       T01 XXXXXXXXXXXXX (XXX)  XXX.XX  XX.XX
    # Full or TiVA show     XXXXXXXXXXXXXXXXX (XXX)  XXX.XX  XX.XX
    if (output %in% c("basic", "standard", "tiva")) {

      var_name <- paste0(spc(var_level, 2), var_txt, " (", var_code, ")")

    } else if (output %in% c("terms", "terms2")){
      # Add term X to every terms (except EGR)
      if (!var_code == "EXGR"){
        term <- term + 1
        var_name <- paste0(spc(var_level, 2), "T", sprintf("%02d", term),
                           " ", var_code, " (", var_txt, ")")
      } else{
        var_name <- paste0(var_code, " (", var_txt, ")")
      }
    }

    row_labels <- c(row_labels, var_name)
    tbl_values <- rbind(tbl_values, c(value, valuesh))

  }

  # Create table
  tbl <- data.frame("VA_components" = row_labels,
                    "USD_MM" = tbl_values[,1],
                    "Percent"= tbl_values[,2])
  # Now we round to 2 decimals
  tbl_rnd <- tbl
  tbl_rnd[, 2:3] <- round(tbl_rnd[, 2:3], 2)
  # Capture table
  tbl_txt <- utils::capture.output(print(tbl_rnd, row.names = FALSE,
                                         quote = FALSE, right = FALSE))
  tbl_txt <- paste0(tbl_txt, collapse = cr)


  # *********************
  # HEADER: EXPORTER----
  # *********************

  # Text for exporter (in title)
  if (all(is.null(exvadec$exporter), exporter == "WLD")) {
    txt_exporter <- "All countries"
  } else if(!is.null(exvadec$exporter)) {
    txt_exporter <- dbgeo[dbgeo$id == exvadec$exporter, ][["txt_en"]]
  } else {
    txt_exporter <- dbgeo[dbgeo$id == exporter, ][["txt_en"]]
  }

  # If not found, just use the variable exporter (ej. C01 in custom)
  if (length(txt_exporter) == 0) {txt_exporter <- exporter}

  # *******************
  # HEADER: SECTOR----
  # *******************

  # Text for sector
  if (sector == "TOTAL") {
    txt_sector <- "All sectors"
  } else {
    txt_sector <- paste0(dbsec[dbsec$id == sector, ][["txt_short_en"]],
                         " (", sector, ")")
  }

  # Special case, if Borin and Mancini (2019) or Miroudot and Ye (2021)
  # If there is an element called "sector" in exvadec then
  # it is a BM/MY decomposition
  if (exists("sector", exvadec)) {
    # If sector is not TOTAL, use sector code as sector
    if (!exvadec$sector == "TOTAL") {
      txt_sector <-
        paste0(dbsec[dbsec$id == exvadec$sector, ][["txt_short_en"]],
               " (", exvadec$sector, ")")
    }
  }

  # ******************
  # HEADER: IMPORTER----
  # *****************

  # Text for importer
  if (importer == "WLD") {
    txt_importer <- "All countries"
  } else {
    txt_importer <- paste0(dbgeo[dbgeo$id == importer, ][["txt_en"]],
                           " (", importer, ")")
  }

  # Special case, if Borin and Mancini (2019) or Miroudot and Ye (2021)
  # If there is an element called "partner" in exvadec then
  # it is a BM/MY decomposition
  if (exists("partner", exvadec)) {
    # If partner is not WLD, use partner code as importer
    if (!exvadec$partner == "WLD") {
      txt_importer <-
        paste0(dbgeo[dbgeo$id == exvadec$partner, ][["txt_en"]],
               " (", exvadec$partner, ")")
    }
  }



  # **********************
  # FOOTNOTE: METHOD ----
  # **********************

  # Text for decomposition method

  method_txt <-
    dbmet[dbmet$id == exvadec$method &
            dbmet$output == exvadec$output, ][["txt_en"]]


  # ***********************************
  # FOOTNOTE: PERSPECTIVE AND APPROACH
  # ***********************************

  # Text for perspective and approach

  # Perspective

  if (exvadec$method %in% c("bm_src", "my")) {
    # Default
    persp_txt <- "Exporting country perspective"
    if (all(exvadec$method == "my", exvadec$perim == "WLD")) {
      persp_txt <- "World perspective"
    }
    # If exists partner or sector
    if (all(exists("partner", exvadec), !exists("sector", exvadec))) {
      persp_txt <- paste0("Bilateral perspective (",
                          exvadec$partner, ")")
    } else if (all(!exists("partner", exvadec), exists("sector", exvadec))) {
      persp_txt <- paste0("Sector perspective (",
                          exvadec$sector, ")")
    } else if (all(exists("partner", exvadec), exists("sector", exvadec))) {
      persp_txt <- paste0("Bilateral-sector perspective (",
                          exvadec$partner, "-",
                          exvadec$sector, ")")
    }
  } else if (exvadec$method %in% c("kww", "wwz")) {
    persp_txt <- "Mix of country and world perspective"
    # Else (oecd, bm_sink)
  } else {
    persp_txt <- "Country perspective"
  }

  # Approach
  if (exvadec$method %in% c("bm_src", "my", "oecd")) {
    appr_txt <- "source approach"
  } else if (exvadec$method %in% c("bm_snk", "kww")) {
    appr_txt <- "sink approach"
  } else if (exvadec$method %in% c("wwz")) {
    appr_txt <- "mix of source and sink approach"
  }

  # *********************
  # FOOTNOTE: PRINT----
  # *********************

  # Start printing

  # Start header
  txt <- paste0(hline(), cr)

  title <- center(paste0("DECOMPOSITION OF VALUE ADDED IN EXPORTS OF ",
                  toupper(txt_exporter), " IN ", exvadec$year))

  txt <- paste0(txt, title, cr)
  txt <- paste0(txt, spc(15), "Sector: ", txt_sector, cr)
  txt <- paste0(txt, spc(15), "Destination: ", txt_importer, cr)
  txt <- paste0(txt, hline(), cr)
  # End header

  # Start table
  txt <- paste0(txt, tbl_txt, cr)
  txt <- paste0(txt, hline(), cr)
  # End table

  # Start footnote
  txt <- paste0(txt, "Method: ", method_txt, cr)
  txt <- paste0(txt, persp_txt, ", ", appr_txt, cr)
  # End footnote

  # End of text

  # Print text to console
  # cli::cli_alert_success(txt) incompatible with Sweave
  # so we use cat
  cat(txt)

  # Return tbl (without double printing to console)
  # and with full decimals
  return(invisible(tbl))

}


