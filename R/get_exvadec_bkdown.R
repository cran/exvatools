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

  df <- dbva[dbva$method == method & dbva$output == output, ]

  term <- 0

  # Get total exports
  EXGR <- get_data(exvadec,  var = "EXGR", exporter = exporter,
                   sector = sector, importer = importer)

  # Create a basic data frame with three columns:
  # id code, level and description
  row_labels <- NULL
  tbl_values <- NULL
  for (i in seq_along(df[["id"]])){

    var_code <- df[i, ][["id"]]
    var_level <- df[i, ][["level"]]
    var_txt <- df[i, ][["txt_en"]]

    # print(paste0("Extracting data from ", var_code))

    value <- round(get_data(exvadec, var_code, exporter = exporter,
                            sector = sector, importer = importer), 2)
    valuesh <- round(100 * value/EXGR, 2)

    # Text will depend on the type of output
    # Standard will show   XXXXXXXXXXXXXXXXX (XXX)  XXX.XX  XX.XX
    # Terms will show      T01 XXXXXXXXXXXXX (XXX)  XXX.XX  XX.XX
    # Full or TiVA show    XXXXXXXXXXXXXXXXX (XXX)  XXX.XX  XX.XX
    if (output %in% c("standard", "tiva")) {

      var_name <- paste0(spc(var_level, 2), var_txt, " (", var_code, ")")

    } else if (output %in% c("terms", "terms2")){

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

  # Text for exporter (in title)
  if (all(is.null(exvadec$exporter), exporter == "WLD")) {
    txt_exporter <- "All countries"
  } else if(!is.null(exvadec$exporter)) {
    txt_exporter <- dbgeo[dbgeo$id == exvadec$exporter, ][["txt_en"]]
  } else {
    txt_exporter <- dbgeo[dbgeo$id == exporter, ][["txt_en"]]
  }

  if (length(txt_exporter) == 0) {txt_exporter <- exporter}

  # Text for sector
  if (sector == "TOTAL") {
    txt_sector <- "All sectors"
  } else {
    txt_sector <- paste0(dbsec[dbsec$id == sector, ][["txt_short_en"]],
                         " (", sector, ")")
  }

  # Special case, if Miroudot and Ye (2021)
  # If there is an element called "sector" in exvadec then
  # it is a my decomposition
  if (exists("sector", exvadec)) {
    # If sector is not TOTAL, use partner code as importer
    if (!exvadec$sector == "TOTAL") {
      txt_sector <-
        paste0(dbsec[dbsec$id == exvadec$sector, ][["txt_short_en"]],
               " (", exvadec$sector, ")")
    }
  }

  # Text for importer
  if (importer == "WLD") {
    txt_importer <- "All countries"
  } else {
    txt_importer <- paste0(dbgeo[dbgeo$id == importer, ][["txt_en"]],
                           " (", importer, ")")
  }

  # Special case, if Miroudot and Ye (2021)
  # If there is an element called "partner" in exvadec then
  # it is a my decomposition
  if (exists("partner", exvadec)) {
    # If partner is not WLD, use partner code as importer
    if (!exvadec$partner == "WLD") {
      txt_importer <-
        paste0(dbgeo[dbgeo$id == exvadec$partner, ][["txt_en"]],
               " (", exvadec$partner, ")")
    }
  }



  # Text for data
  cr <- "\n"

  tbl <- data.frame("VA_components" = row_labels,
                    "USD_MM" = tbl_values[,1],
                    "Percent"= tbl_values[,2])
  tbl_txt <- utils::capture.output(print(tbl, row.names = FALSE,
                                         quote = FALSE, right = FALSE))
  tbl_txt <- paste0(tbl_txt, collapse = cr)

  # Text for decomposition method

  method_txt <-
    dbmet[dbmet$id == exvadec$method &
            dbmet$output == exvadec$output, ][["txt_en"]]


  # Text for perspective and approach
  # Perspective
  if (exvadec$method == "my") {
    if (!exvadec$perim == "country") {
      persp_txt <- "World perspective"
    } else {
      if (all(exvadec$partner == "WLD", exvadec$sector == "TOTAL")) {
        persp_txt <- "Country perspective"
      } else if (all(!exvadec$partner == "WLD", exvadec$sector == "TOTAL")) {
        persp_txt <- paste0("Bilateral perspective (",
                            exvadec$partner, ")")
      } else if (all(exvadec$partner == "WLD", !exvadec$sector == "TOTAL")) {
        persp_txt <- paste0("Sector perspective (",
                            exvadec$sector, ")")
      } else if (all(!exvadec$partner == "WLD", !exvadec$sector == "TOTAL")) {
        persp_txt <- paste0("Bilateral-sector perspective (",
                            exvadec$partner, "-",
                            exvadec$sector, ")")
      }
    }
  } else if (exvadec$method %in% c("kww", "wwz")) {
    persp_txt <- "Mix of country and world perspective"
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

  # Start printing

  # Use cr for \n
  cr <- "\n"

  # Start header
  # txt <- paste0("Done!", cr)
  txt <- paste0(hline(), cr)

  title <- center(paste0("DECOMPOSITION OF VALUE ADDED IN EXPORTS OF ",
                  toupper(txt_exporter), " IN ", exvadec$year))

  txt <- paste0(txt, title, cr)
  txt <- paste0(txt, spc(20), "Sector: ", txt_sector, cr)
  txt <- paste0(txt, spc(20), "Destination: ", txt_importer, cr)
  txt <- paste0(txt, hline(), cr)
  # End header

  # Table
  txt <- paste0(txt, tbl_txt, cr)
  txt <- paste0(txt, hline(), cr)

  # Table footnote
  txt <- paste0(txt, "Method: ", method_txt, cr)
  txt <- paste0(txt, persp_txt, ", ", appr_txt, cr)


  # End of text

  # Print text to console
  # cli::cli_alert_success(txt)
  cat(txt)

  # Return tbl (without printing to console)
  return(invisible(tbl))

}


