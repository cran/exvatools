#' Summary method for `wio` class
#'
#' @param object An object of class `wio`
#' @param ... Additional arguments
#' @return Printout to console
#' @export
summary.wio <- function(object, ...) {

  wio <- object

  # Internal functions
  cr <- "\n"

  # Get dimensions and names
  G <- wio$dims$G
  N <- wio$dims$N
  GX <- wio$dims$GX
  FD <- wio$dims$FD
  g_names <- wio$names$g_names
  n_names <- wio$names$n_names
  fd_names <- wio$names$fd_names
  wio_type <- wio$type
  is_icio <- is.icio(wio_type)

  # Title
  txt_wio <- toupper(dbtyp[dbtyp$id == wio_type, ][["txt_en"]])

  # Column dimensions
  dimensions <- NULL
  for (i in 1:20) {
    dimensions <- rbind(dimensions, paste(nrow(wio[[i]]), "x", ncol(wio[[i]])))
  }
  dimensions <- c(dimensions, rep("", 4))

  # Table of elements
  tbl <- data.frame(Element = dbwio$id,
                    Description = dbwio$txt_en,
                    Dimensions = dimensions)

  # tbl_txt <- utils::capture.output(print(tbl, row.names = FALSE,
  #                                        quote = FALSE, right = FALSE))
  # tbl_txt <- paste0(tbl_txt, collapse = cr)


  # Start printing
  cat(cr)
  cat(hline(), cr)
  cat(center(txt_wio), cr)
  cat(center(paste0("Data for year: ", wio$year)), cr)
  cat(hline(), cr)
  cat(cr)

  print(tbl, right = FALSE, row.names = FALSE)

  cat(cr)
  cat("Available countries, including rest of the world (G):", G, cr)
  cat(vecwrap(wio$names$g_names), cr, cr)


  # Additional disaggregated countries
  if (is_icio){
    cat("Extra disaggregated countries:", GX - G, cr)
    iciox <- get_icio_xdata(wio)
    for(i in seq_along(iciox$codes)) {
      cat(names(iciox$codes)[i], ":",
          strsplit(iciox$codes[[i]], "[|]")[[1]], cr)
    }
    cat(cr)
    cat("Total countries, including disaggregations (GX):", GX, cr)
    cat(cr)
  }

  # Sectors
  cat("Available sectors (N):", N, cr)
  cat(vecwrap(wio$names$n_names), cr, cr)

  # Demand components
  cat("Demand components (FD):", FD, cr)
  cat(wio$names$fd_names)
  cat(cr, cr)

}


#' Summary method for `exvadec` class
#'
#' @param object An object of class `exvadec`.
#' @param ... Additional arguments.
#' @return Printout to console
#' @export
summary.exvadec <- function(object, ...) {

  exvadec <- object

  # Internal functions
  cr <- "\n"

  # Get dimensions and names
  G <- exvadec$dims$G
  N <- exvadec$dims$N
  GX <- exvadec$dims$GX
  FD <- exvadec$dims$FD
  g_names <- exvadec$names$g_names
  n_names <- exvadec$names$n_names
  fd_names <- exvadec$names$fd_names
  wio_type <- exvadec$source

  # Country
  if (is.null(exvadec$exporter)) {
    country <- "all countries"
  } else {
    country <- dbgeo[dbgeo$id == exvadec$exporter, ][["txt_en"]]
  }

  decomp_met <-
    dbmet[dbmet$id == exvadec$method &
            dbmet$output == exvadec$output, ][["txt_en"]]

  # Title
  title <- paste0("DECOMPOSITION OF VALUE ADDED IN EXPORTS FOR ",
                  toupper(country), " IN ", exvadec$year)

  column <- paste0(exvadec$method, "_", exvadec$output)

  tbl <- as.data.frame(
    dbva[dbva$method == exvadec$method &
           dbva$output == exvadec$output, ][c("id", "txt_en")]
    )
  colnames(tbl) <- c("Components", "Description")

  dimensions <- NULL
  for (var in tbl$Components) {
    dimensions <- rbind(dimensions, paste(nrow(exvadec[[var]]), "x",
                                          ncol(exvadec[[var]])))
  }
  tbl$Dimensions <- as.character(dimensions)
  # If terms, correct Components to add
  if (exvadec$output %in% c("terms", "terms2")) {
    num_terms <- length(tbl$Components) - 1
    txt_terms <- as.matrix(c("", paste0("T", sprintf("%02d", c(1:num_terms)))))
    tbl$Components <- trimws(paste(txt_terms, tbl$Components))
  }

  # Start printing
  cat(cr)
  cat(hline(), cr)
  cat(center(title), cr)
  cat(hline(), cr)
  # cat(cr)

  print(tbl, right = FALSE, row.names = FALSE)

  cat(hline(), cr)
  cat(paste0("Method: ", decomp_met, cr))

  cat(cr)
  cat("Available countries, including rest of the world (G):", G, cr)
  cat(vecwrap(exvadec$names$g_names), cr, cr)

  # Sectors
  cat("Available sectors (N):", N, cr)
  cat(vecwrap(exvadec$names$n_names), cr, cr)

}


#' Summary method for `exvadir` class
#'
#' @param object An object of class `exvadir`.
#' @param ... Additional arguments.
#' @return Printout to console
#' @export
summary.exvadir <- function(object, ...) {

  exvadir <- object

  # Internal functions
  cr <- "\n"

  # Get dimensions and names
  G <- exvadir$dims$G
  N <- exvadir$dims$N
  GX <- exvadir$dims$GX
  FD <- exvadir$dims$FD
  g_names <- exvadir$names$g_names
  n_names <- exvadir$names$n_names
  fd_names <- exvadir$names$fd_names
  wio_type <- exvadir$source


  # Exporter
  country <- dbgeo[dbgeo$id == exvadir$exporter, ][["txt_en"]]
  # Title
  title <- paste0("ORIGIN AND DESTINATION OF VALUE ADDED IN EXPORTS OF ",
                  toupper(country), " IN ", exvadir$year)

  va_type <- paste0(dbdir[dbdir$id == exvadir$va_type, ][["txt_en"]],
                    " (", exvadir$va_type, ")")

  flow_type <- paste0(dbdir[dbdir$id == exvadir$flow_type, ][["txt_en"]],
                    " (", exvadir$flow_type, ")")

  # Via
  if (!exvadir$via == "any") {
    via <- dbgeo[dbgeo$id == exvadir$via, ][["txt_en"]]
  }


  # Using inputs of sector
  if (exvadir$sec_orig == "all") {
    sec_orig <- "all sectors"
  } else {
    sec_orig <- paste0(dbsec[dbsec$id == exvadir$sec_orig, ][["txt_short_en"]],
                       "(", exvadir$sec_orig, ")")
  }

  # from country
  if (exvadir$orig_geo == "all") {
    orig_geo <- "all countries"
  } else {
    orig_geo <- dbgeo[dbgeo$id == exvadir$orig_geo, ][["txt_en"]]
  }


  # Start printing
  cat(cr)
  cat(hline(), cr)
  cat(center(title), cr)
  cat(hline(), cr)
  # cat(cr)

  cat("Value added type:", va_type, cr)
  cat("In type of flow:", flow_type, cr)
  cat("That goes via country:", exvadir$via, cr)
  cat("Using inputs from sector:", sec_orig, cr)
  cat("Of country:", orig_geo, cr)
  cat("With sector perspective:", exvadir$perspective, cr)

  cat(hline(), cr)

  cat(cr)
  if (exvadir$orig_geo == "all") {
    cat("Available countries of origin of VA (G):", G, cr)
    cat(vecwrap(exvadir$names$g_names), cr, cr)
  } else {
    # code_column <- paste0("codes_", exvadir$source)
    # db <- dbgeo[dbgeo$id %in% c(exvadir$orig_geo),]
    # codes <- strsplit(db[[code_column]], "[|]")[[1]]
    codes <- strsplit(exvadir$orig_geo, "[|]")[[1]]
    cat("Available countries of origin of VA:", length(codes), cr)
    cat(vecwrap(codes), cr, cr)
  }

  # Sectors
  if (exvadir$sec_orig == "all") {
    cat("Available sectors of origin of VA (N):", N, cr)
    cat(vecwrap(exvadir$names$n_names), cr, cr)
  } else {
    # code_column <- paste0("codes_", exvadir$source)
    # db <- dbsec[dbsec$id %in% c(exvadir$sec_orig),  ]
    # codes <- strsplit(db[[code_column]], "[|]")[[1]]
    codes <- strsplit(exvadir$sec_orig, "[|]")[[1]]
    # sec_txt <- db[["txt_short_en"]]
    cat("Available sectors of origin of VA:", length(codes), cr)
    cat(vecwrap(codes), cr, cr)
  }

  # Destinations
  cat("Available destinations of VA (G):", G, cr)
  cat(vecwrap(exvadir$names$g_names), cr, cr)

}
