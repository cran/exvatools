#' Get data from different exvatools objects
#'
#' Extracts exporting country and sector and destination data from a specific
#' variable in an `exvatools` object.
#' @param exvatools_object An `exvatools` object (`wio`,
#'   `exvadec` or `exvadir`). If it is an ICIO `wio`, it will
#'   be previously melded (i.e., China and Mexico will be grouped).
#' @param var String for the selected variable included in the `exvatools`
#'    object: `"VA"`, `"X"`, `"EXGR"`, `"VAX"`,
#'    `"DC"`, `"DVA"`, etc.
#' @param exporter String vector with codes of the exporting countries.\
#'   If the `exvadec` object includes only one country or country group,
#'     `exporter` is not required (data can only be extracted
#'     for that country).\
#'   If `exporter` is not specified and it is an `exvadir` object,
#'     the exporter will be considered the world (`"WLD"`), as by
#'     definition exporters in `exvadir` objects are the countries of
#'     origin of value added. \
#'   To include a vector with several exporters (e.g., `c("ESP", "FRA")`)
#'     the `exvadec` object must have been created with the option
#'     `exporter = "all"` in the command [make_exvadec()].
#'     `get_data()` will then produce matrices horizontally bound.
#' @param sector A character vector with sector codes, e.g. `TOTAL`,
#'   `AGF`, `MANUF`, `c("TOTAL", "AGF", "MANUF", "SERVS")`.
#'   Available codes can be checked with [info_sec()].
#' @param demand_comp A character vector of demand components, e.g.,
#'   `"HFCE"`, `c("HFCE", "GCFC")`. Only valid for `wio`
#'   objects.
#' @param importer String vector with importing country or country group codes,
#'     e.g. `"WLD"`, `"ESP"`, `"EU27"`, `c("WLD", "EU27",
#'     "NONEU27")`. Available codes can be checked with
#'     [info_geo()].\
#'   Please note that country groups will not show the strict values of
#'     `"DVA"`, `"VAX"` etc. but an average value of the countries
#'     included in that group. To obtain the specific `"DVA"`,
#'     `"VAX"`, etc. for a group, an `exvadec` object must be
#'     specifically created for that country group.\
#'   Of course, variables that do not require to exclude double-counting,
#'     like `"EXGR"`, `"DC"` or`"FC"` will be the same in
#'     both cases, so no specific `exvadec` object will be required.
#' @param custom Boolean specifying whether custom-made groups of countries
#'   or sectors are present in the environment to be used. For instance, a
#'   custom `HITECH` custom variable including high-tech sectors or
#'   a `LDC` variable with list of least-developed countries. Note that
#'   custom variables should be referred to as strings in `get_data()`,
#'   i.e. as `"HITECH"` and `"LDC"`.
#' @return A two-dimensional matrix with sector and geographical data of a
#'   variable.
#' @export
#' @examples
#' wio <- make_wio("wiodtest")
#' get_data(wio, "EXGR", exp = "ESP", sec = "MANUF")
#' get_data(wio, "EXGR", exp = "ESP", sec = c("TOTAL", "MANUF", "SRVWC"),
#'          imp = c("USA", "FRA"))
get_data <- function(exvatools_object, var, exporter,
                      sector = "TOTAL", importer = "WLD",
                      demand_comp = "TOTAL", custom = FALSE){


  # Check object and type
  is_wio <- methods::is(exvatools_object, "wio")
  is_std <- methods::is(exvatools_object, "std")
  is_exvadec <- methods::is(exvatools_object, "exvadec")
  is_exvadir <- methods::is(exvatools_object, "exvadir")
  # Error if none
  is_exvatools <- any(is_wio, is_std, is_exvadec, is_exvadir)
  if (is_exvatools == FALSE) {
    stop(paste0(deparse(substitute(exvatools_object)), " is not ",
                "a valid exvatools object"))
  } else {
    exvatools_object_name <- deparse(substitute(exvatools_object))
  }

  # Check wio source (wiotype)
  if (any(is_wio, is_std)) {
    wio_type <- exvatools_object$type
  } else {
    wio_type <- exvatools_object$source
  }

  # Check icio
  is_icio <- is.icio(wio_type)


  # Names and dimensions
  g_names <- exvatools_object$names$g_names
  n_names <- exvatools_object$names$n_names
  fd_names <- exvatools_object$names$fd_names
  gn_names <- exvatools_object$names$gn_names
  gfd_names <- exvatools_object$names$gfd_names
  G <- exvatools_object$dims$G
  N <- exvatools_object$dims$N
  FD <- exvatools_object$dims$FD
  GN <- exvatools_object$dims$GN
  GFD <- exvatools_object$dims$GFD

  # Meld if wio
  if (all(is_wio, is_icio)) {
    exvatools_object <-
      lapply(exvatools_object,
             function(x) if (is.matrix(x)) {meld(x)} else {x})
  }


  # *************
  #  Missing var
  # *************
  # If no var included
  if (missing(var)) {
    if (is_exvadir) {
      # If exvadir, there is only one var, get the name
      var <- names(exvatools_object[1])
      # Get it
      VAR <- exvatools_object[[var]]
      #var is a string
      var_name <- var
    } else {
      stop("Missing variable in function arguments")
    }
  # If there is a var
  } else {
    # Check if exists
    if (exists(var, exvatools_object)) {
      # Get it
      VAR <- exvatools_object[[var]]
      #var is a string
      var_name <- var
    } else {
      cli::cli_abort(paste0("There is no variable called '{var}' ",
                            " in {exvatools_object_name}"))
    }
  }

  # ******************
  #  Missing exporter
  # ******************
  # If no exporter included, look for exporter in object
  if (missing(exporter)) {
    if (exists("exporter", exvatools_object)) {
      # If exists, check if it is exvadir
      if (is_exvadir) {
        # Default in exvadir is WLD (all origins)
        exporter <- "WLD"
      } else {
        # ESP, CHN, NAFTA, etc: take it
        exporter <- exvatools_object$exporter
      }
    } else{
      stop("Missing exporter in function arguments")
    }
  }


  # ************************
  #  Importer not available
  # ************************
  # Also show error if trying to breakdown by importer a var
  # that is nor broken down
  if(length(importer) > 1 && var_name %in% c("VA", "X")){
    stop(paste(var_name,
               "cannot be disaggregated by countries of destination"))
  }

  # Save col_names for later
  row_names <- rownames(VAR)
  col_names <- colnames(VAR)

  # ***************
  #  Replace "all"
  # ***************
  # If vectors of countries or sectors contain "all"
  # replace 'all' by codes of all individual elements
  # e.g., c("all", "WLD") would become c("AUS", "AUT", ... "ROW", "WLD")
  if ("all" %in% exporter) {
    lst <- as.list(exporter)
    exporter <- unlist(lapply(lst, function(x) if(x == "all") g_names else x))
  }
  if ("all" %in% importer) {
    lst <- as.list(importer)
    importer <- unlist(lapply(lst, function(x) if(x == "all") g_names else x))
  }
  if ("all" %in% sector) {
    lst <- as.list(sector)
    sector <- unlist(lapply(lst, function(x) if(x == "all") n_names else x))
  }
  if ("all" %in% demand_comp) {
    lst <- as.list(demand_comp)
    demand_comp <-
      unlist(lapply(lst, function(x) if(x == "all") fd_names else x))
  }


  # *********
  # EXPORTER
  # *********

  # We initialize a matrix that will collect data for all exporters
  # in the vector exporter
  tmp <- NULL
  # Loop through elements of exporter
  for (i in seq_along(exporter)){
    # If is custom group and exists in environment
    if (all(exists(exporter[i]), custom == TRUE)) {
      # Use custom codes
      exp_codes <- paste(get(exporter[i]), collapse = "|")
    # If it is not a custom group
    } else {
      # If the object includes a variable exporter
      # do not look further. And if it is a group, it should
      # not be disaggregated (e.g. NAFTA will be NAFTA_01, etc.)
      # Typical case: exvadec for one country
      # Exception: exvadir, which always has a exporter
      if (exists("exporter", exvatools_object)) {
        # If it is exvadir and there is an exporter, we still need the codes
        # because exvadir's rows include all countries as origin
        # of value added for the real exporter. So, if for instance
        # we have NAFTA as a exporter, we can select EU27 as 'exporter' for
        # get_data, as we are really selecting the EU27 origin of VA for
        # the ultimate exporter NAFTA
        if (is_exvadir) {
          # This added 17/01
          # If is a custom wio and exporter is "WLD", we need to calculate it
          # as the sum of all countries (we have no database for that)
          if (wio_type == "custom" && exporter[i] == "WLD") {
            exp_codes <- paste0(exvatools_object$names$g_names, collapse = "|")
            # If not, use get_geo_codes (which will also return the same value
            # if wio_type is "custom")
          } else {
            exp_codes <- get_geo_codes(exporter[i], wio_type)
          }
        } else {
          # If it is a country-exvadec object, just take the name
          # (if is NAFTA, rows will be NAFTA_01T02, etc)
          exp_codes <- exvatools_object$exporter
        }
      # If the object does not include a variable exporter
      # we can only have a full exvadec or a wio
      } else {
        # If is a custom wio and exporter is "WLD", we need to calculate it
        # as the sum of all countries (we have no database for that)
        if (wio_type == "custom" && exporter[i] == "WLD") {
          exp_codes <- paste0(exvatools_object$names$g_names, collapse = "|")
        # If not, use get_geo_codes (which will also return the same value
        # if wio_type is "custom")
        } else {
          exp_codes <- get_geo_codes(exporter[i], wio_type)
        }
      }
    }
    # Now that we know exactly what exporter stands for, we can get the
    # corresponding rows (always grepping from row_names, as country-exvadir
    # has only n rows)
    pgn_exp <- grep(exp_codes, row_names)
    # Do not forget drop = FALSE
    tmp2 <- VAR[pgn_exp, , drop = FALSE]
    # If it is group e.g. EU27, the
    # selected rows would be more than N.
    # If this is the case, we sum every Nth row
    if(length(pgn_exp) > N){
      tmp2 <- as.matrix(sum_every_nth_row(tmp2, N))
    }
    # Add horizontally for each exporter
    tmp <- rbind(tmp, tmp2)
  }

  # Assign result to variable VAR and name rows and columns
  VAR <- tmp
  rownames(VAR) <- paste0(rep(exporter, each = N),
                          gsub("[CD]", "_", n_names))
  colnames(VAR) <- col_names


  # *********
  # SECTOR
  # *********

  # We first create a temporary variable to accumulate all exporters
  tmp <- NULL
  # We run every exporter and all sectors for every exporter
  # First all exporters
  for(i in seq_along(exporter)){
    # Get all sectors for country i (can be just 1)
    m <- (i - 1) * N + 1
    n <- (i - 1) * N + N
    # tmp_i is a matrix with all sectors for exporter i
    # Don't forget drop = FALSE
    tmp_i <- VAR[m:n, , drop = FALSE]
    # Then get selected sectors for that country
    for(s in seq_along(sector)){
      # Get sector codes
      # If it is a custom sector, use it
      if (all(exists(sector[s]), custom == TRUE)){
        sec_codes <- paste(get(sector[s]), collapse = "|")
      } else {
        # If sector is "TOTAL" in a custom wio we need to calculate it
        # (as there is no database). We take all the names of n_names
        if (wio_type == "custom" && sector[s] == "TOTAL") {
          sec_codes <- paste(exvatools_object$names$n_names, collapse = "|")
        # If not, use get_sec_codes (which will also return the same value
        # if wio_type is "custom")
        } else {
          sec_codes <- get_sec_codes(sector[s], wio_type)
        }
      }
      # Now get the sector row(s) (do not forget the drop = FALSE, as
      # it may be just one sector)
      tmp_is <- tmp_i[grep(sec_codes, n_names), , drop = FALSE]
      # We sum all the rows belnging to the sector (in case it is a
      # sector group, or TOTAL)
      tmp_is <- csums(tmp_is, sector[s])

      # Now we can add the sector row to the rows of each exporter matrix
      tmp <- rbind(tmp, tmp_is)
    }
  }
  # We assign the result to VAR and we name rows and colums
  VAR <- tmp
  # Each block for each exporter or group will have a number of rows
  # equal to the length of the argument sector
  rownames(VAR) <- paste0(rep(exporter, each = length(sector)), "_", sector)
  # It might be AUS_MANUF, but it can also be AUS_D01T02, so we try to
  # remove the initial letter D or C. Because it could also be AUS_CHM, we
  # will only remove if after initial C or D there is a number.
  rownames(VAR) <- gsub("(_)([CD]{1})([0-9])", "_\\3", rownames(VAR))
  colnames(VAR) <- col_names


  # ****************
  # DEMAND COMPONENT
  # ****************

  # If the selected variable is the matrix of final demand with components
  # we have columns as AUS_HFCF, AUS_GGCF, etc
  if (var == "Yfd"){
    row_names <- rownames(VAR)
    # We initialize a column
    tmp <- NULL
    # We loop the countries in columns
    for (i in seq_along(g_names)){
      p <- (i-1) * FD + 1
      q <- (i-1) * FD + FD
      # tmp_i is a matrix with all demand components of importer i
      tmp_i <- as.matrix(VAR[ , p:q, drop = FALSE])
      for(d in seq_along(demand_comp)){
        # If it is a custom demand component
        if (all(exists(demand_comp[d]), custom == TRUE)){
          # Use custom codes
          dem_codes <- paste(get(demand_comp[d]), collapse = "|")
          # If not, just use the demand component
        } else {
          # If it is "TOTAL", we do not have database for demand components,
          # so we create the code
          if (demand_comp[d] == "TOTAL"){
            dem_codes <- paste0(fd_names, collapse = "|")
          } else {
            dem_codes <- demand_comp[d]
          }
        }
        # We select the demand component(s)
        tmp_id <- tmp_i[, grep(dem_codes, fd_names), drop = FALSE]
        # And we sum them
        tmp_id <- rsums(tmp_id, demand_comp[d])
        # We add the column to the importer
        tmp <- cbind(tmp, tmp_id)
      }
    }
    VAR <- tmp
    # Row names have not varied
    rownames(VAR) <- row_names
    # Colnames yes, each country/group has as many columns as the
    # number of elements of the vector demand_comp
    colnames(VAR) <- paste0(rep(g_names, each = length(demand_comp)),
                            "_", demand_comp)
  }


  # *********
  # IMPORTER
  # *********

  # *******************************************
  # Importer with demand components (Yfd only)
  # *******************************************
  # If the selected variable is the matrix of final demand with components
  # and we want to select country
  # If we want a specific demand components for a country (demand_comp
  # is not "TOTAL"), e.g. HFCF for USA
  if (all(var == "Yfd", !demand_comp == "TOTAL")){
    row_names <- rownames(VAR)
    # Initialize
    tmp <- NULL
    for (i in seq_along(importer)){
      # If it is a custom code
      if (all(exists(importer[i]), custom == TRUE)){
        # Use custom codes
        imp_codes <- paste(get(importer[i]), collapse = "|")
      # If not, look for the importer
      } else{
        # If is a custom wio and importer is "WLD", we need to calculate it
        # as the sum of all countries (we have no database for that)
        if (wio_type == "custom" && importer[i] == "WLD") {
          imp_codes <- paste0(exvatools_object$names$g_names, collapse = "|")
          # If not, use get_geo_codes (which will also return the same value
          # if wio_type is "custom")
        } else {
          imp_codes <- get_geo_codes(importer[i], wio_type)
        }
      }
      # Get available components for each importer
      tmp_i <- VAR[, grep(imp_codes, colnames(VAR)), drop = FALSE]
      # Now we sum every Nth column (N being the demand component)
      tmp_i <- sum_every_nth_col(tmp_i, length(demand_comp))
      # And we add names
      rownames(tmp_i) <- row_names
      colnames(tmp_i) <- paste0(importer[i], "_", demand_comp)
      tmp <- cbind(tmp, tmp_i)
    }
    VAR <- tmp
    rownames(VAR) <- row_names
    colnames(VAR) <- paste0(rep(importer, each = length(demand_comp)),
                            "_", demand_comp)

  # **********************************
  # Importer without demand components
  # **********************************
  # If we are not selecting a Yfd var, we will have G columns (or just 1
  # in the case of X or VA in wio
  } else {
    # Take the row names
    row_names <- rownames(VAR)
    if (all(ncol(VAR) == 1, !importer == "WLD")) {
      # stop(paste("The variable", var, "has no breakdown by importer"))
      cli::cli_abort("The variable {var} has no breakdown by importer")
    }
    if (ncol(VAR) > 1) {
      # We initialize
      tmp <- NULL
      for(i in seq_along(importer)){
        # If it is a custom code
        if (all(exists(importer[i]), custom == TRUE)){
          # Use custom codes from environment
          imp_codes <- paste(get(importer[i]), collapse = "|")
          # If not, get geo codes
        } else{
          # In custom wios there is no "WLD", so we create it
          if (wio_type == "custom" && importer[i] == "WLD") {
            imp_codes <- paste0(g_names, collapse = "|")
          } else {
            imp_codes <- get_geo_codes(importer[i], wio_type)
          }
        }
        tmp_i <- VAR[, grep(imp_codes, colnames(VAR)), drop = FALSE]
        tmp_i <- rsums(tmp_i, importer[i])
        # And we add
        tmp <- cbind(tmp, tmp_i)
      }
      VAR <- tmp
      rownames(VAR) <- row_names
      # The columns will be the importer or vector of importers
      colnames(VAR) <- importer
    }
  }

  return(VAR)

}
