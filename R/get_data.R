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
#' @param imp_sector A character vector with sector codes, e.g. `TOTAL`,
#'   `AGF`, `MANUF`, `c("TOTAL", "AGF", "MANUF", "SERVS")`.
#'   Available codes can be checked with [info_sec()].
#' @param as_num Boolean specifying whether to present the data with
#'   names of rows and columns or just as a numeric vector or matrix.
#'   This option is useful when creating data frames composed of several
#'   `get_data()` commands, as in that case dimensions need to be controlled.
#' @return A two-dimensional matrix with sector and geographical data of a
#'   variable.
#' @export
#' @examples
#' wio <- make_wio("wiodtest")
#' get_data(wio, "EXGR", exporter = "ESP", sector = "MANUF")
#' get_data(wio, "EXGR", exporter = "ESP", sector = c("TOTAL", "MANUF", "SRVWC"),
#'          importer = c("USA", "FRA"))
get_data <- function(exvatools_object, var, exporter,
                      sector = "TOTAL", importer = "WLD", imp_sector = "TOTAL",
                      demand_comp = "TOTAL", as_num = FALSE){


  # Check object and type----
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

  # Check wio source (wiotype)----
  if (any(is_wio, is_std)) {
    wio_type <- exvatools_object$type
  } else {
    wio_type <- exvatools_object$source
  }

  # Check icio----
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

  # *****************************
  # Function to name sectors----
  # *****************************

  str_geo_sec <- function(geo, sec) {
    # If the sector code belongs to a named vector, such as
    # mysecs <- c(PRIMARY = "D01T04|D05T06|D8T14")
    # then use the names.
    #
    # Note that this is valid both for a single code sector[i]
    # or a full vector sector
    # If sector  <- c(a = "D1", b = "D2")
    # names(sector) gives c("a", "b") and names(sector[1]) gives "a"
    #
    # Be careful, if might be a vector with only some names, such as
    # c("C1", mysector = "C2", "C3"). In this case, names() will
    # not be NULL, but some elements will be == ""
    #
    # If names are null or any of the names is ""
    if (any(is.null(names(sec)), any(names(sec) == ""))) {
      # We then make sure to eliminate the
      # C or D at the beginning of the sector. Make sure, using regexp,
      # that only C or D followed by numbers are erased (and not the C
      # in CHEM). Use gsub, not sub, as it can be a vector
      sec_name <- gsub("^[CD](?=\\d)", "", sec, perl = TRUE)
      # If all names in sec are not null (might be just one)
    } else {
      sec_name <- names(sec)
    }
    # Form the string
    res <- paste0(geo, "_", sec_name)
    return(res)
  }

  # *****************************
  #  Function to name countries
  # *****************************
  replace_geo_names <- function(string_vector, geo_named_codes) {
    # Filter out unnamed entries
    named_geo <- geo_named_codes[nzchar(names(geo_named_codes))]

    replace_prefix <- function(name, geo_named_codes) {
      split <- strsplit(name, "_", fixed = TRUE)[[1]]
      prefix <- split[1]
      suffix <- split[2]

      matched <- names(geo_named_codes)[geo_named_codes == prefix]

      if (length(matched) == 1 && nzchar(matched)) {
        return(paste(matched, suffix, sep = "_"))
      } else {
        return(name)
      }
    }

    vapply(string_vector, replace_prefix, character(1),
           geo_named_codes = named_geo)
  }

  # **********************************
  #  Variable extraction in VAR----
  # *********************************
  # If no var included
  if (missing(var)) {
    if (is_exvadir) {
      # If exvadir, there is only one var, get the name
      var <- names(exvatools_object[1])
      # Get it
      VAR <- exvatools_object[[var]]
    } else {
      stop("Missing variable in function arguments")
    }
    # If there is a var
  } else {
    # Check if exists and get it
    if (exists(var, exvatools_object)) {
      VAR <- exvatools_object[[var]]
    } else {
      cli::cli_abort(paste0("There is no variable called '{var}' ",
                            " in {exvatools_object_name}"))
    }
  }

  # Meld if wio and icio
  # We could add the option premeld = TRUE/FALSE,
  # but what is the point of simplifying without first melding?
  if (all(is_wio, is_icio)) {
    VAR <- meld(VAR)
  }

  # ******************
  #  Missing exporter----
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
  #  Importer not available----
  # ************************
  # Also show error if trying to breakdown by importer a var
  # that is nor broken down (e.g. VA, X)
  if(all(length(importer) > 1, ncol(VAR) == 1)){
    stop(paste(var,
               "cannot be disaggregated by countries of destination"))
  }

  # Save col_names for later
  row_names <- rownames(VAR)
  col_names <- colnames(VAR)

  # Function to get geo exporter codes
  # We took it out to make the code clearer
  get_exp_codes <- function(geo_id, exvatools_object) {
    # wio_type <- exvatools_object$type (not needed, and wrong in exvadir)
    # If the object includes a variable exporter
    # do not look further. And if it is a group, it should
    # not be disaggregated (e.g. NAFTA will be NAFTA_01, etc.)
    # Typical case: exvadec for one country
    # Exception: exvadir, which always has a exporter
    if (exists("exporter", exvatools_object)) {
      # If it is exvadir and there is an exporter, we still need the codes
      # because exvadir's rows include ALL countries as origin
      # of value added for the real exporter. So, if for instance
      # we have NAFTA as a exporter, we can select EU27 as 'exporter' for
      # get_data, as we are really selecting the EU27 origin of VA for
      # the ultimate exporter NAFTA
      if (is_exvadir) {
        # This added 17/01
        # If is a custom wio and exporter is "WLD", we need to calculate it
        # as the sum of all countries (we have no database for that)
        if (all(wio_type == "custom", geo_id == "WLD")) {
          exp_codes <- paste0(exvatools_object$names$g_names, collapse = "|")
          # If not, use get_geo_codes (which will also return the same value
          # if wio_type is "custom")
        } else {
          exp_codes <- get_geo_codes(geo_id, wio_type)
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
      if (wio_type == "custom" && geo_id == "WLD") {
        exp_codes <- paste0(exvatools_object$names$g_names, collapse = "|")
        # If not, use get_geo_codes (which will also return the same value
        # if wio_type is "custom")
      } else {
        exp_codes <- get_geo_codes(geo_id, wio_type)
      }
    }
    return(exp_codes)
  }

  # ***************
  #  Expand "all"----
  # ***************
  # If vectors of countries or sectors contain "all" we create modified
  # vectors where 'all' is replaced by codes of all individual elements
  # e.g., c("all", "WLD") would become c("AUS", "AUT", ... "ROW", "WLD")
  # We will add an x to the vector name to mark the expansion
  # Function
  expand_vector_all <- function(vector, all_components) {
    if ("all" %in% vector) {
      lst <- as.list(vector)
      vectorx <- unlist(lapply(lst, function(x) if(x == "all") g_names else x))
    } else {
      vectorx <- vector
    }
    return(vectorx)
  }
  # Vectors expanded
  exporterx <- expand_vector_all(exporter, g_names)
  importerx <- expand_vector_all(importer, g_names)
  # Ther est is not needed
  # sectorx <- expand_vector_all(sector, n_names)
  # demand_compx <- expand_vector_all(demand_comp, fd_names)
  # imp_sectorx <- expand_vector_all(imp_sector, n_names)

  # *********
  # EXPORTER----
  # *********

  # We will collect data in matrix tmp

  tmp <- NULL

  # Loop through elements of exporter
  for (s in seq_along(exporter)){
    if (exporter[s] == "all") {
      # Nothing to change: we take the full matrix
      tmp_s <- VAR
    } else if (exporter[s] == "WLD") {
      # Sum rows of all countries, leaving sectors
      tmp_s <- sumnrow(VAR, N, paste0("WLD", gsub("^[CD]", "_", n_names)))
    } else {
      # Get exp codes (function)
      exp_codes <- get_exp_codes(exporter[s], exvatools_object)
      # Now that we know exactly what exporter stands for, we can get the
      # corresponding rows (always grepping from row_names, as country-exvadir
      # has only n rows)
      pgn_exp <- grep(exp_codes, row_names)
      # Do not forget drop = FALSE
      tmp_s <- VAR[pgn_exp, , drop = FALSE]
      # If it is group e.g. EU27, the
      # selected rows would be more than N.
      # If this is the case, we sum every Nth row
      if(length(pgn_exp) > N) {
        tmp_s <- sumnrow(tmp_s, N, str_geo_sec(exporter[s], n_names))
      }
    }
    tmp <- rbind(tmp, tmp_s)
  } #End for each exporter

  # Assign the rewsult to variable VAR
  VAR <- tmp

  # ********************
  # EXPORTER SECTOR----
  # ********************

  tmp <- NULL

  # We have the selected exporters, each one with N sectors. Now we
  # group the sectors, if needed
  # We first create a temporary variable to accumulate all exporters
  # We run every exporter and all sectors for every exporter
  # Remember that exporters are already grouped, so
  # we go in blocks of N, but in exporterx
  for(s in seq_along(exporterx)){
    # Get all sector rows for country s (can be just 1)
    m <- (s - 1) * N + 1
    n <- (s - 1) * N + N
    # tmp_s is a matrix with all sectors for exporter s
    # Don't forget drop = FALSE
    tmp_s <- VAR[m:n, , drop = FALSE]
    # Then get selected sectors for that country s
    for(i in seq_along(sector)){
      # If all sectors, leave as it is
      if (sector[i] == "all") {
        tmp_s_i <- tmp_s
        #If TOTAL, just sum the rows
      } else if (sector[i] == "TOTAL") {
        tmp_s_i <- csums(tmp_s, str_geo_sec(exporterx[s], sector[i]))
      } else {
        # We need codes
        sec_codes <- get_sec_codes(sector[i], wio_type)
        # Now get the sector row(s) (do not forget the drop = FALSE, as
        # it may be just one sector)
        tmp_s_i <- tmp_s[grep(sec_codes, n_names), , drop = FALSE]
        # We sum all the rows belonging to the sector (in case it is a
        # sector group,) (remember this is done with csums)
        tmp_s_i  <- csums(tmp_s_i, str_geo_sec(exporterx[s], sector[i]))
      } # End all/total/other
      tmp <- rbind(tmp, tmp_s_i)
    } #End for each sector
  } # End for each exporterx
  # We assign the result to VAR
  VAR <- tmp

  # Each block for each exporter or group will have a number of rows
  # equal to the length of the argument sector
  # rownames(VAR) <- paste0(rep(exporter, each = length(sector)), "_", sector)
  # It might be AUS_MANUF, but it can also be AUS_D01T02, so we try to
  # remove the initial letter D or C. Because it could also be AUS_CHM, we
  # will only remove if after initial C or D there is a number.
  # rownames(VAR) <- gsub("(_)([CD]{1})([0-9])", "_\\3", rownames(VAR))
  # colnames do not change
  # colnames(VAR) <- col_names

  # *****************
  # IMPORTERS----
  # *****************

  tmp <- NULL

  # ***********************
  # IMPORTERS IN CASE Yfd----
  # **********************
  if (var == "Yfd") {
    for (r in seq_along(importer)) {
      if (importer[r] == "all") {
        tmp_r <- VAR
      } else if (importer[r] == "WLD") {
        if (FD > 1) {
          tmp_r <- sumncol(VAR, FD, paste0("WLD", "_", fd_names))
        } else {
          tmp_r <- csums(VAR, "WLD")
        }
      } else {
        # We need imp_codes
        imp_codes <- get_geo_codes(importer[r], wio_type)
        # Get importer
        pgf_importer <- grep(imp_codes, colnames(VAR))
        tmp_r <- VAR[, pgf_importer, drop = FALSE]
        if (pgf_importer > FD) {
          tmp_r <- sumncol(tmp_r, FD, paste0(importer[r], "_", fd_names))
        }
      } # End all/WLD/other
      #Add to tmp
      tmp <- cbind(tmp, tmp_r)
    } # End importer
    # Assign to VAR
    VAR <- tmp
  } # End case Yfd

  # **************************
  # IMPORTERS IN OTHER CASES----
  # **************************
  if (!var == "Yfd") {
    for (r in seq_along(importer)) {
      if (importer[r] == "all") {
        tmp_r <- VAR
      } else if (importer[r] == "WLD") {
        # Case matrices cs_cs (Z, B, A...)
        if (ncol(VAR) > G) {
          tmp_r <- sumncol(VAR, N, str_geo_sec("WLD", n_names))
          # Case matrices cs_c (EXGR, Y...)
        } else {
          tmp_r <- rsums(VAR, "WLD")
        }
        # Specific country or country groups
      } else {
        # We need imp_codes
        imp_codes <- get_geo_codes(importer[r], wio_type)
        # Get available components for each importer
        pgn_importer <- grep(imp_codes, colnames(VAR))
        tmp_r <- VAR[, pgn_importer, drop = FALSE]
        # If group
        if (all(ncol(VAR) > G, length(pgn_importer) > N)) {
          tmp_r <- sumncol(tmp_r, N, str_geo_sec(importer[r], n_names))
        } else if (all(ncol(VAR) == G, length(pgn_importer) > 1)) {
          tmp_r <- rsums(tmp_r, importer[r])
        } else {
          # Do nothing
        }
      } # End all/WLD/other
      tmp <- cbind(tmp, tmp_r)
    } # End each importer
    VAR <- tmp
  } # End case not Yfd


  # ******************
  # DEMAND COMPONENTS----
  # ******************


  tmp <- NULL
  # If the selected variable is the matrix of final demand with components
  # we have columns as AUS_HFCF, AUS_GGCF, etc

  # Only in case Yfd has more than one component
  if (all(var == "Yfd", FD > 1)){
    for (r in seq_along(importerx)) {
      p <- (r - 1) * FD + 1
      q <- (r - 1) * FD + FD
      # tmp_d is a matrix with all demand components of importer r
      tmp_r <- as.matrix(VAR[ , p:q, drop = FALSE])
      for(d in seq_along(demand_comp)){
        if (demand_comp[d] == "all") {
          tmp_r_d <- tmp_r
        } else if (demand_comp[d] == "TOTAL") {
          tmp_r_d <- rsums(tmp_r, importerx[r])
        } else {
          # Use the demand component
          dem_codes <- demand_comp[d]
          # We select the demand component(s)
          tmp_r_d <- tmp_r[, grep(dem_codes, fd_names), drop = FALSE]
          # And we sum them
          tmp_r_d <- rsums(tmp_r_d, paste0(importerx[r], "_", demand_comp[d]))
        } # End demand_comp all/TOTAL/other
        tmp <- cbind(tmp, tmp_r_d)
      } #End each demand comp
    } # End importerx
    VAR <- tmp
  } # End case var = Yfd


  # ****************
  # IMPORTER SECTOR----
  # ****************

  tmp <- NULL

  # Now all importers are grouped
  # We will consider only cases where columns include sectors
  # i.e. var columns are > G
  # CORRERRRRRGIR ncol no puede refereirse a exvatools
  if (all(!var == "Yfd", ncol(exvatools_object[[var]]) > G)) {
    for (r in seq_along(importerx)) {
      p <- (r - 1) * N + 1
      q <- (r - 1) * N + N
      # tmp_r_j collects all sectors for importer r
      tmp_r <- VAR[, p:q, drop = FALSE]
      # print(tmp_r_j)
      for (j in seq_along(imp_sector)) {
        if (imp_sector[j] == "all") {
          tmp_r_j <- tmp_r
        } else if (imp_sector[j] == "TOTAL") {
          # Sum columns
          tmp_r_j <- rsums(tmp_r, importerx[r])
        } else {
          # Get sector codes
          sec_codes <- get_sec_codes(imp_sector[j], wio_type)
          # Now get the sector columns(s) (do not forget the drop = FALSE, as
          # it may be just one sector)
          tmp_r_j <- tmp_r[, grep(sec_codes, n_names), drop = FALSE]
          # We sum all the columns belonging to the sector (in case it is a
          # sector group) (remember this is done with rsums)
          tmp_r_j <- rsums(tmp_r_j, str_geo_sec(importerx[r], imp_sector[j]))
        }
        tmp <- cbind(tmp, tmp_r_j)
      } #End imp_sector
    } # End for each importerx
    # Now we can add the sector column to the columns
    VAR <- tmp
  } # End case

  # Rename group countries----
  # Replace geo codes in rows and columns by possible names
  # For instance, if we have c("AUS", "FRA", REST = "WLDxAUS|FRA")
  # rows like WLDxAUS|FRA_MANUF would be replaced by REST_MANUF
  row_names  <- rownames(VAR)
  col_names <- colnames(VAR)
  # Exporter
  row_names <- replace_geo_names(row_names, exporter)
  # for (s in seq_along(exporter)) {
  #   if (all(length(names(exporter[s])) == 1 , nzchar(names(exporter[s])))) {
  #     row_names  <-
  #       gsub(exporter[s], names(exporter[s]), row_names, fixed = TRUE)
  #   }
  # }
  # Importer
  col_names <- replace_geo_names(col_names, importer)
  # for (r in seq_along(importer)) {
  #   if (all(length(names(importer[r])) == 1 , nzchar(names(importer[r])))) {
  #     col_names  <-
  #       gsub(importer[r], names(importer[r]), col_names, fixed = TRUE)
  #   }
  # }
  rownames(VAR) <- row_names
  colnames(VAR) <- col_names

  # Do not keep naming or dimensions
  if (as_num == TRUE) {
    VAR <- as.numeric(VAR)
  }

  return(VAR)

}
