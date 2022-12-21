#' check_wio_args
#' Validates make_wio arguments
#' @param list_args Arguments of make_wio
#' @keywords internal
#' @noRd
#' @return List with validated arguments
check_wio_args <- function(list_args){

  # print("Checking arguments...")

  wiotype <- list_args$wiotype
  year <- list_args$year
  src_dir <- list_args$src_dir


  # Auxiliary functions
  # ***********************************

  is_year <- function(txt){
    # grep("([0-9]{4})", txt, value = TRUE) == txt
    ltxt <- nchar(txt)
    lyear <- attr(regexpr("([0-9]{4})", txt), "match.length")
    res <- ifelse(ltxt == lyear, TRUE, FALSE)
    return(res)
  }

  is_path <- function(txt){
    txt <- gsub("[\\]", "/", txt)
    res <- ifelse (grepl("/", txt), TRUE, FALSE)
    return(res)
  }

  exists_path <- function(txt){
    res <- ifelse (dir.exists(txt), TRUE, FALSE)
    return(res)
  }

  # **********************************************

  # Check wiotype
  if (!exists.wiotype(wiotype)){
    wiotypes <- get_wiotypes_list()
    stop(paste0("The introduced wiotype does not exist.", "\n",
                "Valid wio types are: ", "'",
                paste(wiotypes, collapse = "', '"), "'"))
  }

  # Check year
  if (!is.null(year)){
    # Is it a valid year format?
    if (is_year(year)){
      # OK, let's convert it to integer in case is character
      year <- as.integer(year)
    } else{
      # Is it maybe a path?
      if (is_path(year)){
        # The path was wrongly put in the position of the year
        # Let's see if it is a valid path
        if (exists_path(year)){
          # Then put the value of year as src_dir
          src_dir <- year
          # And year would be null
          year <- NULL
        } else{
          # Error: path does not exist
          stop("Path not valid")
        }
      } else{
        # The alleged year is not a path
        # Error: the year format is not valid
        stop("Year format not valid")
      }
    }
  } else{
    # Year is NULL, but this will be revised later
  }

  # Check source directory
  if (!is.null(src_dir)){
    # Is it a path?
    if (is_path(src_dir)){
      # Is it valid?
      if (exists_path(src_dir)){
        # OK, now let's make sure that it only
        # has backlashes for other OSs
        src_dir <- gsub("\\\\", "/", src_dir )
        # and add a final slash if needed
        if (!substr(src_dir, nchar(src_dir), nchar(src_dir)) == "/") {
          src_dir <- paste0(src_dir, "/")
        }
      } else{
        # Error: path does not exist
        stop(paste0("Path '", src_dir, "' does not exist"))
      }
    } else{
      # Is it a year?
      stop(paste0("Path '", src_dir, "' is not a valid path"))
    }
  } else{
    # Path is null, but might be the working directory
    wd <- getwd()
    wd <- gsub("\\\\", "/", wd )
    # getwd never gives a final slash
    src_dir <- paste0(wd, "/")
  }


  # Not working
  # res <- list()
  # res$wiotype <- wiotype
  # res$year <- year
  # res$src_dir <- src_dir

  # Lists guarantee that the three variables appear
  # (if not, NULL variables are eliminated
  # although anyhow wio$[var] would be also NULL)
  res <- list(wiotype, year, src_dir)
  names(res) <- c("wiotype", "year", "src_dir")

  return(res)

}


#' Check make_exvadec_bkdown() arguments
#'
#' Validates all `make_exvadec_bkdown()` arguments
#' @param list_args Arguments of `make_exvadec_bkdown()`
#' @keywords internal
#' @noRd
#' @return Boolean, `TRUE` if arguments are correct
check_exvadec_args <- function(list_args, my_args = NULL) {

  # Standard arguments are in list_args
  # Miroudot-Ye arguments are in my_args
  # (NULL if unspecified)
  method <- list_args$method
  output <- list_args$output


  # Check that method exists
  list_methods <- unique(dbmet$id)
  if (!method %in% list_methods) {
    cli::cli_abort("Decomposition method '{method}' not valid.")
  }

  # Check that output type exists
  list_outputs <- dbmet[dbmet$id == method, ][["output"]]
  if (!output %in% list_outputs) {
    cli::cli_abort(paste0("Output '{output}' not valid for ",
                          "decomposition method '{method}'."))
  }

  # Perspective world, bilateral or sector are only
  # compatible with Miroudot and Ye. If perim, partner or
  # sector have been specified as arguments with other
  # method, my_args will have length > 0 and an error will appear
  if (length(my_args) > 0) {
    # Method different from MY
    if (!method == "my") {
      cli::cli_abort(paste0("Perpectives world, bilateral or sector are ",
                            "only available for the ",
                            "decomposition method 'my'."))
    # Method MY
    } else if (method == "my") {
    # Check that MY arguments are valid
      for (arg in names(my_args)) {
        # As arguments in R admit partial matching
        # e.g. "sec" is valid for "sector", we use grepl
        # If the is not at least a similar coincidence
        if (!any(grepl(arg, c("perim", "partner", "sector")))) {
          cli::cli_abort("Argument {arg} not valid")
        }
      }
    # Check that MY terms
      if (all(exists("perim", my_args), output == "terms2")) {
        if (!my_args$perim == "WLD") {
          cli::cli_abort(paste0("Output 'terms2' is only compatible ",
                                "with world perspective (perim = 'WLD')"))
        }
      }
    # End of method MY
    }
  # If my_args is NULL, at least check that "terms" 2 is not
  # sele
  } else {
    if (all(method == "my", output == "terms2")) {
      cli::cli_abort(paste0("Output 'terms2' in 'my' requires specification ",
                            "of world perspective (perim = 'WLD')"))
    }
  }

  return(TRUE)

}


#' Check get_exvadec_bkdown() arguments
#'
#' Validates all `get_exvadec_bkdown()` arguments
#' @param list_args Arguments of `get_exvadec_bkdown()`
#' @keywords internal
#' @noRd
#' @return List with validated arguments
check_exvadec_bkdown_args <- function(list_args) {

  # Here we might have to alter the value of
  # arguments, so we need to recover all
  exvadec_object <- list_args$exvadec_object
  exporter <- list_args$exporter
  sector <- list_args$sector
  importer <- list_args$importer

  # Check it is an exvadec object
  exvadec <- check_object(exvadec_object, "exvadec")

  # If exporter is missing, use WLD for global exvadec and
  # exvadec$exporter for country

  # If it is country exvadec,
  # exporter can only be exvadec$exporter
  if (exists("exporter", exvadec)) {
    # If exporter argument is missing, the default will
    # be WLD. Even if this does not provoke error we better
    # make sure that the argument is correct.
    if (list_args$exporter == "WLD") {
      list_args$exporter <- exvadec$exporter
    }
    # The error will appear if the argument is not WLD
    # (e.g. FRA) and exvadec$exporter is ESP.
    if (!list_args$exporter == exvadec$exporter) {
      cli::cli_abort(paste0("The exvadec object contains only ",
                            "data for exporter {exvadec$exporter}"))
    }
  }

  # *******************
  # Special Miroudot-Ye
  # *******************

  # If it is an Miroudoy-Ye exvadec and there is partner
  # importer can only be the partner
  if (exists("partner", exvadec)) {
    # If importer argument is missing, the default will
    # be WLD. Even if this does not provoke error we better
    # make sure that the argument is correct.
    if (list_args$importer == "WLD") {
      list_args$importer <- exvadec$partner
    }
    # The error will appear if the importer argument is not WLD
    # (e.g. FRA) and exvadec$importer is ESP.
    if (!list_args$importer == exvadec$partner) {
      cli::cli_abort(paste0("The exvadec object contains only ",
                            "data for importer {exvadec$partner}"))
    }
  }

  # If it is an Miroudoy-Ye exvadec and there is a sector
  # sector can only be that sector
  if (exists("sector", exvadec)) {
    # If sector argument is missing, the default will
    # be TOTAL. Even if this does not provoke error we better
    # make sure that the argument is correct.
    if (list_args$sector == "TOTAL") {
      list_args$sector <- exvadec$sector
    }
    # The error will appear if the sector argument is not TOTAL
    # (e.g. SRVWC) and exvadec$sector is MANUF.
    if (!list_args$sector == exvadec$sector) {
      cli::cli_abort(paste0("The exvadec object contains only ",
                            "data for sector {exvadec$sector}"))
    }
  }

  # Lists guarantee that the three variables appear
  # (if not, NULL variables are eliminated
  # although anyhow exvadec[var] would be also NULL)
  res <- list(exvadec, exporter, sector, importer)
  names(res) <- c("exvadec_object", "exporter",
                  "sector", "importer")

  # As we might have had to alter the value of
  # some arguments, we return a list of arguments
  return(res)

}


#' check_object
#' Validates object
#' @param object Object
#' @param class_name Character string of name of class
#' @keywords internal
#' @noRd
#' @return Validated object
check_object <- function(object, class_name) {
  if (methods::is(object, class_name)) {
    # Do nothing
  } else {
    cli::cli_abort(c(paste0("Sorry, {deparse(substitute(object))} ",
                            "is not of class {class_name}. Please use"),
                     paste0("make_{class_name}() to create ","
                            a compliant {class_name} object.")))
  }
  return(object)
}

#' exists.wiotype
#' @description Checks if a wiotype is valid
#' @param wiotype A character string
#' @keywords internal
#' @noRd
#' @return Boolean (TRUE if wiotype exists)
#' @examples
#' exists.wiotype(wiotype)
exists.wiotype <- function(wiotype) {
  return(wiotype %in% get_wiotypes_list())
}


#' get_wiotypes_list
#' Produces a list of wiotypes
#' @return Character vector with names of wiotypes
#' @keywords internal
#' @noRd
get_wiotypes_list <- function() {
  return(dbtyp$id)
}


#' is.icio
#' @description Checks if a wiotype is icio extended
#'   (MEX and CHN disaggregation)
#' @param wiotype A character string
#' @keywords internal
#' @noRd
#' @return Boolean (TRUE if wiotype is icio)
is.icio <- function(wiotype) {
  return(wiotype %in% dbtyp[dbtyp$is_icio == TRUE, ][["id"]])
}

#' is.icioshort
#' @description Checks if a wiotype is icio
#' with short MEX and CHN disaggregation (2 each)
#' @param wiotype A character string
#' @keywords internal
#' @noRd
#' @return Boolean (TRUE if wiotype is icio with short )
is.icioshort <- function(wiotype) {
  return(wiotype %in% dbtyp[dbtyp$is_long == FALSE, ][["id"]])
}

#' is.iciolong
#' @description Checks if a wiotype is icio
#' with long MEX and CHN disaggregation (3 MEX, 4 CHN)
#' @param wiotype A character string
#' @keywords internal
#' @noRd
#' @return Boolean (TRUE if wiotype is icio with long disaggregation)
is.iciolong <- function(wiotype) {
  return(wiotype %in% dbtyp[dbtyp$is_long == TRUE, ][["id"]])
}

#' is.rev4
#' @description Checks if a wio type sectors are ISIC rev.4 (CXX)
#' @param wiotype A character string
#' @keywords internal
#' @noRd
#' @return Boolean (TRUE if wiotype is rev.4)
#' @examples
#' is_rev3 <- is.rev3(wio)
is.rev4 <- function(wiotype) {
  return(wiotype %in% dbtyp[dbtyp$is_isic4 == TRUE, ][["id"]])
}


#' is.rev3
#' @description Checks if a wio type sectors are ISIC rev.3 (CXX)
#' @param wiotype A character string
#' @keywords internal
#' @noRd
#' @return Boolean (TRUE if wiotype is rev.3)
#' @examples
#' is_rev3 <- is.rev3(wio)
is.rev3 <- function(wiotype) {
  return(wiotype %in% dbtyp[dbtyp$is_isic3 == TRUE, ][["id"]])
}


#' Check if source file exists
#'
#' Check if wio source file exists, and otherwise returns an
#'   informative error
#' @param src_dir String, directory
#' @param source_file String, name of the source file
#' @keywords internal
#' @noRd
#' @return String cli message
check_wio_source_file <- function(src_dir, source_file) {
  full_path <- paste0(src_dir, source_file)
  wd <- getwd()
  if (!file.exists(full_path)) {
    cli::cli_abort(c(
      "File {source_file} not found in folder",
      "{src_dir}",
      "Please verify that:",
      "*" = "You have downloaded {source_file}.",
      "*" = "It is saved in the current working directory",
      " " = "({wd})",
      " " = "or you have specified in make_wio() the path",
      " " = "to the correct folder (using the argument src_dir).",
      "*" = "You have not renamed {source_file}."
    ), call = NULL)
  }
}




