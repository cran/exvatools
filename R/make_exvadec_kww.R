#' Make Koopman et al. (2014) decomposition
#'
#' Calculates the decomposition of value added in exports of a country
#' or a group of countries according to the methodology of Koopman
#' et al (2013)
#'
#' @param wio_object An object of class `wio` (standardized world
#' input-output table) obtained using [make_wio()].
#' @param exporter A string character with the code of a country
#' (e.g., `USA`) or a country group (e.g., `EU27`,
#' `NAFTA`, etc.). The default `all` produces the export VA
#' decomposition for all individual countries.
#' @param output String character specifying the type of matrices
#'   in output:
#'   `standard`: `DCX`, `DVA`, `VAX`...
#'   `terms`: The 9 terms as per Koopman et al. (2014, pag. 481, eq. 36).
#' @keywords internal
#' @noRd
#' @return A list object of class `exvadec` with several matrices
#' plus metadata.
make_exvadec_kww <- function(wio_object, exporter = "all",
                             output = "standard") {

  # Check class----
  wio <- check_object(wio_object, "wio")

  is_all <- ifelse(exporter == "all", TRUE, FALSE)
  is_exporter <- !is_all

  # We do it with exvadec, as we want quiet output
  exvadec <- make_exvadec(wio_object = wio,
                          method = "wwz",
                          exporter = exporter,
                          output = "terms",
                          quiet = TRUE)

  EXGR <- exvadec[[1]]
  VAX1 <- exvadec[[2]]
  VAX2 <- exvadec[[3]] + exvadec[[4]]
  VAX3 <- exvadec[[5]] + exvadec[[6]]
  REF1 <- exvadec[[7]] + exvadec[[8]]
  REF2 <- exvadec[[9]]
  DDC <- exvadec[[10]] + exvadec[[11]]
  FVA1 <- exvadec[[12]] + exvadec[[13]]
  FVA2 <- exvadec[[14]] + exvadec[[15]]
  FDC <- exvadec[[16]] + exvadec[[17]]
  # FVA1 <- exvadec[[12]] + exvadec[[15]]
  # FVA2 <- exvadec[[13]] + exvadec[[16]]
  # FDC <- exvadec[[14]] + exvadec[[17]]
  VAX <- VAX1 + VAX2 + VAX3
  REF <- REF1 + REF2
  DVA <- VAX + REF
  DC <- DVA + DDC
  FVA <- FVA1 + FVA2
  FC <- FVA + FDC


  if (output == "standard") {

    exvadec <- list(EXGR, DC, DVA, VAX, REF, DDC,
                    FC, FVA, FDC)

    names(exvadec) <- c("EXGR", "DC", "DVA", "VAX", "REF", "DDC",
                        "FC", "FVA", "FDC")

  } else if (output == "terms") {

    exvadec <- list(EXGR, VAX1, VAX2, VAX3,
                    REF1, REF2, DDC,
                    FVA1, FVA2, FDC)

    names(exvadec) <- c("EXGR", "VAX1", "VAX2", "VAX3",
                        "REF1", "REF2", "DDC",
                        "FVA1", "FVA2", "FDC")

  }

  # Sum by rows, as there is no importer in KWW
  exvadec <- lapply(exvadec, "rsums", "WLD")

  # Names and dimensions of exvadec object
  exvadec$dims <- wio$dims
  exvadec$names <- wio$names
  exvadec$method <- "kww"
  exvadec$output <- output
  exvadec$source <- wio$type
  exvadec$year <- wio$year
  if (!exporter == "all") {
    exvadec$exporter <- exporter
  }
  class(exvadec) <- "exvadec"

  # Print result summary
  if (is_all) {
    get_exvadec_bkdown(exvadec)
  } else{
    get_exvadec_bkdown(exvadec, exporter)
  }

  return(exvadec)

}

