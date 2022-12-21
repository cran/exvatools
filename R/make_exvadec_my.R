#' Make Miroudot and Ye (2021) decomposition of VA in exports
#'
#' Calculates the decomposition of value added in exports of a country
#' or a group of countries according to the methodology of Miroudot and
#' Ye (2021), using a source-based approach.
#'
#' @param wio_object An object of class `wio` (standardized world
#'   input-output table) obtained using [make_wio()].
#' @param exporter Character string with the code of a country
#'   (e.g., `"USA"`) or a country group (e.g., `"EU27"`,
#' `NAFTA`, etc.). The default `all` produces the export VA
#' decomposition for all individual countries.
#' @param output String character specifying the type of matrices
#'   in output:
#'   `standard`: `DC`, `DVA`...
#'   `terms`: `T01`, `T02`, `T03`... (12).
#'   `full`: All the terms (in this case, equal to `DVA`...)
#' @param perim String specifying spatial perimeter: default
#'   is `"country"`, but can also be `"WLD"` (world).
#' @param partner String specifying spatial geographical perimeter (only
#'   compatible with `perim = "country"`). Default is
#'   is `"WLD"`, but can also be any country or country group code. In
#'   that case, all flows that cross the bilateral perimeter more than once
#'   will be considered double counting.
#' @param sector String specifying spatial sector perimeter (only
#'   compatible with `perim = "country"`). Default is
#'   is `"TOTAL"`, but can also be any sector or sector group code. In
#'   that case, all flows that cross the sector perimeter more than once
#'   will be considered double counting.
#' @keywords internal
#' @noRd
#' @author Enrique Feas
#' @return A list object of class `exvadec` with several matrices
#' plus metadata.
make_exvadec_my <- function(wio_object, exporter = "all",
                            output = "standard", perim = "country",
                            partner = "WLD", sector = "TOTAL") {

  # Requires functions: bkd, bkoffd, bkt, bktt, hmult,
  # meld, bkdiag, sum_every_nth_row, sum_by_groups_of
  # make_iciox, is.icio

  # exporter <- "ESP"
  # output <- "terms"
  # perim <-  "WLD"
  # partner <- "WLD"
  # sector <- "TOTAL"

  # Check class
  wio <- check_object(wio_object, "wio")

  # Get dimensions----
  G <- wio$dims$G
  N <- wio$dims$N
  GX <- wio$dims$GX
  GN <- wio$dims$GX
  GXN <- wio$dims$GXN

  # Get wio names----
  gxn_names <- wio$names$gxn_names
  gn_names <- wio$names$gn_names
  gx_names <- wio$names$gx_names
  g_names <- wio$names$g_names
  n_names <- wio$names$n_names

  # Check wio type----
  wio_type <- wio$type
  is_icio <- is.icio(wio_type)

  is_all <- ifelse(exporter == "all", TRUE, FALSE)
  is_exporter <- !is_all

  if (is_all) {
    exporter <- wio$names$g_names
  }

  make_my <- function(exporter) {
    # Initialize number and names of group countries, just in case
    # GG is the number of countries when some of them are grouped,
    # e.g. if G = 67, GG with EU27 would be 41 (40 + 27 grouped in EU27)
    GG <- G
    # Country names with groups (initially as g_names)
    gg_names <- g_names
    ggn_names <- gn_names
    expn_names <- paste0(exporter, "_", gsub("^[CD]", "", n_names))

    # Position of exporter, groups
    pgn_exp <- grep(get_geo_codes(exporter, wio_type, TRUE), gxn_names)
    pg_exp <- grep(get_geo_codes(exporter, wio_type), g_names)
    # Check group
    is_group <- ifelse(length(pg_exp) > 1, TRUE, FALSE)
    # ggn_names is group name with sectors, i.e.
    # EU27_01T02...EU27_90T98
    if (is_group) {
      ggn_names <- paste0(exporter, "_", gsub("^[CD]", "", n_names))
    }

    # DVA auxiliary matrices----
    cli::cli_alert_info("Preparing auxiliary matrices...")

    # Vs
    Vs <- wio$W[pgn_exp, pgn_exp, drop = FALSE]

    # A
    Ao <- make_Anots(wio, exporter, perim, partner, sector)
    pgn_exp <- grep(get_geo_codes(exporter, wio$type, TRUE), gxn_names)
    AI <- wio$A - Ao

    # Bo
    B <- wio$B
    Bo <- make_Bnots(wio, exporter, perim, partner, sector)
    # The Miroudot and Ye's formulation is
    # BoAIB <- Bo %*% AI %*% B
    # but we will use the equivalent
    BoAIB <- B - Bo
    # If perimeter is WLD and output is terms (or directly terms2)
    # we get extra decomposition FDC1 y FDC2
    if (all(perim == "WLD", output %in% c("terms", "terms2"))) {
      # Coudl Bo[pgn_exp, ] %*% AI %*% Bo?
      BoAIBo <- Bo %*% AI %*% Bo
      BoAIBminus_Bo <- Bo %*% AI %*% (B - Bo)
    }

    Boss <- Bo[pgn_exp, pgn_exp, drop = FALSE]
    BoAIBss <- BoAIB[pgn_exp, pgn_exp, drop = FALSE]
    Vss <- wio$W[pgn_exp, pgn_exp, drop = FALSE]
    Bss <- wio$B[pgn_exp, pgn_exp, drop = FALSE]


    cli::cli_alert_info("Calculating terms ...")

    # Auxiliary function: if is_icio, melds (sometimes specifying)
    # and then, if it is_group, consolidates and names rows
    # with ggn_names
    check_meld_group <- function(df, meld_rows = TRUE, meld_cols = TRUE) {
      if (is_icio) {
        df <- meld(df, meld_rows = meld_rows, meld_cols = meld_cols)
      }
      if (is_group) {
        col_names <- colnames(df)
        df <- name(sum_every_nth_row(df, N), ggn_names, col_names)
      }
      return(df)
    }

    Vs_Boss <- diagcs(dmult(Vs, Boss))
    Vs_BoAIBss <- diagcs(dmult(Vs, BoAIBss))

    Bots <-
      set_zero(bkoffd(Bo), pgn_exp, pgn_exp)[, pgn_exp, drop = FALSE]

    BoAIBts <-
      set_zero(bkoffd(BoAIB), pgn_exp, pgn_exp)[, pgn_exp, drop = FALSE]

    sum_Vt_Bots <- diagcs(dmult(wio$W, Bots))
    sum_Vt_BoAIBts <- diagcs(dmult(wio$W, BoAIBts))


    if (all(perim == "WLD", output %in% c("terms", "terms2"))) {

      BoAIBots <-
        set_zero(bkoffd(BoAIBo), pgn_exp, pgn_exp)[, pgn_exp, drop = FALSE]
      BoAIBminus_Bots <-
        set_zero(bkoffd(BoAIBminus_Bo),
                 pgn_exp, pgn_exp)[, pgn_exp, drop = FALSE]

      sum_Vt_BoAIBots <- diagcs(dmult(wio$W, BoAIBots))
      sum_Vt_BoAIBminus_Bots <- diagcs(dmult(wio$W, BoAIBminus_Bots))

    }

    EXGR <- wio$EXGR[pgn_exp, , drop = FALSE]
    if (is_group) {
      EXGR[, pg_exp] <- 0
    }
    # If partner is not world, make zero every exports not to the partner
    if (!partner == "WLD") {
      pg_part <- grep(get_geo_codes(partner, wio_type), g_names)
      EXGR[, -pg_part] <- 0
    }
    # If sector is not total, make zero every sector not specified
    if (!sector == "TOTAL") {
      pg_sec <- grep(get_sec_codes(sector, wio_type), n_names)
      EXGR[-pg_sec, ] <- 0
    }


    DVA <- check_meld_group(Vs_Boss %*% EXGR)
    DDC <- check_meld_group(Vs_BoAIBss %*% EXGR)
    FVA <- check_meld_group(sum_Vt_Bots %*% EXGR)
    FDC <- check_meld_group(sum_Vt_BoAIBts %*% EXGR)

    DC <- DVA + DDC
    FC <- FVA + FDC

    if (all(perim == "WLD", output %in% c("terms", "terms2"))) {
      output <- "terms2"
      FDC1 <- check_meld_group(sum_Vt_BoAIBots %*% EXGR)
      FDC2 <- check_meld_group(sum_Vt_BoAIBminus_Bots %*% EXGR)
    }

    EXGR <- check_meld_group(EXGR)


    if(output == "standard") {

      exvadec <- list(EXGR, DC, DVA, DDC,
                      FC, FVA, FDC)
      exvadec_names <- c("EXGR", "DC", "DVA", "DDC",
                         "FC", "FVA", "FDC")

    } else if (output == "terms") {

      exvadec <- list(EXGR, DVA, DDC, FVA, FDC)
      exvadec_names <- c("EXGR", "DVA", "DDC", "FVA", "FDC")

    } else if (output == "terms2") {

      exvadec <- list(EXGR, DVA, DDC, FVA, FDC1, FDC2)
      exvadec_names <- c("EXGR", "DVA", "DDC", "FVA", "FDC1", "FDC2")

    }

    names(exvadec) <- exvadec_names

    return(exvadec)

  }

  exvadec <- list()
  cli::cli_progress_bar("Please wait...",
                        type = "iterator",
                        total = G)
  for (exp in exporter) {
    if (is_all) {cli::cli_alert_info("Country {exp}")}
    exvadec_exp <- make_my(exp)
    cli::cli_progress_update()
    exvadec_names <- names(exvadec_exp)
    for (var in exvadec_names) {
      exvadec[[var]] <- rbind(exvadec[[var]], exvadec_exp[[var]])
    }
  }

  # We have to recover output, as it is not given out of the function
  if (all(perim == "WLD", output %in% c("terms", "terms2"))){
    output <- "terms2"
  }

  # Output----
  cli::cli_alert_info("Preparing output ...")


  # Metadata
  exvadec$dims <- wio$dims
  exvadec$names <- wio$names
  exvadec$method <- "my"
  exvadec$output <- output
  exvadec$source <- wio$type
  exvadec$year <- wio$year
  if (is_exporter){
    exvadec$exporter <- exporter
  }
  exvadec$perim <- perim
  exvadec$partner <- partner
  exvadec$sector <- sector

  class(exvadec) <- "exvadec"

  # cli::cli_alert_success("Done!")

  # Print result summary
  if (is_all) {
    get_exvadec_bkdown(exvadec)
  } else{
    get_exvadec_bkdown(exvadec, exporter)
  }

  return(exvadec)

}

