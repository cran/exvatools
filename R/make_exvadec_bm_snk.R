#' Make Borin and Mancini (2019) sink-based decomposition
#'
#'#' Calculates the decomposition of value added in exports of a country
#' or a group of countries according to the methodology of Borin and
#' Mancini (2019), using a sink-based approach.
#'
#' @param wio_object An object of class `wio` (standardized world
#' input-output table) obtained using [make_wio()].
#' @param exporter A string character with the code of a country
#' (e.g., `USA`) or a country group (e.g., `EU27`,
#' `NAFTA`, etc.). The default `all` produces the export VA
#' decomposition for all individual countries.
#' @param output String character specifying the type of matrices
#'   in output:
#'   `standard`: `DC`, `DVA`, `VAX`...
#'   `terms`: `T01`, `T02`, `T02`... (12).
#' @keywords internal
#' @noRd
#' @return A list object of class `exvadec` with several matrices
#' plus metadata.
make_exvadec_bm_snk <- function(wio_object, exporter,
                                output = "standard"){

  # Check class----
  wio <- check_object(wio_object, "wio")

  # Get dimensions----
  G <- wio$dims$G
  N <- wio$dims$N
  GX <- wio$dims$GX
  GN <- wio$dims$GN
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

  # All exporters----
  # *******************************
  # ALL EXPORTERS
  # *******************************

  if (is_all) {

    is_group <- FALSE

    Vs <- wio$W
    Bss <- wio$Bd
    Bsr <- Brs <- wio$Bm
    Lss <- Lrr <- wio$Ld
    Asr <- Arj <- wio$Am
    Yrr <- wio$Yd
    Yrj <- Ysr <- wio$Ym
    Yrs <- bkt(wio$Ym)

    cli::cli_alert_info("Initializing matrices...")
    # Initialization of matrices for iteration

    sum_Arj_sum_Bnotsjk_Ynotskl <-
      sum_Arj_sum_Bnotsjk_Ynotsklxs <-
      sum_Arj_sum_Bnotsjk_Ynotsks <-
      Arj_Bnotsjs_Es <- name(matrix(0, GN, GX), gn_names, gx_names)

    sum_Arj_sum_Bnotsjk_Ynotskr <-
      name(matrix(0, GN, G), gn_names, g_names)


    cli::cli_alert_info("Starting iteration, please wait...")

    cli::cli_progress_bar("Please wait...",
                          type = "iterator",
                          total = G)

    # Fill of matrices
    for (country in g_names) {

      cli::cli_progress_update()

      # Position
      pgn_cou <- grep(get_geo_codes(country, wio$type, TRUE), gxn_names)
      pg_cou <- grep(get_geo_codes(country, wio$type), g_names)

      # Bnots Ynots
      Bnots <- make_Bnots(wio, country)
      Ynotskl <- set_zero(wio$Y, pgn_cou, -pg_cou)

      # AmBnotsYnots
      Arj_Bnotsjk_Ynotskl <- Arj %*% (Bnots %*% Ynotskl)

      Bnotsjs_Es <- t(sweep(t(Bnots), 1, colSums(wio$E), "*"))

      # Here, we return to basic pgn_cou (from MX1|MX2 to MEX, etc)
      # as all elements are GN x GXN/GM
      # Otherwise, pgn_s would be out of boundaries
      pgn_cou <- grep(get_geo_codes(country, wio$type, TRUE), gn_names)

      # Calculating matrices
      # print("Calculating sum_Arj_sum_Bnotsjk_Ynotskl")
      sum_Arj_sum_Bnotsjk_Ynotskl[pgn_cou,] <-
        bkt(bkoffd(name(repeach(rowSums(Arj_Bnotsjk_Ynotskl), G),
                        gxn_names, g_names)))[pgn_cou, ]

      # print("Calculating sum_Arj_sum_BjkYklxs")
      sum_Arj_sum_Bnotsjk_Ynotsklxs[pgn_cou,] <-
        bkt(bkoffd(name(rowSums(Arj_Bnotsjk_Ynotskl) -
                          Arj_Bnotsjk_Ynotskl,
                        gxn_names, g_names)))[pgn_cou, ]

      # print("Calculating sum_Arj_sum_BjkYks")
      sum_Arj_sum_Bnotsjk_Ynotsks[pgn_cou,] <-
        bkt(bkoffd(Arj_Bnotsjk_Ynotskl))[pgn_cou, ]

      # print("Calculating Arj_Bnotjs_Es")
      Arj_Bnotsjs_Es[pgn_cou,] <-
        bkt(bkoffd(name(sum_by_groups_of(meld(Arj %*% Bnotsjs_Es,
                                              meld_rows = FALSE),
                                         N, bycols = TRUE),
                        gxn_names, g_names)))[pgn_cou, ]

      # print("Calculating Arj_sum_BnotsjkYnotskr")
      sum_Arj_sum_Bnotsjk_Ynotskr[pgn_cou, ] <-
        sum_every_nth_row(bkd(Arj_Bnotsjk_Ynotskl), N)
      sum_Arj_sum_Bnotsjk_Ynotskr[pgn_cou, pg_cou] <- 0

    }  # End iteration matrix fill

    cli::cli_progress_done(result = "{G} inverse matrices computed")


    # Compound matrices

    Asr_Lrr <- Asr %*% Lrr

    Vs_Bss <- diagcs(dmult(Vs, Bss))
    Vs_Bss_Asr_Lrr <- dmult(Vs_Bss, Asr) %*% Lrr
    Vt_Bts <- diagcs(dmult(wio$W, wio$Bm))
    Vt_Bts_Asr_Lrr <- dmult(Vt_Bts, Asr) %*% Lrr

    sum_Yrj <- bkt(bkoffd(name(repeach(rowSums(Yrj), G),
                               gxn_names, g_names)))
    sum_Yrjxs <- bkt(bkoffd(name(rowSums(Yrj) - Yrj, gxn_names, g_names)))

    EXGR <- wio$EXGR


  }# End all

  # Individual exporter----
  # *******************************
  # INDIVIDUAL EXPORTER
  # *******************************

  if (is_exporter) {

    is_exporter <- TRUE
    is_all <- FALSE

    # Position of exporter
    pgn_exp <- grep(get_geo_codes(exporter, wio_type, TRUE), gxn_names)
    pg_exp <- grep(get_geo_codes(exporter, wio_type), g_names)

    # Check group
    is_group <- ifelse(length(pg_exp) > 1, TRUE, FALSE)

    # Dimensions with groups (initially as G)
    GG <- G

    # Country names with groups (initially as g_names)
    gg_names <- g_names
    ggn_names <- gn_names

    # Compound names with groups EU27_01T03
    expn_names <- paste0(exporter, gsub("[CD]", "_", n_names))
    if (is_group) {
      ggn_names <- paste0(exporter, "_", gsub("[CD]", "", n_names))
    }

    # Individual matrices
    Vs <- wio$W[pgn_exp, pgn_exp, drop = FALSE]

    Asr <- set_zero(wio$Am, pgn_exp, pgn_exp)[pgn_exp, , drop = FALSE]
    Arj <- set_zero(wio$Am, pgn_exp, pgn_exp)
    Ass <- (wio$A - Arj)

    Lss <-
      if (is_group) {
        name(solve(diag(GXN) - Ass), gxn_names, gxn_names)
      } else {
        wio$Ld
      }
    Lrr <- Lss
    Lss <- Lss[pgn_exp, pgn_exp, drop = FALSE]

    Bsr <- Brs <- set_zero(wio$Bm, pgn_exp, pgn_exp)
    Bss <- (wio$B - Bsr)[pgn_exp, pgn_exp, drop = FALSE]
    Bsr <- Bsr[pgn_exp, , drop = FALSE]
    Brs <- Bts <- Brs[, pgn_exp, drop = FALSE]

    # Bnots
    cli::cli_alert_info("Solving inverse matrix Bnots...")

    Bnots <- make_Bnots(wio, exporter)

    Ysr <- set_zero(wio$Ym, pgn_exp, pg_exp)
    Yrr <- wio$Y - Ysr
    Ysr <- Ysr[pgn_exp, , drop = FALSE]
    Yrj <- set_zero(wio$Ym, pgn_exp, pg_exp)
    if (is_group) {
      Yrj <- group_cols(Yrj, pg_exp, "replace", exporter, wio_type)
      # Number of countries with group (< G)
      GG <- ncol(Yrj)
      # Name of countries with group (< g_names)
      gg_names <- colnames(Yrj)
    }

    Yrs <- Yrj # Grouped if Yrj is, too
    Yrs <- bkt(bkoffd(Yrs[, exporter, drop = FALSE]))

    sum_Yrj <- name(repmat(as.matrix(rowSums(Yrj)), GG),
                    gxn_names, gg_names)
    sum_Yrj <- bkoffdx(sum_Yrj, gg_names)
    sum_Yrj <- bkt(sum_Yrj[, exporter, drop = FALSE])

    sum_Yrjxs <- name(repmat(as.matrix(rowSums(Yrj)), GG) - Yrj,
                      gxn_names, gg_names)
    sum_Yrjxs <- bkoffdx(sum_Yrjxs, gg_names)
    sum_Yrjxs <- bkt(sum_Yrjxs[, exporter, drop = FALSE])

    Asr_Lrr <- Asr %*% Lrr #NxGN

    # Value added matrices
    Vs_Bss <- diagcs(dmult(Vs, Bss))
    Vs_Bss_Asr_Lrr <- dmult(Vs_Bss, Asr_Lrr) #NxGN
    Vt_Bts <- diagcs(sumnrow(dmult(wio$W, Bts), N, expn_names))
    Vt_Bts_Asr_Lrr <- dmult(Vt_Bts, Asr_Lrr)

    # Ynots
    Ykl <- wio$Y
    Ynotskl <- set_zero(Ykl, pgn_exp, -pg_exp)
    if(is_group){
      Ykl <- group_cols(Ykl, pg_exp, "replace", exporter, wio_type)
      Ynotskl <- set_zero(Ykl, pgn_exp, -grep(exporter, gg_names))
    }

    Arj_Bnotsjk_Ynotskl <- Arj %*% (Bnots %*% Ynotskl)

    # sum_Arj_sum_Bjk_Yklxs
    sum_Arj_sum_Bnotsjk_Ynotsklxs <-
      name(repmat(rowSums(Arj_Bnotsjk_Ynotskl), GG) - Arj_Bnotsjk_Ynotskl,
           gxn_names, gg_names)

    sum_Arj_sum_Bnotsjk_Ynotsklxs <- bkoffdx(sum_Arj_sum_Bnotsjk_Ynotsklxs,
                                             gg_names)
    sum_Arj_sum_Bnotsjk_Ynotsklxs <-
      bkt(sum_Arj_sum_Bnotsjk_Ynotsklxs[, exporter, drop = FALSE])

    # VAXIM3

    # Without grouping (we remake Ynotskr)
    Arj_Bnotsjk_Ynotskr <- Arj %*% (Bnots %*% set_zero(wio$Y, pgn_exp, -pg_exp))
    # Here bkd, not bkdx
    Arj_Bnotsjk_Ynotskr <- bkd(Arj_Bnotsjk_Ynotskr)
    sum_Arj_sum_Bnotsjk_Ynotskr <- sumnrow(Arj_Bnotsjk_Ynotskr, N, expn_names)
    sum_Arj_sum_Bnotsjk_Ynotskr[, pg_exp] <- 0
    sum_Arj_sum_Bnotsjk_Ynotsks <-
      bkt(bkoffd(Arj_Bnotsjk_Ynotskl[, c(exporter), drop = FALSE]))


    Esr <- set_zero(wio$EXGR, pgn_exp, pg_exp)
    Es <- name(diag(rowSums(Esr)), gxn_names, gxn_names)
    # Remember: B is Bnots
    # B_Es <- Bnots %*% Es   # Too slow
    Bnots_Es <- multd(Bnots, Es)
    Arj_Bnotsjs_Es <- rsums(Arj %*% Bnots_Es[, pgn_exp, drop = FALSE], exporter)
    Arj_Bnotsjs_Es <- bkt(Arj_Bnotsjs_Es)
    # Arj_Bnotsjs_Es <- bkt(name(as.matrix(rowSums(Arj %*% Bnots_Es[, pgn_exp])),
    #                            gxn_names, exporter))

    sum_Arj_sum_Bnotsjk_Ynotskl <- rsums(Arj_Bnotsjk_Ynotskl, exporter)
    sum_Arj_sum_Bnotsjk_Ynotskl <- bkt(bkoffd(sum_Arj_sum_Bnotsjk_Ynotskl))

    EXGR <- set_zero(wio$EXGR[pgn_exp, , drop = FALSE], dest = pg_exp)

  } # End exporter


  # *******************
  # CALCULATION OF TERMS
  # *******************

  # Auxiliary function: if is_icio, melds (sometimes specifying)
  # and additionally, if it is_group, consolidates and names rows
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


  # DVA terms----

  cli::cli_alert_info("Calculating DVA terms...")

  VAX1 <- check_meld_group(dmult(Vs_Bss, Ysr))

  VAX2 <- check_meld_group(Vs_Bss_Asr_Lrr %*% Yrr)

  # Terms in VAX3, VAX4, REF1 and REF2 and DDC all have dimension
  # GN x GXN or N x GXN, so Vs_Bss_Asr_Lrr must be first melded in rows and,
  # if is_exporter and is_group, summed by sectors to obtain N rows
  # Once calculated, those terms are melded by columns
  Vs_Bss_Asr_Lrr <- check_meld_group(Vs_Bss_Asr_Lrr, meld_cols = FALSE)

  VAX3 <- meld(hmult(Vs_Bss_Asr_Lrr, sum_Yrjxs))

  VAX4 <- meld(hmult(Vs_Bss_Asr_Lrr, sum_Arj_sum_Bnotsjk_Ynotsklxs))

  # Additional term for VAXIM
  VAXIM3 <- meld(hmult(meld(Vs_Bss_Asr_Lrr),
                       sum_Arj_sum_Bnotsjk_Ynotskr))

  REF1 <- meld(hmult(Vs_Bss_Asr_Lrr, Yrs))

  REF2 <- meld(hmult(Vs_Bss_Asr_Lrr, sum_Arj_sum_Bnotsjk_Ynotsks))

  DDC <- check_meld_group(hmult(Vs_Bss_Asr_Lrr, Arj_Bnotsjs_Es))

  # FVA terms----

  cli::cli_alert_info("Calculating FVA terms...")

  FVA1 <- check_meld_group(dmult(Vt_Bts, Ysr))

  FVA2 <- check_meld_group(Vt_Bts_Asr_Lrr %*% Yrr)


  Vt_Bts_Asr_Lrr <- check_meld_group(Vt_Bts_Asr_Lrr, meld_cols = FALSE)


  FVA3 <- meld(hmult(Vt_Bts_Asr_Lrr, sum_Yrj))

  FVA4 <- meld(hmult(Vt_Bts_Asr_Lrr, sum_Arj_sum_Bnotsjk_Ynotskl))

  FDC <- meld(hmult(Vt_Bts_Asr_Lrr, Arj_Bnotsjs_Es))

  EXGR <- check_meld_group(EXGR)

  VAXIM <- VAX1 + VAX2 + VAXIM3
  VAX <- VAX1 + VAX2 + VAX3 + VAX4
  REF <- REF1 + REF2
  DVA <- VAX + REF
  DC <- DVA + DDC
  FVA <- FVA1 + FVA2 + FVA3 + FVA4
  FC <- FVA + FDC

  cli::cli_alert_info("Preparing output ...")

  # Output----

  if(output=="standard"){

    exvadec <- list(EXGR, DC, DVA, VAX, VAXIM, REF, DDC,
                 FC, FVA, FDC)

    exvadec_names <- c("EXGR", "DC", "DVA", "VAX", "VAXIM", "REF", "DDC",
                    "FC", "FVA", "FDC")

  } else if(output=="terms"){

    exvadec <- list(EXGR, VAX1, VAX2, VAX3, VAX4, REF1, REF2, DDC,
                 FVA1, FVA2, FVA3, FVA4, FDC)

    exvadec_names <- c("EXGR", "VAX1", "VAX2", "VAX3", "VAX4",
                    "REF1", "REF2", "DDC",
                    "FVA1", "FVA2", "FVA3", "FVA4",
                    "FDC")
  }

  # Set names
  names(exvadec) <- exvadec_names

  # Name rows and columns
  if (is_all){
    exvadec <- lapply(exvadec, "rownames<-", gn_names)
    exvadec <- lapply(exvadec, "colnames<-", g_names)
  } else if (is_exporter) {
    exvadec <- lapply(exvadec, "rownames<-", paste0(exporter, "_",
                                              gsub("[CD]", "", n_names)))
    exvadec <- lapply(exvadec, "colnames<-", g_names)
  }

  exvadec$dims <- wio$dims
  exvadec$names <- wio$names
  exvadec$method <- "bm_snk"
  exvadec$output <- output
  exvadec$source <- wio$type
  exvadec$year <- wio$year
  if (is_exporter) {
    exvadec$exporter <- exporter
  }
  class(exvadec) <- "exvadec"

  # cli::cli_alert_success("Done!")

  # Print result summary
  if (exporter == "all") {
    get_exvadec_bkdown(exvadec)
  } else{
    get_exvadec_bkdown(exvadec, exporter)
  }

  return(exvadec)

}
