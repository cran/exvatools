#' Make Borin and Mancini (2019) source-based decomposition of VA in exports
#'
#'#' Calculates the decomposition of value added in exports of a country
#' or a group of countries according to the methodology of Borin and
#' Mancini (2019), using a source-based approach.
#'
#' @param wio_object An object of class `wio` (standardized world
#'   input-output table) obtained using [make_wio()].
#' @param exporter Character string with the code of a country
#'   (e.g., `"USA"`) or a country group (e.g., `"EU27"`,
#' `NAFTA`, etc.). The default `all` produces the export VA
#' decomposition for all individual countries.
#' @param output String character specifying the type of matrices
#'   in output:
#'   `standard`: `DC`, `DVA`, `VAX`...
#'   `terms`: `T01`, `T02`, `T03`... (12).
#' @keywords internal
#' @noRd
#' @return A list object of class `exvadec` with several matrices
#' plus metadata.
make_exvadec_bm_src <- function(wio_object, exporter = "all",
                                output = "standard") {

  # Requires functions: bkd, bkoffd, bkt, bktt, hmult,
  # meld, bkdiag, sumnrow, sumgcol
  # make_iciox, is.icio


  # Check class----
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

  # *********************
  # Legend
  # *********************
  # G: number of countries
  # GG: number of countries, when some are grouped
  #    e.g. if EU27, GG = G-27+1)
  # g_names: country names: "AUS", "AUT"...
  # gg_names: country names when some are grouped: "AUS"..."EU27"
  # gn_names: country-sector names: "AUS_01T02", "AUS_03T05"...,
  #  "AUT_01T02", "AUT_03T05"...
  # ggn_names: country-sector names:
  #   * for exporter "all": gn_names
  #   * for exporter individual country: "ESP_01T02", "ESP_03T05"...
  #       (normally exporter will be used)
  #   * for exporter group: "EU27_01T02", "EU27_03T05"...
  # exporter: name of exporter
  #   * for exporter individual country: "ESP"...
  #   * for exporter group: "EU27"
  # pg_exp: position of exporter(s) in list of country names
  # pgn_exp: position of exporter(s)in list of country-sector names



  # Initialize
  GG <- G
  # Country names with groups (initially as g_names)
  gg_names <- g_names
  ggn_names <- gn_names

  # Position of exporter, groups
  if (is_exporter) {
    pgn_exp <- grep(get_geo_codes(exporter, wio_type, TRUE), gxn_names)
    pg_exp <- grep(get_geo_codes(exporter, wio_type), g_names)
    # Check group
    is_group <- ifelse(length(pg_exp) > 1, TRUE, FALSE)
    if (is_group) {
      # EU27_01T02...EU27_90T98
      ggn_names <- paste0(exporter, "_", gsub("^[CD]", "", n_names))
    }
  } else {
    is_group <- FALSE
  }

  # DVA auxiliary matrices----
  cli::cli_alert_info("Preparing DVA auxiliary matrices...")

  # Vs
  Vs <- wio$W
  if (is_exporter) {
    Vs <- Vs[pgn_exp, pgn_exp, drop = FALSE]
  }

  # B
  Bjk <- wio$B
  Bss <- wio$Bd
  Bsr <- Brs <- wio$Bm
  if (is_exporter) {
    Bsr <- Brs <- set_zero(Bsr, pgn_exp, pgn_exp)
    Bss <- (wio$B - Bsr)[pgn_exp, pgn_exp, drop = FALSE]
    Bsr <- Bsr[pgn_exp, , drop = FALSE]
    Brs <- Brs[, pgn_exp, drop = FALSE]
  }

  # Ym ungrouped
  Ysr <- wio$Ym
  Yrr <- wio$Yd
  if (is_exporter){
    Ysr <- set_zero(Ysr, pgn_exp, pg_exp)
    Yrr <- wio$Y - Ysr
    Ysr <- Ysr[pgn_exp, , drop = FALSE]
  }

  # Ym grouped
  Yrj <- wio$Ym
  if (is_exporter) {
    Yrj <- set_zero(Yrj, pgn_exp, pg_exp)
    if (is_group) {
      # Replace individual countries with group (placed at the end)
      Yrj <- group_cols(Yrj, pg_exp, "replace", exporter, wio_type)
      GG <- ncol(Yrj)
      # AUS... EU27
      gg_names <- colnames(Yrj)
    }
  }

  # Y grouped
  Ykl <- wio$Y
  if (is_group) {
    Ykl <- group_cols(Ykl, pg_exp, "replace", exporter, wio_type)
  }

  # A
  Asr <- wio$Am
  if (is_exporter) {
    Asr <- set_zero(Asr, pgn_exp, pgn_exp)[pgn_exp, , drop = FALSE]
  }
  Arj <- wio$Am
  if (is_exporter) {
    Arj <- set_zero(Arj, pgn_exp, pgn_exp)
  }
  Ass <- wio$A - Arj

  # Arj_Bjk_Ykl
  Arj_Bjk_Ykl <- Arj %*% (Bjk %*% Ykl)

  # sum_Yrj
  sum_Yrj <- name(repmat(as.matrix(rowSums(Yrj)), GG),
                         gxn_names, gg_names)
  if (is_all) {
    sum_Yrj <- bkt(bkoffd(sum_Yrj))
  } else if(is_exporter) {
    sum_Yrj <- bkoffdx(sum_Yrj, gg_names)
    sum_Yrj <- bkt(sum_Yrj[, exporter, drop = FALSE])
  }

  # sum_Yrjxs
  sum_Yrjxs <- name(repmat(as.matrix(rowSums(Yrj)), GG) - Yrj,
                    gxn_names, gg_names)
  if (is_all) {
    sum_Yrjxs <- bkt(bkoffd(sum_Yrjxs))
  } else if(is_exporter) {
    sum_Yrjxs <- bkoffdx(sum_Yrjxs, gg_names)
    sum_Yrjxs <- bkt(sum_Yrjxs[, exporter, drop = FALSE])
  }

  # sum_Arj_sum_Bjk_Ykl
  sum_Arj_sum_Bjk_Ykl <- name(repmat(rowSums(Arj_Bjk_Ykl), GG),
                              gxn_names, gg_names)
  if (is_all) {
    sum_Arj_sum_Bjk_Ykl <- bkt(bkoffd(sum_Arj_sum_Bjk_Ykl))
  } else if(is_exporter) {
    sum_Arj_sum_Bjk_Ykl <- bkoffdx(sum_Arj_sum_Bjk_Ykl, gg_names)
    sum_Arj_sum_Bjk_Ykl <- bkt(sum_Arj_sum_Bjk_Ykl[, exporter, drop = FALSE])
  }

  # sum_Arj_sum_Bjk_Yklxs
  sum_Arj_sum_Bjk_Yklxs <- name(repmat(rowSums(Arj_Bjk_Ykl), GG) -
                                  Arj_Bjk_Ykl,
                                gxn_names, gg_names)
  if (is_all) {
    sum_Arj_sum_Bjk_Yklxs <- bkt(bkoffd(sum_Arj_sum_Bjk_Yklxs))
  } else if(is_exporter) {
    sum_Arj_sum_Bjk_Yklxs <- bkoffdx(sum_Arj_sum_Bjk_Yklxs, gg_names)
    sum_Arj_sum_Bjk_Yklxs <-
      bkt(sum_Arj_sum_Bjk_Yklxs[, exporter, drop = FALSE])
  }

  # sum_Arj_sum_Bjk_Yks
  sum_Arj_sum_Bjk_Yks <- Arj_Bjk_Ykl
  if (is_all) {
    sum_Arj_sum_Bjk_Yks <- bkt(bkoffd(sum_Arj_sum_Bjk_Yks))
  } else if(is_exporter) {
    sum_Arj_sum_Bjk_Yks <- bkoffdx(sum_Arj_sum_Bjk_Yks, gg_names)
    sum_Arj_sum_Bjk_Yks <-
      bkt(sum_Arj_sum_Bjk_Yks[, exporter, drop = FALSE])
  }

  # Yrs
  Yrs <- Yrj  # Also grouped
  if (is_all) {
    Yrs <- bkt(bkoffd(Yrs))
  } else if(is_exporter) {
    Yrs <- bkoffdx(Yrs, gg_names)
    Yrs <- bkt(Yrs[, exporter, drop = FALSE])
  }

  EXGR <- wio$EXGR
  if (is_exporter) {
    EXGR <- set_zero(EXGR, pgn_exp, pg_exp)[pgn_exp, , drop = FALSE]
  }

  # Lss, Lrr
  Lss <- Lrr <- wio$Ld
  if (is_exporter) {
    if (is_group) {
      Lss <- name(solve(diag(GXN) - Ass), gxn_names, gxn_names)
    }
    Lrr <- Lss
    Lss <- Lss[pgn_exp, pgn_exp, drop = FALSE]
  }

  # FVA auxiliary matrices----
  cli::cli_alert_info("Preparing FVA auxiliary matrices...")

  if (is_all){
    Bnots <- make_global_Bnots(wio)
    Btsnots <- bkoffd(Bnots)
    sum_Vt_Btsnots <- diagcs(dmult(wio$W, Btsnots))
    sum_Vt_Bts_minus_Btsnots <- diagcs(dmult(wio$W, (wio$Bm - Btsnots)))
  } else if (is_exporter) {
    Bnots <- make_Bnots(wio, exporter)
    # Btsnots <- bkoffd(Bnots)[, pgn_exp, drop = FALSE]
    Btsnots <- set_zero(Bnots, pgn_exp, pgn_exp)
    Btsnots <- Btsnots[, pgn_exp, drop = FALSE]
    sum_Vt_Btsnots <- diagcs(sumnrow(dmult(wio$W, Btsnots), N))
    sum_Vt_Bts_minus_Btsnots <- diagcs(sumnrow(dmult(wio$W, Brs - Btsnots), N))
  }

  # Vs_Lss
  Vs_Lss <- diagcs(dmult(Vs, Lss))
  Vs_Lss_Asr_Lrr <- dmult(Vs_Lss, Asr) %*% Lrr
  Vs_Bss_minus_Lss <- dmult(Vs, Bss - Lss)

  # Auxiliary function: if is_icio, melds (sometimes specifying)
  # and then, if it is_group, consolidates and names rows
  # with ggn_names
  check_meld_group <- function(df, meld_rows = TRUE, meld_cols = TRUE) {
    if (is_icio) {
      df <- meld(df, meld_rows = meld_rows, meld_cols = meld_cols)
    }
    if (is_group) {
      col_names <- colnames(df)
      df <- name(sumnrow(df, N), ggn_names, col_names)
    }
    return(df)
  }

  # DVA terms----


  cli::cli_alert_info("Calculating DVA terms...")

  VAX1 <- check_meld_group(dmult(Vs_Lss, Ysr))

  VAX2 <- check_meld_group(Vs_Lss_Asr_Lrr %*% Yrr)

  # Terms in VAX3, VAX4, REF1 and REF2 and DDC all have dimension
  # GN x GXN or N x GXN, so Vs_Lss_Asr_Lrr must be first melded in rows and,
  # if is_exporter and is_group, summed by sectors to obtain N rows
  # Once calculated, those terms are melded by columns

  Vs_Lss_Asr_Lrr <- check_meld_group(Vs_Lss_Asr_Lrr, meld_cols = FALSE)

  VAX3 <- meld(hmult(Vs_Lss_Asr_Lrr, sum_Yrjxs))

  VAX4 <- meld(hmult(Vs_Lss_Asr_Lrr, sum_Arj_sum_Bjk_Yklxs))

  REF1 <- meld(hmult(Vs_Lss_Asr_Lrr, Yrs))

  REF2 <- meld(hmult(Vs_Lss_Asr_Lrr, sum_Arj_sum_Bjk_Yks))

  # DDC is (GXN x GXN) (GXN x G)
  DDC <- check_meld_group(dmult(Vs_Bss_minus_Lss, EXGR))

  # GXN x GXN or KN x GXN
  sum_Vt_Btsnots_Asr_Lrr <- dmult(sum_Vt_Btsnots, Asr %*% Lrr)

  # FVA terms----
  cli::cli_alert_info("Calculating FVA terms...")

  FVA1 <- check_meld_group(dmult(sum_Vt_Btsnots, Ysr))

  FVA2 <- check_meld_group(sum_Vt_Btsnots_Asr_Lrr %*% Yrr)

  # FVA3 and FVA4 terms have dimension GN x GXN or KN x GXN
  # so sum_Vt_Btsnots_Asr_Lrr must be previously melded by rows

  sum_Vt_Btsnots_Asr_Lrr <- check_meld_group(sum_Vt_Btsnots_Asr_Lrr,
                                             meld_cols = FALSE)

  FVA3 <- meld(hmult(sum_Vt_Btsnots_Asr_Lrr, sum_Yrj))

  FVA4 <- meld(hmult(sum_Vt_Btsnots_Asr_Lrr, sum_Arj_sum_Bjk_Ykl))


  # FDC is (GXN x GXN) x (GXN x G) or (KN x KX) x (KN x G)
  FDC <- check_meld_group(dmult(sum_Vt_Bts_minus_Btsnots, EXGR))

  EXGR <- check_meld_group(EXGR)

  # Terms

  DAVAX <- VAX1 + VAX2
  VAX <- DAVAX + VAX3 + VAX4
  REF <- REF1 + REF2
  DVA <- VAX + REF
  DC <- DVA + DDC
  FVA <- FVA1 + FVA2 + FVA3 + FVA4
  FC <- FVA + FDC
  GVC <- EXGR - DAVAX
  GVCF <- DVA - DAVAX
  GVCB <- GVC - GVCF

  # Output----
  cli::cli_alert_info("Preparing output ...")

  if(output == "standard") {

    exvadec <- list(EXGR, DC, DVA, VAX, DAVAX, REF, DDC,
                 FC, FVA, FDC, GVC, GVCB, GVCF)

    exvadec_names <- c("EXGR", "DC", "DVA", "VAX", "DAVAX", "REF", "DDC",
                    "FC", "FVA", "FDC", "GVC", "GVCB", "GVCF")

  } else if (output == "terms") {

    exvadec <- list(EXGR, VAX1, VAX2, VAX3, VAX4,
                 REF1, REF2, DDC,
                 FVA1, FVA2, FVA3, FVA4, FDC)

    exvadec_names <- list("EXGR", "VAX1", "VAX2", "VAX3", "VAX4",
                       "REF1", "REF2", "DDC",
                       "FVA1", "FVA2", "FVA3", "FVA4", "FDC")

  }

  names(exvadec) <- exvadec_names

  # Names of rows and columns
  if (is_all) {
    exvadec <- lapply(exvadec, "rownames<-", gn_names)
    exvadec <- lapply(exvadec, "colnames<-", g_names)
  } else if (is_exporter) {
    row_names <- paste0(exporter, gsub("^[CD]", "_", n_names))
    exvadec <- lapply(exvadec, "rownames<-", row_names)
    exvadec <- lapply(exvadec, "colnames<-", g_names)
  }

  # Metadata
  exvadec$dims <- wio$dims
  exvadec$names <- wio$names
  exvadec$method <- "bm_src"
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


