#' Make Wang et al. (2019) decomposition
#'
#'#' Calculates the decomposition of value added in exports of a country
#' or a group of countries according to the methodology of Wang, Wei
#' and Zhu (2013)
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
#'   `terms`: `DVA_FIN`, `DVA_INT`, `DVA_INTrex`... The 16
#'   terms as per Wang et al. (2013, pag. 35).
#'   `terms2`: `DVA_FIN`, `DVA_INT`, `DVA_INTrex`... The 16
#'   terms as per Wang et al. (2013, pag. 61).
#' @keywords internal
#' @noRd
#' @return A list object of class `exvadec` with several matrices
#' plus metadata.
make_exvadec_wwz <- function(wio_object, exporter = "all",
                             output = "standard"){

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

  # Countries with groups (initially as G)
  GG <- G
  # Country names with groups (initially as g_names)
  gg_names <- g_names
  ggn_names <- gn_names

  # # Position of exporter, groups
  if (is_exporter) {
    pgn_exp <- grep(get_geo_codes(exporter, wio_type, TRUE), gxn_names)
    pg_exp <- grep(get_geo_codes(exporter, wio_type), g_names)
    is_group <- ifelse(length(pg_exp) > 1, TRUE, FALSE)
    # If is exporter group, ggn_names will be EU27_01T02...EU27_90T98
    if (is_group) {
      ggn_names <- paste0(exporter, "_", gsub("[CD]", "", n_names))
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
  Bss <- Brr <- wio$Bd
  Bsr <- Brs <- Brj <- Bts <- wio$Bm
  if (is_exporter) {
    Bsr <- Brs <- Brj <- Bts <- set_zero(Bsr, pgn_exp, pgn_exp)
    Bss <- (wio$B - Bsr)[pgn_exp, pgn_exp, drop = FALSE]
    Bsr <- Bsr[pgn_exp, , drop = FALSE]
    Brs <- Brs[, pgn_exp, drop = FALSE]
    Bts <- Bts[, pgn_exp, drop = FALSE]
  }

  # Y
  Ysr <- wio$Ym
  Yrr <- Yjj <- Yss <- wio$Yd
  if (is_exporter){
    Ysr <- set_zero(Ysr, pgn_exp, pg_exp)
    Yrr <- Yjj <- Yss <- wio$Y - Ysr
    Ysr <- Ysr[pgn_exp, , drop = FALSE]
    if (is_group) {
      Yjj <- Yss <- group_cols(Yjj, pg_exp, "replace", exporter, wio_type)
    }
  }
  Yrj <- Yjk <- wio$Ym
  if (is_exporter) {
    Yrj <- Yjk <- set_zero(Yrj, pgn_exp, pg_exp)
    if (is_group) {
      Yrj <- Yjk <- group_cols(Yrj, pg_exp, "replace", exporter, wio_type)
      # Number of countries with groups
      GG <- ncol(Yrj)
      # Names of countries, with groups
      gg_names <- colnames(Yrj)
    }
  }
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

  # L
  Lss <- Lrr <- wio$Ld
  if (is_exporter) {
    if (is_group) {
      Lss <- name(solve(diag(GXN) - Ass), gxn_names, gxn_names)
    }
    Lrr <- Lss
    Lss <- Lss[pgn_exp, pgn_exp, drop = FALSE]
  }

  # Brr_Yrr
  Brr_Yrr <- Brr %*% Yrr

  # sum_Brj_Yjj
  if (is_all) {
    sum_Brj_Yjj <- name(repeach(rowSums(Yjj), G), gxn_names, g_names)
    sum_Brj_Yjj <- bkt(bkoffd(Brj %*% bkoffd(sum_Brj_Yjj)))
  } else if (is_exporter) {
    sum_Brj_Yjj <- name(repeach(rowSums(Yjj), GG), gxn_names, gg_names)
    sum_Brj_Yjj <- Brj %*% bkoffdx(sum_Brj_Yjj, gg_names)
    sum_Brj_Yjj <- bkoffdx(sum_Brj_Yjj, gg_names)[, c(exporter), drop = FALSE]
    sum_Brj_Yjj <- bkt(sum_Brj_Yjj)
  }

  # sum_Brr_Yrj
  if (is_all) {
    sum_Brr_Yrj <- bkt(Brr %*% bkoffd(rowSums(Yrj) - Yrj))
  } else if (is_exporter) {
    sum_Brr_Yrj <- Brr %*% bkoffdx(rowSums(Yrj) - Yrj, gg_names)
    sum_Brr_Yrj <- bkoffdx(sum_Brr_Yrj, gg_names)[, c(exporter), drop = FALSE]
    sum_Brr_Yrj <- bkt(sum_Brr_Yrj)
  }

  # sum_Brj_Yjk
  if (is_all) {
    sum_Brj_Yjk <- bkt(bkoffd(Brj %*% bkoffd(rowSums(Yjk) - Yjk)))
  } else if (is_exporter) {
    sum_Brj_Yjk <- Brj %*% bkoffdx(rowSums(Yjk) - Yjk, gg_names)
    sum_Brj_Yjk <- bkoffdx(sum_Brj_Yjk, gg_names)[, c(exporter), drop = FALSE]
    sum_Brj_Yjk <- bkt(sum_Brj_Yjk)
  }

  # Brr_Yrs
  # Be careful, Yrs is bkt(Yrj) but Brr_Yrs is not Brr %*% bkt(Yrs)
  # but bkt(Brr %*% Yrj)
  if (is_all) {
    Brr_Yrs <- bkt(Brr %*% Yrj)
  } else if (is_exporter) {
    Brr_Yrs <- bkt((Brr %*% Yrj)[, c(exporter), drop = FALSE])
  }

  # Brj_Yjs
  if (is_all) {
    Brj_Yjs <- bkt(Brj %*% Yjk)
  } else if (is_exporter) {
    Brj_Yjs <- bkt((Brj %*% Yjk)[, c(exporter), drop = FALSE])
  }

  # Brs_Yss
  if (is_all) {
    Brs_Yss <- bkt(Brj %*% Yss)
  } else if (is_exporter) {
    Brs_Yss <- bkt((Brj %*% Yss)[, c(exporter), drop = FALSE])
  }

  # sum_Brs_Ysj

  if (is_all) {
    sum_Brs_Ysj <- bkd(name(repmat(rowSums(Yrj), G), gxn_names, g_names))
    sum_Brs_Ysj <- bkt(Brj %*% sum_Brs_Ysj)
  } else if (is_exporter) {
    sum_Brs_Ysj <- name(repmat(rowSums(Yrj), GG), gxn_names, gg_names)
    sum_Brs_Ysj <- Brj %*% bkdx(sum_Brs_Ysj, gg_names)
    sum_Brs_Ysj <- bkt(sum_Brs_Ysj[, c(exporter), drop = FALSE])
  }


  Xr <- bkt(name(repmat(wio$X, G), gxn_names, g_names))
  # As it is bkt, we will use basic country codes
  if (is_exporter) {
    Xr <- Xr[grep(get_geo_codes(exporter, wio_type), gn_names), , drop = FALSE]
  }

  Asr_Lrr <- Asr %*% Lrr

  Asr_Lrr_Yrr <- Asr_Lrr %*% Yrr

  EXGR <- wio$EXGR
  if (is_exporter) {
    EXGR <- set_zero(EXGR, pgn_exp, pg_exp)
  }


  Er <- bkt(bkoffd(name(repmat(rowSums(EXGR), GX), gxn_names, gx_names)))
  if (is_exporter) {
    Er <- Er[pgn_exp, , drop = FALSE]
  }


  Asr_Lrr_Er <- hmult(Asr_Lrr, Er)
  # Meld columns
  if (is_icio) {
    Asr_Lrr_Er <- meld(hmult(Asr_Lrr, Er), meld_rows = FALSE)
  }

  # FVA auxiliary matrices----
  cli::cli_alert_info("Preparing FVA auxiliary matrices...")

  # FVA matrices
  Vt_Bts <- dmult(wio$W, Bts)
  if (is_icio) {
    # Rows must be melded first, otherwise it does not match
    Vt_Bts <- meld(Vt_Bts, meld_cols = FALSE)
  }
  Vr_Brs <- bkt(Vt_Bts)


  sum_Vt_Bts <- bkoffd(name(repmat(sum_every_nth_col(Vr_Brs, N), G),
                            rownames(Vr_Brs), gn_names))

  sum_Vt_Btsxr <- sum_Vt_Bts - Vr_Brs
  # Now we diagonalize
  Vr_Brs <- bkdiag(Vr_Brs)
  sum_Vt_Btsxr <- bkdiag(sum_Vt_Btsxr)


  # Value added matrices----

  Vs_Bss <- diagcs(dmult(Vs, Bss))
  Vs_Lss <- diagcs(dmult(Vs, Lss))
  Vs_Lss_Asr <- dmult(Vs_Lss, Asr)
  Vs_Lss_Asr_Lrr <- Vs_Lss_Asr %*% Lrr
  Vs_Bss_minus_Lss <- dmult(Vs, Bss - Lss)
  Vs_Bss_minus_Lss_Asr <- dmult(Vs_Bss_minus_Lss, Asr)

  # Auxiliary function: if is_icio, melds (sometimes rows only)
  # and then, if it is_group, consolidates the rows and names the matrix
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

  VAX2 <- check_meld_group(Vs_Lss_Asr %*% Brr_Yrr)

  # Meld to multiply by GNxGX
  Vs_Lss_Asr <- check_meld_group(Vs_Lss_Asr, meld_cols = FALSE)
  Vs_Bss_minus_Lss_Asr <- meld(Vs_Bss_minus_Lss_Asr, meld_cols = FALSE)


  VAX3 <- meld(hmult(Vs_Lss_Asr, sum_Brj_Yjj))

  VAX4 <- meld(hmult(Vs_Lss_Asr, sum_Brr_Yrj))

  VAX5 <- meld(hmult(Vs_Lss_Asr, sum_Brj_Yjk))

  REF1 <- meld(hmult(Vs_Lss_Asr, Brr_Yrs))

  REF2 <- meld(hmult(Vs_Lss_Asr, Brj_Yjs))

  REF3 <- meld(hmult(Vs_Lss_Asr, Brs_Yss))

  DDC1 <- meld(hmult(Vs_Lss_Asr, sum_Brs_Ysj))

  DDC2 <- check_meld_group(hmult(Vs_Bss_minus_Lss_Asr, Xr))

  # FVA terms----

  cli::cli_alert_info("Calculating FVA terms...")

  FVA1 <- check_meld_group(hmult(Vr_Brs, Ysr))

  FVA2 <- check_meld_group(hmult(sum_Vt_Btsxr, Ysr))

  FVA3 <- check_meld_group(hmult(Vr_Brs, Asr_Lrr_Yrr))

  FVA4 <- check_meld_group(hmult(sum_Vt_Btsxr, Asr_Lrr_Yrr))

  FDC1 <- check_meld_group(hmult(Vr_Brs, Asr_Lrr_Er))

  FDC2 <- check_meld_group(hmult(sum_Vt_Btsxr, Asr_Lrr_Er))

  if (is_all) {
    EXGR <- meld(EXGR)
  } else if (is_exporter) {
    EXGR <- EXGR[pgn_exp, , drop = FALSE]
    EXGR <- check_meld_group(EXGR)
  }

  # Initial output
  exvadec <- list(VAX1, VAX2, VAX3, VAX4, VAX5, REF1, REF2, REF3,
               DDC1, DDC2, FVA1, FVA2, FVA3, FVA4, FDC1, FDC2)

  exvadec_names <- c("VAX1", "VAX2", "VAX3", "VAX4", "VAX5",
                  "REF1", "REF2", "REF3", "DDC1", "DDC2",
                  "FVA1", "FVA2", "FVA3", "FVA4",
                  "FDC1", "FDC2")

  names(exvadec) <- exvadec_names


  # Output----
  cli::cli_alert_info("Preparing output ...")

  if (output == "standard"){

    VAX <- Reduce("+", exvadec[1:5])
    REF <- Reduce("+", exvadec[6:8])
    DVA <- VAX + REF
    DDC <- Reduce("+", exvadec[9:10])
    DC <- DVA + DDC
    FVA <- Reduce("+", exvadec[11:14])
    FDC <- Reduce("+", exvadec[15:16])
    FC <- FVA + FDC

    exvadec <- list(EXGR, DC, DVA, VAX, REF, DDC, FC, FVA, FDC)

    exvadec_names <- c("EXGR", "DC", "DVA", "VAX", "REF", "DDC",
                       "FC", "FVA", "FDC")

  } else if (output == "terms"){

    DVA_FIN <- exvadec$VAX1  # T01
    DVA_INT <- exvadec$VAX2  # T02
    DVA_INTrex1 <- exvadec$VAX3  # T03
    DVA_INTrex2 <- exvadec$VAX4  # T04
    DVA_INTrex3 <- exvadec$VAX5  # T05
    RDV_FIN1 <- exvadec$REF1  # T06
    RDV_FIN2 <- exvadec$REF2  # T07
    RDV_INT <- exvadec$REF3  # T08
    DDC_FIN <- exvadec$DDC1  # T09
    DDC_INT <- exvadec$DDC2  # T10
    FVA_FIN1 <- exvadec$FVA1  # T11
    FVA_FIN2 <- exvadec$FVA2  # T12
    FVA_INT1 <- exvadec$FVA3  # T13
    FVA_INT2 <- exvadec$FVA4  # T14
    MDC <- exvadec$FDC1  # T15
    ODC <- exvadec$FDC2  # T16

    exvadec <- list(EXGR, DVA_FIN, DVA_INT,
                    DVA_INTrex1, DVA_INTrex2, DVA_INTrex3,
                    RDV_FIN1, RDV_FIN2, RDV_INT, DDC_FIN, DDC_INT,
                    FVA_FIN1, FVA_FIN2, FVA_INT1, FVA_INT2, MDC, ODC)

    exvadec_names <- c("EXGR", "DVA_FIN", "DVA_INT",
                    "DVA_INTrex1", "DVA_INTrex2", "DVA_INTrex3",
                    "RDV_FIN1", "RDV_FIN2", "RDV_INT", "DDC_FIN", "DDC_INT",
                    "FVA_FIN1", "FVA_FIN2",
                    "FVA_INT1", "FVA_INT2", "MDC", "ODC")


  } else if (output == "terms2"){

    DVA_FIN <- exvadec$VAX1  # T01
    DVA_INT <- exvadec$VAX2  # T02
    DVA_INTrex1 <- exvadec$VAX3  # T03
    DVA_INTrex2 <- exvadec$VAX4  # T04
    DVA_INTrex3 <- exvadec$VAX5  # T05
    RDV_FIN1 <- exvadec$REF1  # T06
    RDV_FIN2 <- exvadec$REF2  # T07
    RDV_INT <- exvadec$REF3  # T08
    DDC_FIN <- exvadec$DDC1  # T09
    DDC_INT <- exvadec$DDC2  # T10
    MVA_FIN <- exvadec$FVA1  # T11
    MVA_INT <- exvadec$FVA3  # T12
    MDC <- exvadec$FDC1  # T13
    OVA_FIN <- exvadec$FVA4  # T14
    OVA_INT <- exvadec$FVA2  # T15
    ODC <- exvadec$FDC2  # T16

    exvadec <- list(EXGR, DVA_FIN, DVA_INT, DVA_INTrex1, DVA_INTrex2, DVA_INTrex3,
                 RDV_FIN1, RDV_FIN2, RDV_INT, DDC_FIN, DDC_INT,
                 MVA_FIN, MVA_INT, MDC, OVA_FIN, OVA_INT, ODC)

    exvadec_names <- c("EXGR", "DVA_FIN", "DVA_INT",
                    "DVA_INTrex1", "DVA_INTrex2", "DVA_INTrex3",
                    "RDV_FIN1", "RDV_FIN2", "RDV_INT", "DDC_FIN", "DDC_INT",
                    "MVA_FIN", "MVA_INT", "MDC", "OVA_FIN", "OVA_INT", "ODC")


  }

  # Set names
  names(exvadec) <- exvadec_names

  # Name rows and columns
  if (is_all){
    exvadec <- lapply(exvadec, "rownames<-", gn_names)
    exvadec <- lapply(exvadec, "colnames<-", g_names)
  } else if (is_exporter) {
    row_names <- paste0(exporter, gsub("C|D", "_", n_names))
    exvadec <- lapply(exvadec, "rownames<-", row_names)
    exvadec <- lapply(exvadec, "colnames<-", g_names)
  }


  # Names and dimensions of exvadec object
  exvadec$dims <- wio$dims
  exvadec$names <- wio$names
  exvadec$method <- "wwz"
  exvadec$output <- output
  exvadec$source <- wio$type
  exvadec$year <- wio$year
  if (is_exporter) {
    exvadec$exporter <- exporter
  }
  class(exvadec) <- "exvadec"


  # cli::cli_alert_success("Done!")

  # Print result summary
  if (is_all) {
    get_exvadec_bkdown(exvadec)
  } else{
    get_exvadec_bkdown(exvadec, exporter)
  }

  return(exvadec)

  #End function

}
