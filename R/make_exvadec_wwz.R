#' Make Wang et al. (2013) decomposition
#'
#' Creates Wang et al. (2013) decomposition
#' @param wio_object A class `wio` object
#' @param exporter A string character with the code of a country
#' @param output String, type of output
#' @param quiet Boolean, quiet output
#' @keywords internal
#' @noRd
#' @return A list with matrices
make_exvadec_wwz <- function(wio_object, exporter = "all",
                             output = "standard", quiet = FALSE){

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
  is_group <- FALSE

  # Initialize names and number of countries in case exporter is group
  gg_names <- g_names
  GG <- G

  if (is_exporter) {
    # Row names with sectors for exporter (valid also if group)
    expn_names <- paste0(exporter, "_", gsub("^[CD]", "", n_names))

    # Position of exporter
    pgn_exp <- grep(get_geo_codes(exporter, wio_type, TRUE), gxn_names)
    pg_exp <- grep(get_geo_codes(exporter, wio_type), g_names)

    # Check if exporter is group
    is_group <- ifelse(length(pg_exp) > 1, TRUE, FALSE)
    if (is_group) {
      group_names <-
        strsplit(get_geo_codes(exporter, wio$type, TRUE), "[|]")[[1]]
      gg_names <- c(g_names[!g_names %in% group_names], exporter)
      GG <- length(gg_names)
    }
  }

  # Auxiliary functions to improve code readability
  get_block_exp <- function(df) df[pgn_exp, pgn_exp, drop = FALSE]
  get_cols_exp <- function(df) df[, pgn_exp, drop = FALSE]
  get_rows_exp <- function(df) df[pgn_exp, , drop = FALSE]
  group_cols_exp <- function(df) group_cols(df, pg_exp, "replace", exporter, wio_type)
  sum_X <- function(df) name(repmat(rsums(df), GG), gxn_names, gg_names)
  sum_Xxs <- function(df) name(repmat(rsums(df), GG) - df, gxn_names, gg_names)
  bkt_bkoffd <- function(df) {
    df <- bkoffd(df)
    df <- bkt(df)
  }
  bkt_col_bkoffd <- function(df) {
    df <- bkoffdx(df, gg_names)
    df <- df[, exporter, drop = FALSE]
    df <- bkt(df)
  }
  bkt_col <- function(df) bkt(df[, exporter, drop = FALSE])
  sumnrow_meld <- function(df, meld_rows = TRUE, meld_cols = TRUE) {
    if (is_icio) {
      df <- meld(df, meld_rows = meld_rows, meld_cols = meld_cols)
    }
    if (is_group) {
      col_names <- colnames(df)
      df <- name(sumnrow(df, N), expn_names, col_names)
    }
    return(df)
  }
  set_zero_exp <- function(df) {
    if (nrow(df) == ncol(df)) {
      return(set_zero(df, pgn_exp, pgn_exp))
    } else {
      return(set_zero(df, pgn_exp, pg_exp))
    }
  }

  # DVA auxiliary matrices----

  if (!quiet) {cli::cli_alert_info("Preparing DVA auxiliary matrices...")}

  # Vs
  Vs <- if (is_exporter) get_block_exp(wio$W) else wio$W

  # Preparation
  Bm <- if (is_exporter) set_zero_exp(wio$Bm) else wio$Bm
  Bd <- wio$B - Bm
  Am <- if (is_exporter) set_zero_exp(wio$Am) else wio$Am
  Ad <- wio$A - Am
  Ym <- if (is_exporter) set_zero_exp(wio$Ym) else wio$Ym
  Yd <- wio$Y - Ym

  Brr <- Bd
  Brj <- Bm
  Bss <- if (is_exporter) get_block_exp(Bd) else Bd
  Bsr <- if (is_exporter) get_rows_exp(Bm) else Bm
  Bts <- if (is_exporter) get_cols_exp(Bm) else Bm
  Asr <- if (is_exporter) get_rows_exp(Am) else Am
  Lrr <- if (is_group) solve(diag(GXN) - Ad) else wio$Ld
  Lss <- if (is_exporter) get_block_exp(Lrr) else Lrr

  Yrr <- Yd
  Yjj <- Yss <- if (is_group) group_cols_exp(Yd) else Yd
  Yrj <- Yjk <- if (is_group) group_cols_exp(Ym) else Ym
  Ykl <- if (is_group) group_cols_exp(wio$Y) else wio$Y

  # VAX1
  Ysr <- if (is_exporter) get_rows_exp(Ym) else Ym

  # VAX2
  Brr_Yrr <- Bd %*% Yd

  # VAX3
  sum_Brj_Yjj <-
    if (is_exporter) {
      bkt_col_bkoffd(Brj %*% bkoffdx(sum_X(Yjj), gg_names))
    } else {
      bkt(bkoffd((Brj %*% bkoffd(sum_X(Yjj)))))
    }

  # VAX4
  sum_Brr_Yrj <-
    if (is_exporter) {
      bkt_col_bkoffd(Brr %*% bkoffdx(sum_Xxs(Yrj), gg_names))
    } else {
      bkt(Brr %*% bkoffd(sum_Xxs(Yrj)))
    }

  # VAX5
  sum_Brj_Yjk <-
    if (is_exporter) {
      bkt_col_bkoffd(Brj %*% bkoffdx(sum_Xxs(Yjk), gg_names))
    } else {
      bkt(bkoffd(Brj %*% bkoffd(sum_Xxs(Yjk))))
    }

  # Be careful, Yrs is bkt(Yrj) but Brr_Yrs is not Brr %*% bkt(Yrs)
  # but bkt(Brr %*% Yrj)
  # REF1
  Brr_Yrs <- if (is_exporter) bkt_col(Brr %*% Yrj)  else bkt(Brr %*% Yrj)

  # REF2
  Brj_Yjs <- if (is_exporter) bkt_col(Brj %*% Yjk) else bkt(Brj %*% Yjk)

  # REF3
  Brs_Yss <- if (is_exporter) bkt_col(Brj %*% Yss) else bkt(Brj %*% Yss)

  # DDC1
  sum_Brs_Ysj <-
    if (is_exporter) {
      bkt_col(Brj %*% bkdx(sum_X(Yrj), gg_names))
    } else {
      bkt(Brj %*% bkd(sum_X(Yrj)))
    }

  # DDC2
  Xr <- bkt(sum_X(wio$X))
  # As it is bkt, we will use basic country codes
  if (is_exporter) {
    Xr <- Xr[grep(get_geo_codes(exporter, wio_type), gn_names), , drop = FALSE]
  }

  # FVA auxiliary matrices----
  if (!quiet) {cli::cli_alert_info("Preparing FVA auxiliary matrices...")}

  # Auxiliary matrices fro FVA and FDC terms
  Asr_Lrr <- Asr %*% Lrr
  Asr_Lrr_Yrr <- Asr_Lrr %*% Yrr

  EXGR <- if (is_exporter) set_zero_exp(wio$EXGR) else wio$EXGR
  Er <- bkt(bkoffd(name(repmat(rowSums(EXGR), GX), gxn_names, gx_names)))
  if (is_exporter) {
    Er <- get_rows_exp(Er)
  }
  Asr_Lrr_Er <- meld(hmult(Asr_Lrr, Er), meld_rows = FALSE)


  # VA matrices
  Vt_Bts <- meld(dmult(wio$W, Bts), meld_cols = FALSE)
  Vr_Brs <- bkt(Vt_Bts)

  sum_Vt_Bts <- bkoffd(name(repmat(sumncol(Vr_Brs, N), G),
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

  # DVA terms----

  if (!quiet) {cli::cli_alert_info("Calculating DVA terms...")}

  VAX1 <- sumnrow_meld(dmult(Vs_Bss, Ysr))
  VAX2 <- sumnrow_meld(Vs_Lss_Asr %*% Brr_Yrr)

  # Meld to multiply by GNxGX
  Vs_Lss_Asr <- sumnrow_meld(Vs_Lss_Asr, meld_cols = FALSE)
  Vs_Bss_minus_Lss_Asr <- meld(Vs_Bss_minus_Lss_Asr, meld_cols = FALSE)

  VAX3 <- meld(hmult(Vs_Lss_Asr, sum_Brj_Yjj))
  VAX4 <- meld(hmult(Vs_Lss_Asr, sum_Brr_Yrj))
  VAX5 <- meld(hmult(Vs_Lss_Asr, sum_Brj_Yjk))

  REF1 <- meld(hmult(Vs_Lss_Asr, Brr_Yrs))
  REF2 <- meld(hmult(Vs_Lss_Asr, Brj_Yjs))
  REF3 <- meld(hmult(Vs_Lss_Asr, Brs_Yss))

  DDC1 <- meld(hmult(Vs_Lss_Asr, sum_Brs_Ysj))
  DDC2 <- sumnrow_meld(hmult(Vs_Bss_minus_Lss_Asr, Xr))

  # FVA terms----

  if (!quiet) {cli::cli_alert_info("Calculating FVA terms...")}

  FVA1 <- sumnrow_meld(hmult(Vr_Brs, Ysr))
  FVA2 <- sumnrow_meld(hmult(sum_Vt_Btsxr, Ysr))
  FVA3 <- sumnrow_meld(hmult(Vr_Brs, Asr_Lrr_Yrr))
  FVA4 <- sumnrow_meld(hmult(sum_Vt_Btsxr, Asr_Lrr_Yrr))

  FDC1 <- sumnrow_meld(hmult(Vr_Brs, Asr_Lrr_Er))
  FDC2 <- sumnrow_meld(hmult(sum_Vt_Btsxr, Asr_Lrr_Er))

  EXGR <-
    if (is_exporter) {
      sumnrow_meld(get_rows_exp(set_zero_exp(wio$EXGR)))
    } else {
      meld(wio$EXGR)
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
  if (!quiet) {cli::cli_alert_info("Preparing output ...")}

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

    exvadec <- list(EXGR, DVA_FIN, DVA_INT,
                    DVA_INTrex1, DVA_INTrex2, DVA_INTrex3,
                    RDV_FIN1, RDV_FIN2, RDV_INT, DDC_FIN, DDC_INT,
                    MVA_FIN, MVA_INT, MDC, OVA_FIN, OVA_INT, ODC)

    exvadec_names <- c("EXGR", "DVA_FIN", "DVA_INT",
                       "DVA_INTrex1", "DVA_INTrex2", "DVA_INTrex3",
                       "RDV_FIN1", "RDV_FIN2", "RDV_INT", "DDC_FIN", "DDC_INT",
                       "MVA_FIN", "MVA_INT", "MDC", "OVA_FIN", "OVA_INT", "ODC")

  }

  # Set names
  names(exvadec) <- exvadec_names

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


  if (!quiet) {cli::cli_alert_success("Done!")}

  # Print result summary
  if (!quiet) {
    if (is_all) {
      get_exvadec_bkdown(exvadec)
    } else {
      get_exvadec_bkdown(exvadec, exporter)
    }
  }

  return(exvadec)

}
