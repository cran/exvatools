#' Make OECD (2019) decomposition
#'
#' Creates OECD decomposition
#' @param wio_object A class `wio` object
#' @param exporter A string character with the code of a country
#' @param output String, type of output
#' @param quiet Boolean
#' @keywords internal
#' @noRd
#' @return A list with matrices
make_exvadec_oecd <- function(wio_object, exporter = "all",
                              output = "standard", quiet = FALSE) {

  # exporter <- "ESP"
  # output <- "tiva"
  # quiet <- FALSE


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
    pgn_exp_melt <- grep(get_geo_codes(exporter, wio_type), gn_names)

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
  get_rows_exp_melt <- function(df) df[pgn_exp_melt, , drop = FALSE]
  group_cols_exp <- function(df) group_cols(df, pg_exp, "replace", exporter, wio_type)
  # sum_X <- function(df) name(repmat(rsums(df), GG), gxn_names, gg_names)
  # sum_Xxs <- function(df) name(repmat(rsums(df), GG) - df, gxn_names, gg_names)
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
  sumnrow_rows_exp_melt <- function(df) {
    df <- df[pgn_exp_melt, , drop = FALSE]
    if (is_group) {
      df <- sumnrow(df, N, expn_names)
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

  # Simple auxiliary matrices----

  if (!quiet) {cli::cli_alert_info("Preparing simple auxiliary matrices...")}

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

  EXGR <- if (is_exporter) get_rows_exp(set_zero_exp(wio$EXGR)) else wio$EXGR

  # Complex auxiliary matrices----

  if (!quiet) {cli::cli_alert_info("Preparing complex auxiliary matrices...")}

  Vs_Bss <- diagcs(dmult(Vs, Bss))
  Vs_Lss <- diagcs(dmult(Vs, Lss))
  Vs_Bss_minus_Lss <- diagcs(dmult(Vs, Bss - Lss))

  K <- nrow(Lss) # Normally GXN

  Vs_diag_Lss <- diagcs(dmult(Vs, Lss * diag(K)))
  Vs_offdiag_Lss <- diagcs(dmult(Vs, Lss * (matrix(1, K, K) - diag(K))))

  # VBY
  Vs_Bsj_Yjk <- dmult(wio$W, (wio$B %*% wio$Y))


  # DVA terms----

  if (!quiet) {cli::cli_alert_info("Calculating DVA terms...")}

  if (is_exporter) {
    VAX <- set_zero_exp(bkoffd(Vs_Bsj_Yjk))
    VAD <- sumnrow_meld(get_rows_exp(Vs_Bsj_Yjk - VAX))
    VAX <- sumnrow_meld(get_rows_exp(VAX))
  } else {
    VAX <- meld(bkoffd(Vs_Bsj_Yjk))
    VAD <- meld(bkd(Vs_Bsj_Yjk))
  }

  # Total VA of Fnal Demand
  FDVA <- VAD + VAX

  DC <- sumnrow_meld(dmult(Vs_Bss, EXGR))

  # DVA direct
  DVA1 <- sumnrow_meld(dmult(Vs_diag_Lss, EXGR))

  # DVA indirect
  DVA2 <- sumnrow_meld(dmult(Vs_offdiag_Lss, EXGR))

  # DVA reimported
  DDC <- sumnrow_meld(dmult(Vs_Bss_minus_Lss, EXGR))

  # FVA terms----

  if (!quiet) {cli::cli_alert_info("Calculating FVA terms...")}

  # FC
  Vt_Bts <- diagcs(dmult(wio$W, Bts))

  FC <- sumnrow_meld(dmult(Vt_Bts, EXGR))

  # Export matrices for GVC indicators

  Esr <- if (is_exporter) set_zero_exp(wio$EXGR) else wio$EXGR
  Esr <- bkdiag(bkoffd(name(repmat(rowSums(Esr), GX), gxn_names, gx_names)))
  Ers <- bkt(Esr)
  if (is_exporter) {
    Esr <- get_rows_exp(Esr)
    Ers <- get_rows_exp(Ers)
  }

  # GVCB (backwards)
  # Block transpose so rows are 0, V2_B21, V3_B31...
  # and we multiply by 0, E1, E1, E1 (not 0, E12, E13...!)
  # (  0      V2B21E1   V3B31E1 )
  # (V1B12E2     0      V3V32E2 )
  # (V1B13E3  V2B23E3      0    )
  # We recalculate
  Vt_Bts <- bkt(dmult(wio$W, Bts))

  GVCB <- bktt(hmult(Vt_Bts, Esr))
  GVCB <- sumnrow_meld(sumgcols(GVCB, N, gx_names))

  # GVCB <- sumnrow_meld(name(sum_by_groups_of(GVCB, N, bycols = TRUE),
  #                               rownames(Esr), gx_names))

  # GVCF (forward)
  # Rows now are 0, V1_B12, V1_B13...
  # and multiply by 0, E2, E3...
  # (  0      V1B12E2   V1B13E3 )
  # (V2B21E1     0      V2V23E3 )
  # (V3B31E1  V3B32E2      0    )
  Vs_Bsr <- dmult(Vs, Bsr)

  GVCF <- bktt(hmult(Vs_Bsr, Ers))
  GVCF <- sumnrow_meld(sumgcols(GVCF, N, gx_names))

  EXGR <- sumnrow_meld(EXGR)


  DVA <- DVA1 + DVA2
  REF <- DVA - VAX
  GVC <- GVCB + GVCF

  # Extra TIVA terms----

  if (output == "tiva") {

    if (!quiet) {cli::cli_alert_info("Calculating extra TiVA terms...")}

    IMGR <-
      if (is_exporter) {
        sumnrow_rows_exp_melt(bkt(meld(set_zero_exp(wio$EXGR))))
      } else {
        bkt(EXGR)
      }

    VAM <-
      if (is_exporter) {
        sumnrow_rows_exp_melt(bkt(meld(set_zero_exp(bkoffd(Vs_Bsj_Yjk)))))
      } else {
        VAM <- bkt(meld(bkoffd(Vs_Bsj_Yjk)))
      }

    Zm <- if (is_exporter) set_zero_exp(wio$Zm) else wio$Zm
    Zd <- wio$Z - Zm

    Zm_melt <- meld(sumgcols(Zm, N, gx_names))
    Zd_melt <- meld(sumgcols(Zd, N, gx_names))

    EXGR_INT <- if (is_exporter) sumnrow_rows_exp_melt(Zm_melt) else Zm_melt
    IMGR_INT <- if (is_exporter) sumnrow_rows_exp_melt(bkt(Zm_melt)) else bkt(Zm_melt)
    DOM_INT <- if (is_exporter) sumnrow_rows_exp_melt(Zd_melt) else Zd_melt

    EXGR_FNL <- if (is_exporter) sumnrow_rows_exp_melt(meld(Ym)) else meld(Ym)
    IMGR_FNL <- if (is_exporter) sumnrow_rows_exp_melt(meld(bkt(Ym))) else meld(bkt(Ym))
    DOM_FNL <- if (is_exporter) sumnrow_rows_exp_melt(meld(Yd)) else meld(Yd)

    DOM <- DOM_INT + DOM_FNL
    BALGR <- EXGR - IMGR
    BALVA <- VAX - VAM

    # Pending
    # EXGR_DVAFX <- dmult(diagcs(dmult(Vs, Bss)), EXGR)
    # EXGR_FNLDVAFX <- dmult(diagcs(dmult(Vs, Bss)), Ysr)
    # EXGR_INTDVAFX <- dmult(diagcs(dmult(Vs, Bss)), Asj_Bjk_Ykl)

    X <- if (is_exporter) sumnrow_rows_exp_melt(meld(wio$X)) else (meld(wio$X))
    VA <- if (is_exporter) sumnrow_rows_exp_melt(meld(wio$VA)) else (meld(wio$VA))

    # End extra TiVA terms
  }


  # Output----

  if (!quiet) {cli::cli_alert_info("Preparing output ...")}

  if (output == "standard") {
    exvadec <- list(EXGR, DC, DVA, VAX, REF, DDC,
                    FC, GVC, GVCB, GVCF)
    exvadec_names <- c("EXGR", "DC", "DVA", "VAX", "REF", "DDC",
                       "FC", "GVC", "GVCB", "GVCF")

  } else if (output == "terms") {

    exvadec <- list(EXGR, DVA1, DVA2, DDC, FC)

    exvadec_names <- c("EXGR", "EXGR_DDC", "EXGR_IDC", "EXGR_RIM", "EXGR_FVA")

  } else if (output == "tiva") {

    exvadec <- list(EXGR, EXGR_FNL, EXGR_INT,
                    IMGR, IMGR_FNL, IMGR_INT,
                    DOM, DOM_FNL, DOM_INT,
                    BALGR,
                    DC, DVA1, DVA2, DDC, REF,
                    FDVA, VAD, VAX,
                    VAM, BALVA,
                    FC, GVCB, GVCF,
                    VA, X)

    exvadec_names <- c("EXGR", "EXGR_FNL", "EXGR_INT",
                       "IMGR", "IMGR_FNL", "IMGR_INT",
                       "DOM", "DOM_FNL", "DOM_INT",
                       "BALGR",
                       "EXGR_DVA", "EXGR_DDC", "EXGR_IDC", "EXGR_RIM", "REF",
                       "FD_VA", "DXD_DVA", "FFD_DVA",
                       "DFD_FVA", "BALVAFD",
                       "EXGR_FVA", "DEXFVAP", "FEXDVAP",
                       "VA", "PROD")

  }

  # Set names
  names(exvadec) <- exvadec_names

  # Additional metadata
  exvadec$dims <- wio$dims
  exvadec$names <- wio$names
  exvadec$method <- "oecd"
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
    if (exporter == "all") {
      get_exvadec_bkdown(exvadec)
    } else {
      get_exvadec_bkdown(exvadec, exporter)
    }
  }

  return(exvadec)

}
