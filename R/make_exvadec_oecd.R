#' Make OECD (2019) decomposition
#'
#'#' Calculates the decomposition of value added in exports of a country
#' or a group of countries according to the OECD methodology.
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
#'   `original`: Original denomination of terms.
#' @keywords internal
#' @noRd
#' @return A list object of class `exvadec` with several matrices
#' plus metadata.
make_exvadec_oecd <- function(wio_object, exporter = "all",
                              output = "standard", quiet = FALSE) {

  # exporter <- "EU27"
  # output <- "standard"

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
    expn_names <- paste0(exporter, "_", gsub("[CD]", "", n_names))
    # If is exporter group, ggn_names will be EU27_01T02...EU27_90T98
    if (is_group) {
      ggn_names <- paste0(exporter, "_", gsub("[CD]", "", n_names))
    }
  } else {
    is_group <- FALSE
  }

  # Simple auxiliary matrices----

  if (!quiet) {cli::cli_alert_info("Preparing simple auxiliary matrices...")}

  # Vs
  Vs <- Vt <- wio$W
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

  # EXGR
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

  if (!quiet) {cli::cli_alert_info("Calculating DVA terms...")}

  if (is_all) {
    VAX <- meld(bkoffd(Vs_Bsj_Yjk))
    VAD <- meld(bkd(Vs_Bsj_Yjk))
  } else if (is_exporter) {
    VAX <- set_zero(bkoffd(Vs_Bsj_Yjk), pgn_exp, pg_exp)
    VAD <- check_meld_group((Vs_Bsj_Yjk - VAX)[pgn_exp, , drop = FALSE])
    VAX <- check_meld_group((VAX)[pgn_exp, , drop = FALSE])
  }

  # Total VA of Fnal Demand
  FDVA <- VAD + VAX

  DC <- check_meld_group(dmult(Vs_Bss, EXGR))

  # DVA direct
  DVA1 <- check_meld_group(dmult(Vs_diag_Lss, EXGR))

  # DVA indirect
  DVA2 <- check_meld_group(dmult(Vs_offdiag_Lss, EXGR))

  # DVA reimported
  DDC <- check_meld_group(dmult(Vs_Bss_minus_Lss, EXGR))


  # FVA terms----

  if (!quiet) {cli::cli_alert_info("Calculating FVA terms...")}

  # FC
  Vt_Bts <- diagcs(dmult(wio$W, Bts))

  FC <- check_meld_group(dmult(Vt_Bts, EXGR))

  # Export matrices for GVC indicators
  Esr <- wio$EXGR
  if (is_exporter) {
    Esr <- set_zero(Esr, pgn_exp, pg_exp)
  }
  Esr <- bkdiag(bkoffd(name(repmat(rowSums(Esr), GX), gxn_names, gx_names)))
  Ers <- bkt(Esr)
  if (is_exporter) {
    Esr <- Esr[pgn_exp, , drop = FALSE]
    Ers <- Ers[pgn_exp, , drop = FALSE]
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
  GVCB <- check_meld_group(sumgcols(GVCB, N, gx_names))

  # GVCB <- check_meld_group(name(sum_by_groups_of(GVCB, N, bycols = TRUE),
  #                               rownames(Esr), gx_names))

  # GVCF (forward)
  # Rows now are 0, V1_B12, V1_B13...
  # and multiply by 0, E2, E3...
  # (  0      V1B12E2   V1B13E3 )
  # (V2B21E1     0      V2V23E3 )
  # (V3B31E1  V3B32E2      0    )
  Vs_Bsr <- dmult(Vs, Bsr)

  GVCF <- bktt(hmult(Vs_Bsr, Ers))
  GVCF <- check_meld_group(sumgcols(GVCF, N, gx_names))

  EXGR <- check_meld_group(EXGR)


  DVA <- DVA1 + DVA2
  REF <- DVA - VAX
  GVC <- GVCB + GVCF

  # Extra TIVA terms----

  if (output == "tiva") {

    if (!quiet) {cli::cli_alert_info("Calculating extra TiVA terms...")}

    if (is_all) {
      IMGR <- bkt(EXGR)
    } else if (is_exporter) {
      pgn_exp_melt <- grep(get_geo_codes(exporter, wio_type), gn_names)
      IMGR <- bkt(meld(set_zero(wio$EXGR, pgn_exp, pg_exp)))[pgn_exp_melt,
                                                             , drop = FALSE]
      if (is_group) {
        IMGR <- sumnrow(IMGR, N, ggn_names)
      }
    }

    if (is_all) {
      VAM <- bkt(meld(bkoffd(Vs_Bsj_Yjk)))
    } else if (is_exporter) {
      VAM <- bkt(meld(set_zero(bkoffd(Vs_Bsj_Yjk), pgn_exp, pg_exp)))
      VAM <- VAM[pgn_exp_melt, , drop = FALSE]
      if (is_group) {
        VAM <- sumnrow(VAM, N, ggn_names)
      }
    }

    Z <- wio$Z
    if (is_all) {
      Zd <- wio$Zd
      Zm <- wio$Zm
    } else if (is_exporter) {
      Zm <- set_zero(wio$Zm, pgn_exp, pgn_exp)
      Zd <- wio$Z - Zm
    }

    EXGR_INT <- meld(sumgcols(Zm, N, gx_names))

    IMGR_INT <- bkt(EXGR_INT)

    DOM_INT <- meld(sumgcols(Zd, N, gx_names))

    # DOM_INT <- meld(name(sum_by_groups_of(Zd, N, bycols = TRUE),
    #                      gxn_names, gx_names))
    if (is_exporter) {
      EXGR_INT <- EXGR_INT[pgn_exp_melt, , drop = FALSE]
      IMGR_INT <- IMGR_INT[pgn_exp_melt, , drop = FALSE]
      DOM_INT <- DOM_INT[pgn_exp_melt, , drop = FALSE]
      if (is_group) {
        EXGR_INT <- sumnrow(EXGR_INT, N, ggn_names)
        IMGR_INT <- sumnrow(IMGR_INT, N, ggn_names)
        DOM_INT <- sumnrow(DOM_INT, N, ggn_names)
      }
    }

    Ym <- wio$Ym
    Yd <- wio$Yd
    if (is_exporter) {
      Ym <- set_zero(wio$Ym, pgn_exp, pg_exp)
      Yd <- wio$Y - Ym
    }

    EXGR_FNL <- meld(Ym)
    IMGR_FNL <- meld(bkt(Ym))
    DOM_FNL <- meld(Yd)

    if (is_exporter) {
      EXGR_FNL <- EXGR_FNL[pgn_exp_melt, , drop = FALSE]
      IMGR_FNL <- IMGR_FNL[pgn_exp_melt, , drop = FALSE]
      DOM_FNL <- DOM_FNL[pgn_exp_melt, , drop = FALSE]
      if (is_group) {
        EXGR_FNL <- sumnrow(EXGR_FNL, N, ggn_names)
        IMGR_FNL <- sumnrow(IMGR_FNL, N, ggn_names)
        DOM_FNL <- sumnrow(DOM_FNL, N, ggn_names)
      }
    }

    DOM <- DOM_INT + DOM_FNL

    BALGR <- EXGR - IMGR
    BALVA <- VAX - VAM

    # Pending
    # EXGR_DVAFX <- dmult(diagcs(dmult(Vs, Bss)), EXGR)
    # EXGR_FNLDVAFX <- dmult(diagcs(dmult(Vs, Bss)), Ysr)
    # EXGR_INTDVAFX <- dmult(diagcs(dmult(Vs, Bss)), Asj_Bjk_Ykl)


    X <- meld(wio$X)
    VA <- meld(wio$VA)
    if (is_exporter) {
      X <- X[pgn_exp_melt, , drop = FALSE]
      VA <- VA[pgn_exp_melt, , drop = FALSE]
      if (is_group) {
        X <- sumnrow(X, N, ggn_names)
        VA <- sumnrow(VA, N, ggn_names)
      }
    }

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


  # End function

}

