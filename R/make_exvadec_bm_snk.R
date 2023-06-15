#' Make Borin and Mancini (2023) sink-based decomposition
#'
#' Creates Borin and Mancini sink-based decomposition
#' @param wio_object A class `wio` object
#' @param exporter A string character with the code of a country
#' @param output String, type of output
#' @param quiet Boolean, quiet output
#' @keywords internal
#' @noRd
#' @return A list with matrices
make_exvadec_bm_snk <- function(wio_object, exporter = "all",
                                output = "standard", quiet = FALSE) {

  # Check class----
  wio <- check_object(wio_object, "wio")

  # Check if is all or exporter
  if (exporter == "all") {
    is_all <- TRUE
    exporter <- wio$names$g_names
    exp_quiet <- TRUE
  } else {
    is_all <- FALSE
    exp_quiet <- quiet
  }

  # Initiate list
  exvadec <- list()

  # If is all, show progress bar (unless quiet)
  if (is_all && !quiet) {
    options(cli.progress_show_after = 0)
    cli::cli_progress_bar("Calculating inverse extraction matrix...",
                          type = "iterator",
                          total = wio$dims$G)
  }

  # Iterate
  for (exp in exporter) {
    if (is_all && !quiet) cli::cli_progress_update(status = exp)
    exvadec_exp <- make_exvadec_bm_snk_exp(wio, exp, output = output,
                                           quiet = exp_quiet)
    # Get exvadec names
    exvadec_names <- names(exvadec_exp)
    # Add to exvadec list
    for (var in exvadec_names) {
      exvadec[[var]] <- rbind(exvadec[[var]], exvadec_exp[[var]])
    }
  }
  if (is_all && !quiet) cli::cli_progress_done()

  # Metadata
  exvadec$dims <- wio$dims
  exvadec$names <- wio$names
  exvadec$method <- "bm_snk"
  exvadec$output <- output
  exvadec$source <- wio$type
  exvadec$year <- wio$year
  if (!is_all){
    exvadec$exporter <- exporter
  }
  class(exvadec) <- "exvadec"

  # Show conclusion only if is_all
  if (is_all && !quiet) {cli::cli_alert_success("Done!")}

  # Print result summary
  if (!quiet) {
    if (is_all) {
      get_exvadec_bkdown(exvadec)
    } else{
      get_exvadec_bkdown(exvadec, exporter)
    }
  }

  return(exvadec)

}

#' Make Borin and Mancini (2023) sink-based decomposition for a single country
#'
#' Creates Borin and Mancini sink-based decomposition for a single country
#' @param wio_object A class `wio` object
#' @param exporter A string character with the code of a country
#' @param output String, type of output
#' @param quiet Boolean, quiet output
#' @keywords internal
#' @noRd
#' @return A list with matrices
make_exvadec_bm_snk_exp <- function(wio_object, exporter,
                                    output = "standard", quiet = FALSE) {

  # Requires functions: bkd, bkoffd, bkt, bktt, hmult,
  # meld, bkdiag, sumnrow, sumgcol
  # make_iciox, is.icio

  # wio <- make_wio("iciotest")
  # exporter <- "ESP"
  # output <- "standard"
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

  # Initialize names and number of countries in case exporter is group
  gg_names <- g_names
  GG <- G

  # Row names with sectors for exporter (valid also if group)
  expn_names <- paste0(exporter, "_", gsub("^[CD]", "", n_names))

  # Position of exporter
  pgn_exp <- grep(get_geo_codes(exporter, wio_type, TRUE), gxn_names)
  pg_exp <- grep(get_geo_codes(exporter, wio_type), g_names)

  # Check if exporter is group
  is_group <- ifelse(length(pg_exp) > 1, TRUE, FALSE)
  if (is_group) {
    group_names <- strsplit(get_geo_codes(exporter, wio$type, TRUE), "[|]")[[1]]
    gg_names <- c(g_names[!g_names %in% group_names], exporter)
    GG <- length(gg_names)
  }

  # Auxiliary functions to improve code readability
  get_block_exp <- function(df) df[pgn_exp, pgn_exp, drop = FALSE]
  get_cols_exp <- function(df) df[, pgn_exp, drop = FALSE]
  get_rows_exp <- function(df) df[pgn_exp, , drop = FALSE]
  group_cols_exp <- function(df) group_cols(df, pg_exp, "replace", exporter, wio_type)
  make_Ynots <- function(Y) set_zero(Y, pgn_exp, -pg_exp)
  sum_X <- function(df) name(repmat(rsums(df), GG), gxn_names, gg_names)
  sum_Xxs <- function(df) name(repmat(rsums(df), GG) - df, gxn_names, gg_names)
  bkt_col_bkoffd <- function(df) {
    df <- bkoffdx(df, gg_names)
    df <- df[, exporter, drop = FALSE]
    df <- bkt(df)
  }
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
  Vs <- get_block_exp(wio$W)

  # Bx (extraction)
  if (!quiet) {cli::cli_alert_info("Calculating extraction matrix of {exporter}...")}
  Bx <- make_Bnots(wio, exporter)
  Bxss <- get_block_exp(Bx)
  Bxts <- get_cols_exp(set_zero_exp(Bx))

  # Preparation
  Bm <- set_zero_exp(wio$Bm)
  Bd <- wio$B - Bm
  Am <- set_zero_exp(wio$Am)
  Ad <- wio$A - Am
  Ym <- set_zero_exp(wio$Ym)
  Yd <- wio$Y - Ym

  Bss <- get_block_exp(Bd)
  Bts <- get_cols_exp(Bm)
  Asr <- get_rows_exp(Am)
  Lrr <- if (is_group) solve(diag(GXN) - Ad) else wio$Ld
  Asr_Lrr <- Asr %*% Lrr

  Yrj <- if (is_group) group_cols_exp(Ym) else Ym

  # Basic value added matrices
  Vs_Bss <- diagcs(dmult(Vs, Bss))
  Vs_Bss_Asr_Lrr <- dmult(Vs_Bss, Asr_Lrr)
  sum_Vt_Bts <- diagcs(sumnrow(dmult(wio$W, Bts), N))
  sum_Vt_Bts_Asr_Lrr <- dmult(sum_Vt_Bts, Asr_Lrr)

  # Exports
  EXGR <- get_rows_exp(set_zero_exp(wio$EXGR))

  # VAX1, VAX2 (and FVA1, FVA2)
  Ysr <- get_rows_exp(Ym)
  Yrr <- Yd
  Yxkl <- if(is_group) group_cols_exp(make_Ynots(wio$Y)) else make_Ynots(wio$Y)
  Arj_Bxjk_Yxkl <- Am %*% (Bx %*% Yxkl)

  if (output %in% c("standard", "terms")) {
    # VAX3
    sum_Yrjxs <- bkt_col_bkoffd(sum_Xxs(Yrj))
    # VAX4
    sum_Arj_sum_Bxjk_Yxklxs <- bkt_col_bkoffd(sum_Xxs(Arj_Bxjk_Yxkl))
  }

  # REF1
  Yrs <- bkt_col_bkoffd(Yrj)
  # REF2
  sum_Arj_sum_Bxjk_Yxks <- bkt_col_bkoffd(Arj_Bxjk_Yxkl)

  if (output == "standard") {
    # VAXIM3
    # Without grouping (we remake Yxkr)
    Arj_Bxjk_Yxkr <- Am %*% (Bx %*% make_Ynots(wio$Y))
    # Here bkd, not bkdx
    Arj_Bxjk_Yxkr <- set_zero_exp(bkd(Arj_Bxjk_Yxkr))
    sum_Arj_sum_Bxjk_Yxkr <- sumnrow(Arj_Bxjk_Yxkr, N, expn_names)
    # result is melt
  }

  # DDC and # FDC
  Esr <- set_zero_exp(wio$EXGR)
  Es <- name(diag(rowSums(Esr)), gxn_names, gxn_names)
  Bx_Es <- multd(Bx, Es)

  Arj_Bxjs_Es <- bkt(rsums(Am %*% get_cols_exp(Bx_Es), exporter))

  if (output == "terms") {
    # FVA3
    sum_Yrj <- bkt_col_bkoffd(sum_X(Yrj))
    # FVA4
    sum_Arj_sum_Bxjk_Yxkl <- bkt(bkoffd(rsums(Arj_Bxjk_Yxkl, exporter)))
  }

  # DVA terms----

  if (!quiet) {cli::cli_alert_info("Calculating DVA terms...")}

  if (output %in% c("basic")) {
    DC <- sumnrow_meld(dmult(Vs_Bss, EXGR))
    Vs_Bss_Asr_Lrr <- sumnrow_meld(Vs_Bss_Asr_Lrr, meld_cols = FALSE)
    REF1 <- meld(hmult(Vs_Bss_Asr_Lrr, Yrs))
    REF2 <- meld(hmult(Vs_Bss_Asr_Lrr, sum_Arj_sum_Bxjk_Yxks))
    DDC <- sumnrow_meld(hmult(Vs_Bss_Asr_Lrr, Arj_Bxjs_Es))
    DVA <- DC - DDC
    REF <- REF1 + REF2
    VAX <- DVA - REF
  } else if (output %in% c("standard", "terms")) {
    VAX1 <- sumnrow_meld(dmult(Vs_Bss, Ysr))
    VAX2 <- sumnrow_meld(Vs_Bss_Asr_Lrr %*% Yrr)
    if (output == "standard") {
      # Additional term for VAXIM
      VAXIM3 <- meld(hmult(sumnrow_meld(Vs_Bss_Asr_Lrr), sum_Arj_sum_Bxjk_Yxkr))
      VAXIM <- VAX1 + VAX2 + VAXIM3
    }
    # Terms in VAX3, VAX4, REF1 and REF2 and DDC all have dimension
    # GN x GXN or N x GXN, so Vs_Bss_Asr_Lrr must be first melded in rows and,
    # if is_exporter and is_group, summed by sectors to obtain N rows
    # Once calculated, those terms are melded by columns
    Vs_Bss_Asr_Lrr <- sumnrow_meld(Vs_Bss_Asr_Lrr, meld_cols = FALSE)
    VAX3 <- meld(hmult(Vs_Bss_Asr_Lrr, sum_Yrjxs))
    VAX4 <- meld(hmult(Vs_Bss_Asr_Lrr, sum_Arj_sum_Bxjk_Yxklxs))
    REF1 <- meld(hmult(Vs_Bss_Asr_Lrr, Yrs))
    REF2 <- meld(hmult(Vs_Bss_Asr_Lrr, sum_Arj_sum_Bxjk_Yxks))
    DDC <- sumnrow_meld(hmult(Vs_Bss_Asr_Lrr, Arj_Bxjs_Es))
    if (output == "standard") {
      VAX <- VAX1 + VAX2 + VAX3 + VAX4
      REF <- REF1 + REF2
      DVA <- VAX + REF
      DC <- DVA + DDC
    }
  }

  # FVA terms----

  if (!quiet) {cli::cli_alert_info("Calculating FVA terms...")}

  if (output %in% c("basic", "standard")) {
    FC <- sumnrow_meld(dmult(sum_Vt_Bts, EXGR))
    # Now meld rows
    sum_Vt_Bts_Asr_Lrr <- sumnrow_meld(sum_Vt_Bts_Asr_Lrr, meld_cols = FALSE)
    FDC <- meld(hmult(sum_Vt_Bts_Asr_Lrr, Arj_Bxjs_Es))
    FVA <- FC - FDC
  } else if (output == "terms") {
    FVA1 <- sumnrow_meld(dmult(sum_Vt_Bts, Ysr))
    FVA2 <- sumnrow_meld(sum_Vt_Bts_Asr_Lrr %*% Yrr)
    # Now meld rows
    sum_Vt_Bts_Asr_Lrr <- sumnrow_meld(sum_Vt_Bts_Asr_Lrr, meld_cols = FALSE)
    FVA3 <- meld(hmult(sum_Vt_Bts_Asr_Lrr, sum_Yrj))
    FVA4 <- meld(hmult(sum_Vt_Bts_Asr_Lrr, sum_Arj_sum_Bxjk_Yxkl))
    FDC <- meld(hmult(sum_Vt_Bts_Asr_Lrr, Arj_Bxjs_Es))
  }

  EXGR <- sumnrow_meld(EXGR)

  if (!quiet) {cli::cli_alert_info("Preparing output ...")}

  # Output----

  if (output == "basic") {
    exvadec <- list(EXGR, DC, DVA, VAX, REF, DDC,
                    FC, FVA, FDC)
    exvadec_names <- c("EXGR", "DC", "DVA", "VAX", "REF", "DDC",
                       "FC", "FVA", "FDC")
  } else if(output=="standard"){
    exvadec <- list(EXGR, DC, DVA, VAX, VAXIM, REF, DDC,
                    FC, FVA, FDC)
    exvadec_names <- c("EXGR", "DC", "DVA", "VAX", "VAXIM", "REF", "DDC",
                       "FC", "FVA", "FDC")
  } else if(output=="terms"){
    exvadec <- list(EXGR, VAX1, VAX2, VAX3, VAX4,
                    REF1, REF2, DDC,
                    FVA1, FVA2, FVA3, FVA4, FDC)
    exvadec_names <- c("EXGR", "VAX1", "VAX2", "VAX3", "VAX4",
                       "REF1", "REF2", "DDC",
                       "FVA1", "FVA2", "FVA3", "FVA4", "FDC")
  }

  # Set names
  names(exvadec) <- exvadec_names

  if (!quiet) {cli::cli_alert_success("Done!")}

  return(exvadec)

}

