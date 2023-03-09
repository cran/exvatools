#' Make Borin and Mancini (2023) source-based decomposition
#'
#' Creates Borin and Mancini source-based decomposition
#' @param wio_object A class `wio` object
#' @param exporter A string character with the code of a country
#' @param output String, type of output
#' @param quiet Boolean, quiet output
#' @keywords internal
#' @noRd
#' @return A list with matrices
make_exvadec_bm_src <- function(wio_object, exporter = "all",
                                output = "standard", perim = "country",
                                partner = "WLD", sector = "TOTAL",
                                quiet = FALSE) {

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
    exvadec_exp <- make_exvadec_bm_src_exp(wio, exp, output = output,
                                           perim = "country",
                                           partner = partner,
                                           sector = sector,
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
  exvadec$method <- "bm_src"
  exvadec$output <- output
  exvadec$source <- wio$type
  exvadec$year <- wio$year
  if (!is_all){
    exvadec$exporter <- exporter
  }
  # Include partner and sector only if different from "WLD" and "TOTAL"
  if (!partner == "WLD") {
    exvadec$partner <- partner
  }
  if (!sector == "TOTAL") {
    exvadec$sector <- sector
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

#' Make Borin and Mancini (2023) source-based decomposition for a single country
#'
#' Creates Borin and Mancini source-based decomposition for a single country
#' @param wio_object A class `wio` object
#' @param exporter A string character with the code of a country
#' @param output String, type of output
#' @param quiet Boolean, quiet output
#' @keywords internal
#' @noRd
#' @return A list with matrices
make_exvadec_bm_src_exp <- function(wio_object, exporter = "all",
                                    output = "standard", perim = "country",
                                    partner = "WLD", sector = "TOTAL",
                                    quiet = FALSE) {

  # Requires functions: bkd, bkoffd, bkt, bktt, hmult,
  # meld, bkdiag, sumnrow, sumgcol
  # make_iciox, is.icio

  # wio <- make_wio("iciotest")
  # exporter <- "ESP"
  # output <- "standard"
  # quiet <- FALSE
  # perim <- "country"
  # partner <- "WLD"
  # sector <- "TOTAL"


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

  # Check perspective
  country_persp <- TRUE
  if (any(!partner=="WLD", !sector == "TOTAL")) {
    country_persp <- FALSE
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
  check_perim <- function(df) {
    row_names <- rownames(df)
    col_names <- colnames(df)
    #Select
    # We take exporter from n_names
    p_exp <- grep(get_geo_codes(exporter, wio_type, TRUE), row_names)
    # If NAFTA_01T03, p_exp will be empty
    if (all(is_group, length(p_exp) == 0)) {
      p_exp <- grep(exporter, row_names)
    }
    # We take sector from n_names
    if (!sector=="TOTAL") {
      p_sec <- grep(get_sec_codes(sector, wiotype = wio_type), n_names,
                    invert = TRUE)
    }
    # We take partner from col_names
    if (!partner=="WLD") {
      p_par <- grep(get_geo_codes(partner, wiotype = wio_type, TRUE), col_names,
                    invert = TRUE)
    }
    # We take sectors in partner as subgroup. E.g. if p_exp is 6,7,8 (length N
    # and p_sec is 2, 3, then p_exp_sec will be 7, 8

    if (!sector == "TOTAL") {
      p_exp_sec <- p_exp[p_sec]
      # First sector
      df[p_exp_sec, ] <- 0
    }

    if (!partner == "WLD") {
      # Then partner
      df[p_exp, p_par] <- 0
    }

    return(df)
  }

  # DVA auxiliary matrices----
  if (!quiet) {cli::cli_alert_info("Preparing DVA auxiliary matrices...")}

  # Vs
  Vs <- get_block_exp(wio$W)

  # Bx (extraction)
  Bx <- make_Bnots(wio, exporter, perim = "country",
                   partner = partner, sector = sector)
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
  Brs <- get_cols_exp(Bm)
  Asr <- get_rows_exp(Am)
  Lrr <- if (is_group) solve(diag(GXN) - Ad) else wio$Ld

  Yrj <- if (is_group) group_cols_exp(Ym) else Ym

  Ykl <- if (is_group) group_cols_exp(wio$Y) else wio$Y


  # Basic value added matrices
  Vs_Bxss <- diagcs(dmult(Vs, Bxss))
  Asr_Lrr <- Asr %*% Lrr
  Vs_Bxss_Asr_Lrr <- dmult(Vs_Bxss, Asr_Lrr)
  Vs_Bss_minus_Bxss <- dmult(Vs, Bss - Bxss)
  sum_Vt_Bxts <- diagcs(sumnrow(dmult(wio$W, Bxts), N))
  sum_Vt_Bts_minus_Bxts <- diagcs(sumnrow(dmult(wio$W, Brs - Bxts), N))
  sum_Vt_Bxts_Asr_Lrr <- dmult(sum_Vt_Bxts, Asr_Lrr)

  # Exports
  EXGR <- get_rows_exp(set_zero_exp(wio$EXGR))
  if (!country_persp) {
    EXGR <- check_perim(EXGR)
  }

  Arj_Bjk_Ykl <- Am %*% (wio$B %*% Ykl)

  if (output %in% c("standard", "terms")) {
    # VAX1, VAX2, FVA1, FVA2
    Ysr <- get_rows_exp(Ym)
    Yrr <- Yd
  }

  # This is for all (needed for VAX)
  # REF1
  Yrs <- bkt_col_bkoffd(Yrj)
  # REF2
  sum_Arj_sum_Bjk_Yks <- bkt_col_bkoffd(Arj_Bjk_Ykl)

  if (output %in% c("standard", "terms")) {
    # VAX3
    sum_Yrjxs <- bkt_col_bkoffd(sum_Xxs(Yrj))
    # VAX4
    sum_Arj_sum_Bjk_Yklxs <- bkt_col_bkoffd(sum_Xxs(Arj_Bjk_Ykl))
  }

  if (output == "terms") {
    # FVA3
    sum_Yrj <- bkt_col_bkoffd(sum_X(Yrj))
    # FVA4
    sum_Arj_sum_Bjk_Ykl <- bkt_col_bkoffd(sum_X(Arj_Bjk_Ykl))
  }

  if (!quiet) {cli::cli_alert_info("Calculating DVA terms...")}

  if (output == "basic") {

    DVA <- sumnrow_meld(dmult(Vs_Bxss, EXGR))
    Vs_Bxss_Asr_Lrr <- sumnrow_meld(Vs_Bxss_Asr_Lrr, meld_cols = FALSE)
    REF1 <- meld(hmult(Vs_Bxss_Asr_Lrr, Yrs))
    REF2 <- meld(hmult(Vs_Bxss_Asr_Lrr, sum_Arj_sum_Bjk_Yks))
    REF <- REF1 + REF2
    VAX <- DVA - REF
    DDC <- sumnrow_meld(dmult(Vs_Bss_minus_Bxss, EXGR))
    DC <- DVA + DDC

  } else if (output %in% c("standard", "terms")) {

    VAX1 <- sumnrow_meld(dmult(Vs_Bxss, Ysr))
    VAX2 <- sumnrow_meld(Vs_Bxss_Asr_Lrr %*% Yrr)
    # Terms in VAX3, VAX4, REF1 and REF2 and DDC all have dimension
    # GN x GXN or N x GXN, so Vs_Lss_Asr_Lrr must be first melded in rows and,
    # if is_exporter and is_group, summed by sectors to obtain N rows
    # Once calculated, those terms are melded by columns
    Vs_Bxss_Asr_Lrr <- sumnrow_meld(Vs_Bxss_Asr_Lrr, meld_cols = FALSE)
    VAX3 <- meld(hmult(Vs_Bxss_Asr_Lrr, sum_Yrjxs))
    VAX4 <- meld(hmult(Vs_Bxss_Asr_Lrr, sum_Arj_sum_Bjk_Yklxs))
    REF1 <- meld(hmult(Vs_Bxss_Asr_Lrr, Yrs))
    REF2 <- meld(hmult(Vs_Bxss_Asr_Lrr, sum_Arj_sum_Bjk_Yks))
    DDC <- sumnrow_meld(dmult(Vs_Bss_minus_Bxss, EXGR))
    if (output == "standard") {
      DAVAX <- VAX1 + VAX2
      VAX <- DAVAX + VAX3 + VAX4
      REF <- REF1 + REF2
      DVA <- VAX + REF
      DC <- DVA + DDC
      GVC <- sumnrow_meld(EXGR) - DAVAX
      GVCF <- DVA - DAVAX
      GVCB <- GVC - GVCF
    }

  }

  # FVA terms----
  if (!quiet) {cli::cli_alert_info("Calculating FVA terms...")}

  if (output %in% c("basic", "standard")) {

    FVA <- sumnrow_meld(dmult(sum_Vt_Bxts, EXGR))
    FDC <- sumnrow_meld(dmult(sum_Vt_Bts_minus_Bxts, EXGR))
    FC <- FVA + FDC

  } else if (output == "terms") {

    FVA1 <- sumnrow_meld(dmult(sum_Vt_Bxts, Ysr))
    FVA2 <- sumnrow_meld(sum_Vt_Bxts_Asr_Lrr %*% Yrr)
    # FVA3 and FVA4 terms have dimension GN x GXN or KN x GXN
    # so sum_Vt_Btsnots_Asr_Lrr must be previously melded by rows
    sum_Vt_Bxts_Asr_Lrr <- sumnrow_meld(sum_Vt_Bxts_Asr_Lrr,
                                        meld_cols = FALSE)
    FVA3 <- meld(hmult(sum_Vt_Bxts_Asr_Lrr, sum_Yrj))
    FVA4 <- meld(hmult(sum_Vt_Bxts_Asr_Lrr, sum_Arj_sum_Bjk_Ykl))
    FDC <- sumnrow_meld(dmult(sum_Vt_Bts_minus_Bxts, EXGR))

  }

  EXGR <- sumnrow_meld(EXGR)

  # Output----
  if (!quiet) {cli::cli_alert_info("Preparing output ...")}

  if (output == "basic") {
    exvadec <- list(EXGR, DC, DVA, VAX, REF, DDC,
                    FC, FVA, FDC)
    exvadec_names <- c("EXGR", "DC", "DVA", "VAX", "REF", "DDC",
                       "FC", "FVA", "FDC")
  } else if(output == "standard") {
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

  # If not pure country perspective
  if (!country_persp) {
    exvadec <- lapply(exvadec, check_perim)
  }

  if (!quiet) {cli::cli_alert_success("Done!")}

  return(exvadec)

}

