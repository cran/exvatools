#' Make Miroudot and Ye (2021) source-based decomposition
#'
#' Creates Miroudot and Ye (2021) source-based decomposition
#' @param wio_object A class `wio` object
#' @param exporter A string character with the code of a country
#' @param output String, type of output
#' @param quiet Boolean, quiet output
#' @keywords internal
#' @noRd
#' @return A list with matrices
make_exvadec_my <- function(wio_object, exporter = "all",
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
    exvadec_exp <- make_exvadec_my_exp(wio, exp, output = output,
                                       perim = perim,
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


  # We have to recover output, as it is not given out of the function
  if (all(perim == "WLD", output %in% c("terms", "terms2"))){
    output <- "terms2"
  }

  # Metadata
  exvadec$dims <- wio$dims
  exvadec$names <- wio$names
  exvadec$method <- "my"
  exvadec$output <- output
  exvadec$source <- wio$type
  exvadec$year <- wio$year
  if (!is_all){
    exvadec$exporter <- exporter
  }

  exvadec$perim <- perim

  # Include partner and sector only if different from
  # default, otherwise get_exvadec_bkdown() does not work
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

#' Make Miroudot and Ye (2021) source-based decomposition for a single country
#'
#' Creates Miroudot and Ye source-based decomposition for a single country
#' @param wio_object A class `wio` object
#' @param exporter A string character with the code of a country
#' @param output String, type of output
#' @param quiet Boolean, quiet output
#' @keywords internal
#' @noRd
#' @return A list with matrices
make_exvadec_my_exp <- function(wio_object, exporter,
                                output = "standard", perim = "country",
                                partner = "WLD", sector = "TOTAL",
                                quiet = FALSE) {

  # Requires functions: bkd, bkoffd, bkt, bktt,
  # hmult, meld, bkdiag, sumnrow, sumgcol
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

  # Auxiliary matrices----
  if (!quiet) {cli::cli_alert_info("Preparing auxiliary matrices...")}

  # Vs
  Vs <- get_block_exp(wio$W)

  # A
  Ao <- make_Anots(wio, exporter, perim, partner, sector)
  AI <- wio$A - Ao

  # Bo
  B <- wio$B
  Bo <- make_Bnots(wio, exporter, perim = perim,
                   partner= partner, sector = sector)
  # The Miroudot and Ye's formulation is
  # BoAIB <- Bo %*% AI %*% B
  # but we will use the equivalent
  BoAIB <- B - Bo
  # If perimeter is WLD and output is terms (or directly terms2)
  # we get extra decomposition FDC1 y FDC2
  if (all(perim == "WLD", output %in% c("terms", "terms2"))) {
    BoAIBo <- Bo %*% AI %*% Bo
    BoAIBminus_Bo <- Bo %*% AI %*% (B - Bo)
  }

  Boss <- get_block_exp(Bo)
  BoAIBss <- get_block_exp(BoAIB)
  Bss <- get_block_exp(wio$B)

  if (!quiet) {cli::cli_alert_info("Calculating terms ...")}

  Vs_Boss <- diagcs(dmult(Vs, Boss))
  Vs_BoAIBss <- diagcs(dmult(Vs, BoAIBss))

  Bots <- get_cols_exp(set_zero_exp(bkoffd(Bo)))

  BoAIBts <- get_cols_exp(set_zero_exp(bkoffd(BoAIB)))

  sum_Vt_Bots <- diagcs(dmult(wio$W, Bots))
  sum_Vt_BoAIBts <- diagcs(dmult(wio$W, BoAIBts))


  if (all(perim == "WLD", output %in% c("terms", "terms2"))) {

    BoAIBots <- get_cols_exp(set_zero_exp(bkoffd(BoAIBo)))

    BoAIBminus_Bots <- get_cols_exp(set_zero_exp(bkoffd(BoAIBminus_Bo)))

    sum_Vt_BoAIBots <- diagcs(dmult(wio$W, BoAIBots))
    sum_Vt_BoAIBminus_Bots <- diagcs(dmult(wio$W, BoAIBminus_Bots))

  }

  EXGR <- get_rows_exp(set_zero_exp(wio$EXGR))
  if (!country_persp) {
    EXGR <- check_perim(EXGR)
  }

  DVA <- sumnrow_meld(Vs_Boss %*% EXGR)
  DDC <- sumnrow_meld(Vs_BoAIBss %*% EXGR)
  FVA <- sumnrow_meld(sum_Vt_Bots %*% EXGR)
  FDC <- sumnrow_meld(sum_Vt_BoAIBts %*% EXGR)

  DC <- DVA + DDC
  FC <- FVA + FDC

  if (all(perim == "WLD", output %in% c("terms", "terms2"))) {
    output <- "terms2"
    FDC1 <- sumnrow_meld(sum_Vt_BoAIBots %*% EXGR)
    FDC2 <- sumnrow_meld(sum_Vt_BoAIBminus_Bots %*% EXGR)
  }

  EXGR <- sumnrow_meld(EXGR)


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

  # If not pure country perspective
  if (!country_persp) {
    exvadec <- lapply(exvadec, check_perim)
  }

  if (!quiet) {cli::cli_alert_success("Done!")}

  return(exvadec)

}

