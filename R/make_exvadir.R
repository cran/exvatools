#' Direction (detailed  origin and destination) of value added in exports
#'
#' Direction of value added in exports, i.e., details of both
#' geographical and sector origin of the VA incorporated in exports and
#' of the final destination (in gross terms or in terms of final absorption).
#' @param wio_object A `wio` object
#' @param orig_geo Geographical origin of value added (default: `"all"`)
#' @param sec_orig Code of sector of origin of value added (default: `"all"`)
#' @param exporter Country code (or country group code) of exporting country
#' @param via Code of intermediate importing country (default: `"any"`)
#' @param flow_type Gross exports (`"EXGR"`) or in terms of final demand:
#'   `"EXGRY"`, `"EXGRY_FIN"`, `"EXGRY_INT"`.
#' @param va_type VA total content (`"TC"`), domestic (`"DC"`) or foreign
#'   content (`"FC"`) or VA content excluding double counting
#'   (`"TVA"`, `"DVA"`, `"FVA"`)
#' @param intra Boolean for inclusion of intra-regional exports
#'   (default: `FALSE`)
#' @param perspective Sector perspective, `"origin"` or `"exporter"`.
#' @return Matrix with source and destination of value added in exports
#' @export
#' @examples
#' wio <- make_wio("wiodtest", quiet = TRUE)
#' # Foreign services content of value added incorporated in exports of Spain,
#' # by country of origin and final destination, expressed in gross terms
#' # (equivalent to OECD TiVA’s indicator EXGR_SERV_FVA).
#' exvadir <- make_exvadir(wio, va = "FC", flow="EXGR", exp="ESP",
#'                         sec_orig="SRVWC")
#' summary(exvadir)
make_exvadir <- function(wio_object, exporter,
                         va_type="TC", flow_type="EXGR",
                         orig_geo = "all", sec_orig = "all",
                         via="any", perspective="exporter",
                         intra=FALSE){

  # Uses: bkd, bkoffd, get_geo_codes, get_sec_codes, meld

  # Check class----
  wio <- check_object(wio_object, "wio")

  # Check icio----
  wio_type <- wio$type
  is_icio <- is.icio(wio_type)

  # Dimensions----
  G <- wio$dims$G
  N <- wio$dims$N
  GX <- wio$dims$GX
  GN <- G * N
  GXN <- GX*N

  # Names----
  gxn_names <- wio$names$gxn_names
  gn_names <- wio$names$gn_names
  gx_names <- wio$names$gx_names
  g_names <- wio$names$g_names
  n_names <- wio$names$n_names

  # Position of exporter, groups
  pgn_exp <- grep(get_geo_codes(exporter, wio_type, TRUE), gxn_names)
  pg_exp <- grep(get_geo_codes(exporter, wio_type), g_names)
  is_group <- ifelse(length(pg_exp) > 1, TRUE, FALSE)

  # Codes and position for country of origin of VA----
  if(!orig_geo=="all" & !orig_geo=="sum"){
    # Position of exporter, groups
    pgn_orig <- grep(get_geo_codes(orig_geo, wio_type, TRUE), gxn_names)
    pg_orig <- grep(get_geo_codes(orig_geo, wio_type), g_names)
  }

  # Codes/position for sector of origin of VA----
  if(!sec_orig=="all"){
    pgn_sec <- grep(get_sec_codes(sec_orig, wio_type, remove_letter = TRUE),
                    gxn_names)
  }

  # Codes/position of intermediate importer (via)----
  if(!via=="any"){
    # Codes
    pgn_imp <- grep(get_geo_codes(via, wio_type, TRUE), gxn_names)
    pg_imp <- grep(get_geo_codes(via, wio_type), g_names)
  }

  # Type of VA (VA o content) and matrix Bts----
  va_type = toupper(va_type)
  if(va_type %in% c("TCX", "DCX", "FCX", "TC", "DC", "FC")){
    Bts <- wio$B
  } else if(va_type %in% c("TVA", "DVA", "FVA")){
    # Matrix Bo for exporter
    cli::cli_alert_info("Calculating inverse matrix...")
    Ao <- wio$A
    Ao[pgn_exp, -c(pgn_exp)] <- 0
    Bo <- solve(diag(GXN) - Ao)
    Bts <- Bo
  }

  # Domestic or foreign VA----
  # bkd and bkoffd correct icio, if needed
  if(va_type %in% c("DCX", "DC", "DVA")){
    Bm <- bkoffd(Bts)
    if (intra == FALSE) {
      Bm[pgn_exp, pgn_exp] <- 0
    }
    Bd <- Bts - Bm
    Bts <- Bd
  } else if(va_type %in% c("FCX", "FC", "FVA")){
    Bm <- bkoffd(Bts)
    if (intra == FALSE) {
      Bm[pgn_exp, pgn_exp] <- 0
    }
    Bts <- Bm
  }

  # Matrix Vt_Bts----
  Vt_Bts <- dmult(wio$W, Bts)

  # Specific country of origin of VA----
  # We make the rows of countries not origin as 0 (for all sectors)
  if(!orig_geo=="all" & !orig_geo=="sum"){
    Vt_Bts[-pgn_orig, ] <- 0
  }

  # Specific sector or origin of VA----
  # We make the rows of sectors not origin as 0 (for all countries)
  if(!sec_orig=="all"){
    Vt_Bts[-pgn_sec, ] <- 0
  }


  # Perspective (origin or exporter)----
  # If perspective is "exporter" (default) we diagonalize the matrix
  # Be careful: we are diagonalizing every submatrix ij of Vt_Bts
  # The result is not a block-diagonal matrix, but a full matrix with
  # every block diagonalized
  if(perspective=="exporter"){
    Vt_Bts <- bkdiag(Vt_Bts)
  }

  # Selection of exporter column----
  # Ws (GXN x GXN) x Bts(GXN x N) = GXN x N
  # All: we take the column of the exporter of matrix B
  # Vs_Bts <- wio$W %*% Bts[,pgn_exp] #dim GXN x N
  Vt_Bts <- Vt_Bts[, pgn_exp]

  # Destination and intermediate importing country----
  # We define the matrix EXGRY depending on the
  # destination options chosen, and whether there is
  # an intermediate importer
  # We first prepare Ym and Am
  Ym <- wio$Ym
  Am <- wio$Am
  # make intra-flows zero if needed
  if(is_group==TRUE && intra==FALSE){
    Ym[pgn_exp, pg_exp] <- 0
    Am[pgn_exp, pgn_exp] <- 0
  }
  sumEXGR <- sum(wio$EXGR[pgn_exp,])


  if(via=="any"){

    #If there is no intermediate importer

    if(flow_type == "EXGR"){
      # Normal EXGR
      EXGRY <- wio$EXGR[pgn_exp,]
      if(is_group==TRUE && intra==FALSE){
        EXGRY[, pg_exp] <- 0
      }
      #
    } else if(flow_type=="EXGRY"){
      B_Y <- wio$B %*% wio$Y
      EXGRY <- (Ym + Am %*% B_Y)[pgn_exp,]
      #
    } else if(flow_type=="EXGRY_FIN"){
      EXGRY <- Ym[pgn_exp,]
      #
    } else if(flow_type=="EXGRY_INT"){
      B_Y <- wio$B %*% wio$Y
      EXGRY <- (Am %*% B_Y)[pgn_exp,]
      #
    } else if(flow_type=="FD"){
      EXGRY <- (wio$Y)[pgn_exp,]
    }

  } else if(!via=="any"){

    # If an importer is specified we take the Ym row of exporter
    # and make all columns of other importers except selected = 0
    # Then we take Ysr + A[pgn_exp, pgn_imp]BY[pgn_imp,]
    # i.e. we use importer as exporter for BY
    # E.g.. Y12 + A12*BY[2,]

    Ysr <- Ym[pgn_exp, ]
    Ysr[, -pg_imp] <- 0

    if(flow_type=="EXGR"){
      cli::cli_alert_info(c("EXGR is not compatible with intermediate importer",
                            "Taking EXGRY instead"))
      flow_type <- "EXGRY"
    }

    if(flow_type=="EXGRY"){
      B_Y <- wio$B %*% wio$Y
      EXGRY <- Ysr + Am[pgn_exp, pgn_imp] %*% B_Y[pgn_imp,]
      #
    } else if(flow_type=="EXGRY_FIN"){
      EXGRY <- Ysr
      #
    } else if(flow_type=="EXGRY_INT"){
      #
      B_Y <- wio$B %*% wio$Y
      EXGRY <- Am[pgn_exp, pgn_imp] %*% B_Y[pgn_imp,]
    }

  }

  # Calculation of result----
  # Remember it is normally GNxN x NxG, i.e., GNxGN
  result <- Vt_Bts %*% EXGRY

  # Melding and preparation of output----
  # Meld, if needed (icio)
  result <- if(is_icio) meld(result) else result

  rownames(result) <- gn_names
  colnames(result) <- g_names


  # Output----
  exvadir <- list(result)
  exvadir_names <- c(va_type)
  names(exvadir) <- exvadir_names

  exvadir$va_type <- va_type
  exvadir$exporter <- exporter
  exvadir$via <- via
  exvadir$flow_type <- flow_type
  exvadir$orig_geo <- orig_geo
  exvadir$sec_orig <- sec_orig
  exvadir$perspective <- perspective
  exvadir$intra <- intra
  exvadir$sumEXGR <- sumEXGR

  exvadir$dims <- wio$dims
  exvadir$names <- wio$names
  exvadir$source <- wio$type
  exvadir$year <- wio$year

  class(exvadir) <- "exvadir"

  # cli::cli_alert_success("Done!")

  return(exvadir)

}



