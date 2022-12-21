#' Meld ICIO-type matrix (consolidating China and Mexico sub-components)
#'
#' @description Meld `ICIO` matrix with extended countries.
#' Melds countries `CHN` and `MEX` from their extended versions
#' e.g., `CN1` and `CN2` are melded into `CHN`.
#' @param df A block matrix.
#' @param meld_rows Boolean, true to meld rows.
#' @param meld_cols Boolean, true to meld cols.
#' @return Melded version of `ICIO` matrix.
#' @author Enrique Feas
#' @export
meld <- function(df, meld_rows=TRUE, meld_cols=TRUE){

  dfinfo <- get_df_info(df)

  c1 <- dfinfo$c1
  s1 <- dfinfo$s1
  c2 <- dfinfo$c2
  s2 <- dfinfo$s2

  # Rows
  if (meld_rows) {
    # Extract positions. Those that do not exist
    # will be NULL with length() zero
    # Rows
    prMEX <- dfinfo$prMEX
    prMXall <- dfinfo$prMXall
    prCHN <- dfinfo$prCHN
    prCNall <- dfinfo$prCNall

    # MEX
    if (all(length(prMEX) > 0, length(prMXall) > 0)) {
      if (!sum(df[prMEX, ]) == 0) {
        print("Row MEX is not empty")
      }
      if (length(prMEX) == 1) {
        df[prMEX, ] <- csums(df[prMXall, , drop = FALSE])
      } else if (length(prMEX) > 1) {
        df[prMEX, ] <- sumnrow(df[prMXall, , drop = FALSE], s1)
      }
    } else {
      # Assign NULL instead of current integer(0)
      prMXall <- NULL
    }

    # CHN
    if (all(length(prCHN) > 0, length(prCNall) > 0)) {
      if (!sum(df[prCHN, ]) == 0) {
        print("Row CHN is not empty")
      }
      if (length(prCHN) == 1) {
        df[prCHN, ] <- csums(df[prCNall, , drop = FALSE])
      } else if (length(prCHN) > 1) {
        df[prCHN, ] <- sumnrow(df[prCNall, , drop = FALSE], s1)
      }
    } else {
      # Assign NULL instead of integer(0)
      prCNall <- NULL
    }

    # If there is at least one, df[-c(num, NULL)] will work, but if
    # both are null, df[-c(NULL)] will give error
    # In this case, do not attemp to meld
    if (all(is.null(prMXall), is.null(prCNall))) {
      meld_rows <- FALSE
    }

  }

  # Columns
  if (meld_cols) {

    # Columns
    pcMEX <- dfinfo$pcMEX
    pcMXall <- dfinfo$pcMXall
    pcCHN <- dfinfo$pcCHN
    pcCNall <- dfinfo$pcCNall

    # MEX
    if (all(length(pcMEX) > 0, length(pcMXall) > 0)) {
      if (!sum(df[, pcMEX]) == 0) {
        print("Row MEX is not empty")
      }
      if (length(pcMEX) == 1) {
        df[, pcMEX] <- rsums(df[, pcMXall, drop = FALSE])
      } else if (length(pcMEX) > 1) {
        df[, pcMEX] <- sumncol(df[, pcMXall, drop = FALSE], s2)
      }
    } else {
      # Assign NULL instead of integer(0)
      pcMXall <- NULL
    }

    # CHN
    if (all(length(pcCHN) > 0, length(pcCNall) > 0)) {
      if (!sum(df[, pcCHN]) == 0) {
        print("Row CHN is not empty")
      }
      if (length(pcCHN) == 1) {
        df[, pcCHN] <- rsums(df[, pcCNall, drop = FALSE])
      } else if (length(pcCHN) > 1) {
        df[, pcCHN] <- sumncol(df[, pcCNall, drop = FALSE], s2)
      }
    } else {
      # Assign NULL instead of integer(0)
      pcCNall <- NULL
    }

    # If there exists one, df[-c(num, NULL)] will work, but if
    # both are null, df[-c(NULL)] will give error
    # In this case, do not attemp to meld
    if (all(is.null(pcMXall), is.null(pcCNall))) {
      meld_cols <- FALSE
    }

  }

  # Remove matrix rows
  if (meld_rows == TRUE) {
    df <- df[-c(prMXall, prCNall), , drop = FALSE]
  }

  # Remove matrix columns
  if (meld_cols == TRUE) {
    df <- df[, -c(pcMXall, pcCNall), drop = FALSE]
  }

  # Output
  return(df)

}
