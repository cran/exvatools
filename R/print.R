#' Print method for `wio` class
#'
#' @param x An object of class `wio`
#' @param ... Additional arguments
#' @return Printout to console
#' @export
print.wio <- function(x, ...) {

  print_object(x)

}


#' Print method for `exvadec` class
#'
#' @param x An object of class `exvadec`
#' @param ... Additional arguments
#' @return Printout to console
#' @export
print.exvadec <- function(x, ...) {

  print_object(x)

}


#' Print method for `exvadir` class
#'
#' @param x An object of class `exvadir`
#' @param ... Additional arguments
#' @return Printout to console
#' @export
print.exvadir <- function(x, ...) {

  print_object(x)

}


#' Print object
#'
#' Common print generic method for `exvatools` objects.
#' @param exvatools_object An object of one of the `exvatools` classes.
#' @keywords internal
#' @noRd
#' @return Printout to console
print_object <- function(exvatools_object) {
  for (i in seq_along(exvatools_object)) {
    if (is.matrix(exvatools_object[[i]])) {
      namevar <- names(exvatools_object[i])
      r <- nrow(exvatools_object[[i]])
      c <- ncol(exvatools_object[[i]])
      dimns <- paste0("(", r, " x ", c, ")")
      txt_desc <- paste0("", namevar, ": matrix num ", dimns)
      rdif <- paste0("...", r - 5, " more")
      cdif <- paste0(c - 5, " more")
      if (cdif > 1) {
        var <- round(as.data.frame(exvatools_object[[i]][1:5, 1:5]))
        # var <- round(var[1:5, 1:5], 2)
        var[[cdif]] <- rep("...", 5)
      } else {
        var <- round(exvatools_object[[i]][1:5, 1], 2)
        var <- as.data.frame(as.matrix(var))
        colnames(var) <- namevar
      }
      txt_mat <- paste0(utils::capture.output(print(var)), collapse = "\n")
      cat(txt_desc, "\n")
      cat(txt_mat, "\n")
      cat(rdif, "\n\n")
    } else {
      var <- exvatools_object[[i]]
      namevar <- names(exvatools_object[i])
      if (length(var) == 1) {
        txt_desc <- paste0(namevar, ": ", var)
        cat(txt_desc, "\n\n")
      } else {
        for (v in seq_along(var)) {
          namesubvar <- names(var[v])
          len <- length(var[[v]])
          if (len > 1) {
            var[[v]] <- var[[v]][1:5]
            var[[v]][1] <- paste0(namevar, "$", namesubvar, ": ", var[[v]][1])
            var[[v]] <- c(var[[v]], paste0("...", len - 5, " more"))
          } else {
            var[[v]] <- paste0(namevar, "$", namesubvar, ": ", var[[v]])
          }
          cat(vecwrap(var[[v]]), "\n\n")
        }
      }
    }
  }
  cat("class:", class(exvatools_object), "\n")
}
