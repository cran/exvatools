#' vecwrap
#'
#' Wraps vector of text in lines of 80 (or other limit) characters,
#' merging first all elements with comma and then each line with \\n,
#' so it can be used by cat (strwrap of base R does not allow it)
#' @param txt A text string
#' @param nchars Number of characters per line (default = 80)
#' @keywords internal
#' @noRd
#'
#' @return Text string (1 string only)
vecwrap <- function(txt, nchars = 70) {

  txt <- paste(strwrap(paste0(txt, collapse=", "), nchars),
               collapse = "\n")
  return(txt)

}


#' spc
#' Insert spaces of width 'width'
#'
#' @param num_spaces Number of spaces
#' @param width Width (in characters) of each space
#'
#' @return Spaces
#' @keywords internal
#' @noRd
spc <- function(num_spaces, width = 1){
  res <- strrep(" ", width * num_spaces)
  return(res)
}


#' formatnumber
#' Format a number with thousands mark and decimal
#'
#' @param value A number
#' @param num_decimals Number of decimals to show
#'
#' @return String
#' @keywords internal
#' @noRd
formatnumber <- function(value, num_decimals){
  res <- formatC(value, digits = num_decimals, format="f",
                 big.mark=",", decimal.mark = ".")
  return(res)
}


#' hline
#' Inserts horizontal line (of equal sign)
#'
#' @param num_chars Integer number of symbols (default = 70)
#'
#' @return String line
#' @keywords internal
#' @noRd
hline <- function(num_chars = 70){
  return(strrep("=", num_chars))
}


#' Center text among spaces
#'
#' @param txt String
#' @param limchars Integer, maximum number of characters in line
#'
#' @return Character string
#' @keywords internal
#' @noRd
center <- function(txt, limchars = 70) {
  num_spc <- round((limchars - nchar(txt))/2, 0)
  new_txt <- paste0(strrep(" ", num_spc), txt)
  return(new_txt)
}


