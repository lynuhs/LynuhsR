#' A string function to remove whitespace
#'
#' This function will remove all whitespace before and after a string
#' @param str The string that should have it's whitespace removed
#' @export
#' @examples
#' strTrim(str)

strTrim <- function(str){
  gsub("^\\s+|\\s+$", "", str)
  return(str)
}
