#' A convertion function
#'
#' This function will change a data frame's columns or a vector from factor to character
#' @param data The data frame or factor vector that should be converted
#' @param levels The maximum number of levels in the factor to perform the conversion. Will ignore number of levels if left blank.
#' @export
#' @examples
#' factorToCharacter(data, levels = 0)

factorToCharacter <- function(data, levels = 0){
  if(is.data.frame(data)){
    for(i in 1:ncol(data)){
      if(is.factor(data[,i])){
        if(length(levels(data[,i])) <= levels | levels == 0){
          data[,i] <- as.character(data[,i])
        }
      }
    }
  } else if(is.factor(data)){
    if(length(levels(data)) <= levels | levels == 0){
      data <- as.character(data)
    }
  }

  return(data)
}
