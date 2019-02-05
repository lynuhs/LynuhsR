#' A convertion function
#'
#' This function will change a data frame's columns from factor to character
#' @param data The data frame or factor vector that should be converted
#' @export
#' @examples
#' factorToCharacter(data)

factorToCharacter <- function(data){
  if(is.data.frame((data))){
    for (i in 1:ncol(data)){
      if(is.factor(data[,i])){
        data[,i] <- as.character(data[,i])
      }
    }
  } else if (is.factor(data)){
    data <- as.character(data)
  } else if (!is.character(data)){
    stop("Data must be a data frame or factor vector!")
  }

  return(data)
}
