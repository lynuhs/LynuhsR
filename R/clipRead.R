#' A function to read data from the clipboard
#'
#' This function will copy data from the clipboard
#' @param data The data that should be copied to the clipboard
#' @param format The format of the output, defaults to character
#' @export header If the copied data has a header row for dataFrame, defaults to TRUE
#' @examples
#' clip.read(data, format=c("character","dataFrame"), header=TRUE)

clip.read <- function(format = "character", header = TRUE){
  if(format == "character"){
    data <- readClipboard()
  } else if (format == "dataFrame"){
    data <- readClipboard()
    data <- strsplit(data, "\t")
    start <- ifelse(header, 2, 1)

    df <- NULL
    for (i in start:(length(data))){
      df <- rbind(df, data.frame(matrix(data[[i]], nrow=1)))
    }

    if(header){ colnames(df) <- data[[1]] }
    data <- factorsToCharacter(data)
  } else {
    stop("Incorrect format chosen!")
  }

  return (data)
}
