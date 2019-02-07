#' A function to read data from the clipboard
#'
#' This function will copy data from the clipboard
#' @param data The data that should be copied to the clipboard
#' @param type The type of the output, defaults to auto
#' @param header If the copied data has a header row for dataFrame, defaults to TRUE
#' @export
#' @examples
#' clip.read(data, type=c("auto","character","dataFrame"), header=TRUE)

clip.read <- function(type = "auto", header = TRUE){
  data <- readClipboard()

  if(type == "auto"){
    type <- "character"
    if(regexpr("\t", data[1]) > 0){
      cols <- strsplit(data, "\t")
      if (all(lengths(cols) == length(cols[[1]]))){
        type <- "dataFrame"
      }
    }
  }

  if (type == "dataFrame"){
    data <- strsplit(data, "\t")
    start <- ifelse(header, 2, 1)

    df <- NULL
    for (i in start:(length(data))){
      df <- rbind(df, data.frame(matrix(data[[i]], nrow=1)))
    }

    if(header){ colnames(df) <- data[[1]] }
    data <- factorToCharacter(df)
  } else if (type != "character"){
    stop("Incorrect type chosen!")
  }

  data <- type.convert(data)

  return (data)
}
