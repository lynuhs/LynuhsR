#' A function to copy data to the clipboard
#'
#' This function will copy data to the clipboard and strucutre it in a way that works for Excel
#' @param data The data that should be copied to the clipboard
#' @param sep The separator which should be used in the copied output, defaults to "\t"
#' @param row.names Decides if the output should keep row names or not, defaults to FALSE
#' @param col.names Decides if the output should keep column names or not, defaults to TRUE
#' @export
#' @examples
#' clip.write(data, sep=NULL, row.names=FALSE, col.names=TRUE)

clip.write <- function(data, sep = NULL, row.names = FALSE, col.names = TRUE){
  if(is.null(sep)){
    sep = "\t"
  }
  if(is.data.frame(data)){
    write.table(data, "clipboard-16384", sep=sep, row.names=row.names, col.names=col.names)
  } else {
    tryCatch({
      writeClipboard(as.character(data))
    }, error = function(err){
      stop("argument must be a character vector or a raw vector")
    })
    
  }
}
