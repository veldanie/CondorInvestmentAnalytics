
#' writeMultipleData
#'
#' Writes multiple dataframes to a single .xslx file
#' @param file "filename.xlsx"
#' @param ... dataframes

xlsx.writeMultipleData <- function (file, sep, ...)
{
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    if (i == 1){
      write.xlsx(objects[[i]], file, sheetName = objnames[i], sep = sep)
    }else{
      write.xlsx(objects[[i]], file, sheetName = objnames[i],
                 append = TRUE, sep = sep) 
    }
  }
}