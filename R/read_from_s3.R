
#' Read file from s3.
#'
#' read file from s3
#' @param filename Filenam str.
#' @param bucket Bucket
#' @param sep Sep
#' @return dataframe object.
#' @export

read_from_s3 <- function(filename, bucket, sep =","){
  df <- s3read_using(FUN=read.csv, bucket = bucket, object=filename, sep = sep, header=TRUE, check.names=FALSE)
  return(df)
}
