#' read_csv_s3
#'
#' Extract csv file from S3
#' @param bucket_name S3 Bucket name, string
#' @param file_name S3 file path location in Bucket, string
#' @param sep CSV columns sep, string
#' @return data.frame.
#' @export

read_csv_s3 <- function(bucket_name='condor-sura-qa', file_name = 'inputs/static/asset_data.csv', sep = ','){
  svc = paws.storage::s3()
  s3_download <- svc$get_object(
    Bucket = bucket_name,
    Key = file_name
  )
  output_data <- s3_download$Body %>% rawToChar %>% read.csv(text = .,sep = sep)
  return(output_data)
}
