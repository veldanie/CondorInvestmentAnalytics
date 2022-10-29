#' extract_parquet_fxfwd
#'
#' Extract data from S3, transform it to nested list, when it has to have two columns by fx.
#' @param bucket_name S3 Bucket name, string
#' @param file_name S3 file path location in Bucket, string
#' @param to_nested_list Transform xts series in nested list with fx as names, boolean 
#' @param colnames_fx vector with two columns names for fx. The sep = '_' 
#' @return data, in xts table or nested list of xts.
#' @export

extract_parquet_fxfwd <- function(bucket_name='suraraven', file_name = 'output/series_fxfwd/series_fxfwd.parquet', to_nested_list=TRUE, colnames_fx = c('outright','premium')){
  fx_series_df <- s3read_using(FUN=read_parquet, bucket = bucket_name, object=file_name, sep = sep, header=TRUE, check.names=FALSE)
  #fx_series_df <-read_parquet(paste0('s3://',bucket_name,'/',file_name))
  fx_series_xts <- xts(fx_series_df[,setdiff(colnames(fx_series_df), "Date")], order.by = as.Date(fx_series_df$Date))
  if (to_nested_list){
    series_fxfwd_list <-list()
    fx_series_colnames <- colnames(fx_series_xts)
    fx_fwd <- unique(substring(fx_series_colnames, first = 1, last = 6))
    fx_fwd <- gsub('_', '', fx_fwd)
    for(id in fx_fwd){
      id_columns <- fx_series_colnames[grep(id, fx_series_colnames)]
      fx_series_xts_temp <- na.omit(fx_series_xts[,id_columns])
      names(fx_series_xts_temp) <- colnames_fx
      series_fxfwd_list[[id]] <- fx_series_xts_temp
      output_data <- series_fxfwd_list
    }
  }else{
    output_data <- fx_series_xts
  }
  return(output_data)
}
