#' extract_parquet_s3select
#'
#' Extract data from S3 throught S3 Select, transform it to nested list
#' @param ticker_list vector with tickers
#' @param bucket_name S3 Bucket name, string
#' @param file_name S3 file path location in Bucket, string
#' @param to_nested_list Transform xts series in nested list with tickers as names, boolean 
#' @param since_date Date since the data is extracted
#' @return data, in xts table or nested list of xts.
#' @export
extract_parquet_s3select <- function(ticker_list, bucket_name='condor-sura-qa', file_name = 'output/series_table/series_table.parquet', to_nested_list=TRUE, since_date='2008-01-01'){
  output_data <- NULL
  if(length(ticker_list)!=0){
    if(is.null(since_date)){
      query <- paste("SELECT \"date\",",paste0("\"",ticker_list,collapse = ",","\""),"FROM s3object s")
    }else{
      query <- paste("SELECT \"date\",",paste0("\"",ticker_list,collapse = ",","\""),"FROM s3object s WHERE \"date\" >",paste0("CAST(\'",since_date,"\'") ,"AS TIMESTAMP)")
    }
    svc = paws.storage::s3()
    InputSerialization = list('Parquet' = list(CompressionType = 'NONE'))
    OutputSerialization = list('CSV' = list(QuoteFields = "ASNEEDED"))
    resp <- svc$select_object_content(Bucket = bucket_name, Key = file_name, Expression = query, ExpressionType = 'SQL',
                                      InputSerialization = InputSerialization , OutputSerialization = OutputSerialization)
    records <- resp$Payload$Records$Payload
    df <- read.csv(text = records, header = FALSE, sep = ',')
    colnames(df) <- c('Date',ticker_list)
    df$Date <- as.Date(df$Date)
    df <- xts(df[,-1, drop = FALSE], order.by = df$Date )
    df[is.na(df)] <- ''
    storage.mode(df) <- "numeric"
    if(to_nested_list){
      output_data <- list()
      for (ticker in ticker_list) {
        df_tick <- df[,names(df)==ticker]
        df_tick <- na.omit(df_tick)
        output_data[[ticker]] <- df_tick
      }
    }else{
      output_data <- df
    }
  }
  return(output_data)
}
