
#' Get Asset
#'
#' Get Asset.
#' @param ticker Ticker.
#' @param asset_data Assets DF.
#' @return Asset id.
#' @export

get_asset <- function(ticker, asset_data, index_df=NULL) {
  if (!is.null(index_df)){
    tk_ind <- ticker %in% index_df$Ticker
    if(tk_ind){
      id <- index_df$IndexId[match(ticker,index_df$Ticker)]
    }else{
      id <- asset_data$Asset[asset_data$TickerBenchmark==ticker][1]
    }
  }else{
    id <- asset_data$Asset[asset_data$TickerBenchmark==ticker][1]
  }
  return(id)
}
