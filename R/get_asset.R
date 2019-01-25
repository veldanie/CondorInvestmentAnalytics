
#' Get Asset
#'
#' Get Asset.
#' @param ticker Ticker.
#' @param asset_data Assets DF.
#' @return Asset id.
#' @export

get_asset <- function(ticker, asset_data) {
  id <- asset_data$Asset[asset_data$TickerBenchmark==ticker]
  return(id)
}
