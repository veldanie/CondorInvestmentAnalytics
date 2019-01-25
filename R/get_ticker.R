
#' Get ticker
#'
#' Get Tiker.
#' @param asset Assetid.
#' @param asset_data Assets DF.
#' @return Ticker.
#' @export

get_ticker <- function(asset, asset_data) {
  tk <- asset_data$TickerBenchmark[asset_data$Asset==asset]
  return(tk)
}
