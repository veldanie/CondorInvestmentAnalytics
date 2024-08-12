
#' Get ticker
#'
#' Get Tiker.
#' @param asset Assetid.
#' @param asset_data Assets DF.
#' @return Ticker.
#' @export

get_ticker <- function(assets, asset_data, invest_assets = NULL, fixed_tickers=NULL) {
  pos_assets <- match(assets, asset_data$Asset)
  if (!is.null(invest_assets) && invest_assets == "ETF") {
    tk <- asset_data$TickerETF[pos_assets]
  }else if (!is.null(invest_assets) && invest_assets == "IA") {
    tk <- asset_data$TickerInvestAsset[pos_assets]
    if (!is.null(fixed_tickers)) {
      tk[match(names(fixed_tickers), assets)] <- fixed_tickers
    }
  }else{
    tk <- asset_data$TickerBenchmark[pos_assets]
  }
  return(tk)
}
