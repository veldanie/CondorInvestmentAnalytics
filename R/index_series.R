
#' Index Series
#'
#' Estimates portfolio mean return, variance and annualized volatility.
#' @param series_list Portfolio weights.
#' @param weights named vector of weights.
#' @param dates Dates.
#' @param val_ini Initial value.
#' @param invest_assets Index, ETF.
#' @return index series
#' @export

index_series <- function(series_list, weights, dates, val_ini = 100, invest_assets = NULL){
  series_sync <- series_merge(series_list = series_list, dates = dates, asset_data = asset_data, ref_curr = ref_curr, assets = names(weights), invest_assets = invest_assets)
  rets <- returns(series_sync)
  index <- xts(val_ini * c(1, cumprod(1 + as.numeric(rets %*% weights[names(series_sync)]))), order.by = c(index(series_sync)[1], index(rets)))
  return(index)
}
