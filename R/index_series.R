
#' Index Series
#'
#' Estimates portfolio mean return, variance and annualized volatility.
#' @param series_list Portfolio weights.
#' @param weights named vector of weights.
#' @param dates Dates.
#' @param val_ini Initial value.
#' @param ref_curr New index reference currency
#' @param invest_assets Index, ETF.
#' @param anual_cost Anual total return cost.
#' @return index series
#' @export

index_series <- function(series_list, weights, dates, val_ini = 100, ref_curr = "USD", invest_assets = NULL, anual_cost = 0){
  series_sync <- series_merge(series_list = series_list, dates = dates, asset_data = asset_data, ref_curr = ref_curr, assets = names(weights), currencies = NULL,
                              convert_to_ref = TRUE, invest_assets = invest_assets)
  if(length(series_sync)>1){
    rets <- returns(series_sync)
    cost_factor <- (1 - anual_cost)^(1/252)
    cum_rets <- c(1, cumprod((1 + as.numeric(rets %*% weights[names(series_sync)]))*cost_factor))
    index <- xts(val_ini * cum_rets, order.by = c(index(series_sync)[1], index(rets)))
  }else{
    index <- NULL
  }
  return(index)
}
