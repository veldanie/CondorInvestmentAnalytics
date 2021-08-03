
#' Index Series
#'
#' Estimates portfolio mean return, variance and annualized volatility.
#' @param series_list Portfolio weights.
#' @param weights named vector of weights.
#' @param dates Dates.
#' @param rebal Rebal period.
#' @param val_ini Initial value.
#' @param ref_curr New index reference currency
#' @param invest_assets Index, ETF.
#' @param anual_cost Anual total return cost.
#' @return index series
#' @export

index_series <- function(series_list, weights, dates, rebal='monthly', val_ini = 100, ref_curr = "USD", invest_assets = NULL, anual_cost = 0){
  series_sync <- series_merge(series_list = series_list, dates = dates, asset_data = asset_data, ref_curr = ref_curr, assets = names(weights), currencies = NULL,
                              convert_to_ref = TRUE, invest_assets = invest_assets)
  if(length(series_sync)>1){
    daily_rets <- returns(series_sync)
    if(rebal=='daily'){
      cost_factor <- (1 - anual_cost)^(1/252)
      cum_rets <- c(1, cumprod((1 + as.numeric(daily_rets %*% weights[names(series_sync)]))*cost_factor))
      index <- xts(val_ini * cum_rets, order.by = c(index(series_sync)[1], index(daily_rets)))
    }else{
      rets <- returns(series_sync, period=rebal)
      ini_date <- index(series_sync)[1]
      rebal_dates <- c(ini_date, index(rets))
      cash_ini <- w * val_ini
      index <- xts(val_ini, order.by=index(series_sync)[1])
      for(i in 2:length(rebal_dates)){
        cum_rets_i <- apply(1+daily_rets[index(daily_rets) > rebal_dates[i-1] & index(daily_rets) <= rebal_dates[i]],2,cumprod) 
        index <- rbind(index, cum_rets_i %*% cash_ini)
        cash_ini <- w * as.numeric(tail(index,1))
      }
    }
  }else{
    index <- NULL
  }
  return(index)
}
