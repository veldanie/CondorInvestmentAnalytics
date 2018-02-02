#' Porfolio backtest
#'
#' Estimates de covariance using weighted averages of products of past returns.

#' @param weights Portfolio weights.
#' @param capital Initial capital in reference currency.
#' @param currency Reference currency.
#' @param asset_data Datafrane with info regarding each asset.
#' @param series_backtest xts series for backtest.
#' @param fx_hedge_asset Forward hedge ratio per asset.
#' @param fwd_prem forward premium corresponding to holding period (hold_per).
#' @param slippage Slippage basis points.
#' @param commission Commission basis points.
#' @return Backtesting results.
#' @export

portfolio_backtest <- function(weights, capital, currency, asset_data, series_backtest, fx_hedge_asset = rep(0, length(weights)), fwd_prem = NULL, hold_per = '1M', slippage = 5, commission = 5) {

  hold_per_days <- switch(hold_per, '1D' = 1, '1M' = 30, '1Y' = 360)
  n_assets <- length(weights)
  asset_univ <- names(weights)

  index_curr <- asset_data$Currency[match(asset_univ, asset_data$Asset)]
  currencies <- unique(index_curr)
  lcurr <- length(currencies)

  id_fwd <- sapply(index_curr, function(x) ifelse(any(c(x, currency) == 'USD') && (x != currency), c(x, currency)[c(x, currency)!= "USD"], x))
  quotes_curr <- sapply(index_curr, iso_quote, curr2 = currency)

  date_ini <- index(series_backtest)[1]
  date_last <- tail(index(series_backtest),1)

  fx_ini <- as.numeric(series_backtest[,index_curr][date_ini])
  index_val_ini <- as.numeric(series_backtest[,asset_univ][date_ini])


  # Forward outrights:
  if(any(fx_hedge_asset != 0)){
    fwd_outright <- rbind(xts(0, order.by = date_ini), fwd_prem)
    names(fwd_outright) <- names(fwd_prem)
    for (i in 1:ncol(fwd_outright)){
      fwd_fact <- fwd_outright[,i]
      fwd_fact[2] <- fwd_fact[2] * as.numeric(index(fwd_fact)[2]-date_ini)/hold_per_days
      fwd_outright[,i] <- fx_ini[match(substr(names(fwd_prem)[i],1,3), index_curr)] * cumprod(1 + fwd_fact)
    }
  }
  # Initial cash in reference currency:
  cash_ini_ref <- weights[asset_univ] * capital

  # Initial cash in foreign currency:
  cash_ini <- mapply(cash_conv, cash_in = cash_ini_ref, spot = fx_ini, spot_id = quotes_curr, MoreArgs = list(curr_in = currency))
  cash_ini_hedge <- cash_ini * fx_hedge_asset[names(cash_ini)]

  # Transaction costs,
  index_units <- cash_ini/index_val_ini # Number of units depend on execution price.
  tc <- transaction_costs(index_units, index_val_ini, slippage = slippage, commission = commission)

  index_units <- cash_ini/tc$exec_price # Number of units depend on execution price.

  cum_diff_index <- apply(diff(series_backtest[,asset_univ][paste0(c(date_ini,date_last), collapse = '/')])[-1,], 2, cumsum)

  ret_cash_matrix <- matrix(0, ncol = n_assets, nrow = dim(cum_diff_index)[1])

  for(j in 1:ncol(cum_diff_index)){
    spot_ser <- as.numeric(series_backtest[,index_curr[j]][-1,])
    ret_cash <- cum_diff_index[,j]*index_units[j]
    cash_full <- ret_cash + cash_ini[j]

    #If there is hedging:
    if(fx_hedge_asset[j] != 0){
      outrights_ser <- as.numeric(na.approx(object = fwd_outright[, paste0(id_fwd[j], hold_per)], xout = index(series_backtest))[-1,])

      cash_hedged_conv <- mapply(spot = outrights_ser, cash_conv, MoreArgs = list(cash_in = cash_ini_hedge[j], curr_in = index_curr[j], spot_id = quotes_curr[j]))
      cash_not_hedged_conv <- mapply(cash_in = cash_full - cash_ini_hedge[j], spot = spot_ser, cash_conv, MoreArgs = list(curr_in = index_curr[j], spot_id = quotes_curr[j]))

      cash_full_conv <- cash_hedged_conv + cash_not_hedged_conv
    }else{
      cash_full_conv <- mapply(cash_in = cash_full, spot = spot_ser, cash_conv, MoreArgs = list(curr_in = index_curr[j], spot_id = quotes_curr[j]))
      ret_cash_conv <- cash_full_conv - cash_ini_ref[j]
    }
    #ret_cash_conv <- mapply(cash_in = ret_cash, spot = spot_ser, cash_conv, MoreArgs = list(curr_in = index_curr[j], spot_id = quotes_curr[j]))
    ret_cash_matrix[,j] <- ret_cash_conv
  }

  ret_cash_port <- xts(apply(ret_cash_matrix,1,sum), ymd(rownames(cum_diff_index)))
  ret_port <- ret_cash_port/capital

  return(list(ret_cash_port = ret_cash_port, ret_port = ret_port))
}
