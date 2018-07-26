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
#' @param rebal_per_in_months Rebalancing period in months.
#' @param slippage Slippage basis points.
#' @param commission Commission basis points.
#' @param invest_assets Investable asset. By default: Index. It can be set to ETF or IA (investable asset).
#' @return Backtesting results.
#' @export

portfolio_backtest <- function (weights, capital, currency, asset_data, series_backtest, fx_hedge_asset = rep(0, length(weights)), fwd_prem = NULL, hold_per = '1M', rebal_per_in_months = NA, slippage = 5, commission = 5, invest_assets = NULL) {

  hold_per_days <- switch(hold_per, '1D' = 1, '1M' = 30, '3M' = 90, '1Y' = 360)
  n_assets <- length(weights)
  asset_univ <- names(weights)

  if(!is.null(invest_assets) && invest_assets == 'ETF'){
    index_curr <- asset_data$CurrencyETF[match(asset_univ, asset_data$Asset)]
  }else if (!is.null(invest_assets) && invest_assets == 'IA'){
    index_curr <- asset_data$CurrencyIA[match(asset_univ, asset_data$Asset)]
  }else{
    index_curr <- asset_data$Currency[match(asset_univ, asset_data$Asset)]
  }

  currencies <- unique(index_curr)
  lcurr <- length(currencies)

  id_fwd <- index_curr
  #id_fwd <- sapply(index_curr, function(x) ifelse(any(c(x, currency) == 'USD') && (x != currency), c(x, currency)[c(x, currency)!= "USD"], x))
  quotes_curr <- sapply(index_curr, iso_quote, curr2 = currency)

  date_ini <- index(series_backtest)[1]
  date_last <- tail(index(series_backtest),1)

  rebal_dates <- NA
  if(!is.na(rebal_per_in_months)){
    rebal_dates <- seq(from = date_ini, to = date_last, by = paste(rebal_per_in_months, "months"))[-1]
    rebal_dates <- index(series_backtest)[findInterval(rebal_dates, index(series_backtest))]
  }

  fx_ini <- as.numeric(series_backtest[,index_curr][date_ini])
  index_val_ini <- as.numeric(series_backtest[,asset_univ][date_ini])

  # Forward outrights:
  if(any(fx_hedge_asset != 0)){
    fx_hedge_asset <-fx_hedge_asset[asset_univ]
    fwd_prem <- fwd_prem[index(fwd_prem)>date_ini]
    fwd_outright <- rbind(xts(t(rep(0, ncol(fwd_prem))), order.by = date_ini), fwd_prem)
    names(fwd_outright) <- names(fwd_prem)
    for (i in 1:ncol(fwd_outright)){
      fwd_fact <- fwd_outright[,i]
      fwd_fact[2] <- fwd_fact[2] * as.numeric(index(fwd_fact)[2]-date_ini)/hold_per_days
      fwd_outright[,i] <- fx_ini[match(substr(names(fwd_prem)[i],1,3), index_curr)] * cumprod(1 + fwd_fact)
    }
  }

  # Initial cash in reference currency:
  cash_ini_ref <- weights[asset_univ] * capital

  cash_ini <- mapply(cash_conv, cash_in = cash_ini_ref, spot = fx_ini, spot_id = quotes_curr, MoreArgs = list(curr_in = currency))
  cash_ini_hedge <- cash_ini * fx_hedge_asset[names(cash_ini)]

  # Transaction costs,
  index_units <- cash_ini/index_val_ini # Number of units depend on execution price.
  tc <- transaction_costs(index_units, index_val_ini, slippage = slippage, commission = commission)

  index_units <- cash_ini/tc$exec_price # Number of units depend on execution price.


  if(is.na(rebal_per_in_months)){
    series_assets <- series_backtest[,asset_univ][paste0(c(date_ini,date_last), collapse = '/')]
    cum_diff_index <- apply(diff(series_assets)[-1,], 2, cumsum) + (transaction_costs(0, series_assets[-1], slippage = slippage, purchase = FALSE)$exec_price - series_assets[-1])
  }else{
    dec_dates <- c(date_ini, rebal_dates, date_last)
    cum_diff_index <- series_backtest[,asset_univ][paste0(c(date_ini,date_last), collapse = '/')][-1,]
    for(k in 1:(length(rebal_dates)+1)){
      series_backtest_temp <- series_backtest[,asset_univ][paste0(c(dec_dates[k],dec_dates[k+1]), collapse = '/')]
      cum_diff_index[which(index(cum_diff_index) > dec_dates[k] & index(cum_diff_index) <= dec_dates[k+1]),] <- apply(diff(series_backtest_temp)[-1,], 2, cumsum) +
                                                                                                                (transaction_costs(0, series_backtest_temp[-1], slippage = slippage, purchase = FALSE)$exec_price - series_backtest_temp[-1])
    }
  }

  fx_hedge_ind <- fx_hedge_asset != 0 # Indicator of assets that are hedged.
  fx_conv_ind <- index_curr != currency# Indicator of assets with index_curr != ref_curr
  fx_nhedge_conv <- !fx_hedge_ind & fx_conv_ind # Indicator of non-hedged assets with index_curr != ref_curr.

  if(is.na(rebal_per_in_months)){
    spot_ser <- series_backtest[,index_curr, drop = FALSE][-1,]
    ret_cash <- cum_diff_index * (rep(1, nrow(cum_diff_index)) %*% t(index_units))
    cash_full <- ret_cash + (rep(1, nrow(ret_cash)) %*% t(cash_ini))
    cash_full_conv <- cash_full
    n_dates <- nrow(cash_full)

    #If there is hedging:
    if(any(fx_hedge_ind)){
      outrights_ser <- lapply(fwd_outright[, paste0(id_fwd[fx_hedge_ind], hold_per)], na.approx, xout = index(series_backtest)[-1])

      cash_hedged_conv <- matrix(mapply(spot = as.vector(t(matrix(unlist(outrights_ser), ncol=sum(fx_hedge_ind)))),
                                        cash_in = rep(cash_ini_hedge[fx_hedge_ind], n_dates), curr_in = rep(index_curr[fx_hedge_ind], n_dates),
                                        spot_id = rep(quotes_curr[fx_hedge_ind], n_dates), cash_conv),
                                 ncol = sum(fx_hedge_ind), byrow = TRUE)
      cash_not_hedged_conv <- matrix(mapply(cash_in = as.vector(t(cash_full[, fx_hedge_ind] - (rep(1, n_dates) %*% t(cash_ini_hedge[fx_hedge_ind])))),
                                            spot = as.vector(t(spot_ser[, fx_hedge_ind])),
                                            curr_in = rep(index_curr[fx_hedge_ind], n_dates), spot_id = rep(quotes_curr[fx_hedge_ind], n_dates),
                                            cash_conv),
                                     ncol = sum(fx_hedge_ind), byrow = TRUE)

      cash_full_conv[,fx_hedge_ind] <- cash_hedged_conv + cash_not_hedged_conv
    }
    if(any(fx_nhedge_conv)){
      cash_full_conv[,fx_nhedge_conv] <- matrix(mapply(cash_in = as.vector(t(cash_full[,fx_nhedge_conv])), spot = as.vector(t(spot_ser[,fx_nhedge_conv])),
                                                       curr_in = rep(index_curr[fx_nhedge_conv], n_dates), spot_id = rep(quotes_curr[fx_nhedge_conv], n_dates), cash_conv),
                                                ncol = sum(fx_nhedge_conv), byrow = TRUE)
    }
    ret_cash_matrix <- cash_full_conv - (rep(1, nrow(cash_full_conv)) %*% t(cash_ini_ref))
    ret_cash_port <- xts(apply(ret_cash_matrix,1,sum), ymd(rownames(cum_diff_index)))
    cash_full_conv_all <- cash_full_conv
    diff_cash_assets <- rbind(xts(ret_cash_matrix[1,]), diff(ret_cash_matrix)[-1,])
    colnames(diff_cash_assets) <- colnames(cum_diff_index)
  }else{
    cash_ini_ref_update <- cash_ini_ref
    ret_cash_port <- ret_port <- xts(rep(0, nrow(cum_diff_index)), order.by = index(cum_diff_index))
    diff_cash_assets <- xts(matrix(0, nrow = nrow(cum_diff_index), ncol = n_assets), order.by = index(cum_diff_index))
    cash_full_conv_all <- xts(matrix(0, nrow(cum_diff_index), ncol = n_assets), order.by = index(cum_diff_index))
    colnames(cash_full_conv_all) <- colnames(diff_cash_assets) <- colnames(cum_diff_index)
    capital_prev <- capital

    for(k in 1:(length(rebal_dates)+1)){
      spot_ser <- series_backtest[,index_curr, drop = FALSE][index(series_backtest) > dec_dates[k] & index(series_backtest) <= dec_dates[k+1]]
      cum_diff_index_temp <- cum_diff_index[index(cum_diff_index) > dec_dates[k] & index(cum_diff_index) <= dec_dates[k+1]]
      ret_cash <- cum_diff_index_temp * (rep(1, nrow(cum_diff_index_temp)) %*% t(index_units))
      cash_full <- ret_cash + (rep(1, nrow(ret_cash)) %*% t(cash_ini))
      cash_full_conv <- cash_full
      n_dates <- nrow(cash_full)

      #If there is hedging:
      if(any(fx_hedge_ind)){
        outrights_ser <- lapply(fwd_outright[, paste0(id_fwd[fx_hedge_ind], hold_per)], na.approx, xout = index(cum_diff_index_temp))

        cash_hedged_conv <- matrix(mapply(spot = as.vector(t(matrix(unlist(outrights_ser), ncol=sum(fx_hedge_ind)))),
                                          cash_in = rep(cash_ini_hedge[fx_hedge_ind], n_dates), curr_in = rep(index_curr[fx_hedge_ind], n_dates),
                                          spot_id = rep(quotes_curr[fx_hedge_ind], n_dates), cash_conv),
                                   ncol = sum(fx_hedge_ind), byrow = TRUE)
        cash_not_hedged_conv <- matrix(mapply(cash_in = as.vector(t(cash_full[, fx_hedge_ind] - (rep(1, n_dates) %*% t(cash_ini_hedge[fx_hedge_ind])))),
                                              spot = as.vector(t(spot_ser[, fx_hedge_ind])),
                                              curr_in = rep(index_curr[fx_hedge_ind], n_dates), spot_id = rep(quotes_curr[fx_hedge_ind], n_dates),
                                              cash_conv),
                                       ncol = sum(fx_hedge_ind), byrow = TRUE)

        cash_full_conv[,fx_hedge_ind] <- cash_hedged_conv + cash_not_hedged_conv
      }
      if(any(fx_nhedge_conv)){
        cash_full_conv[, fx_nhedge_conv] <- matrix(mapply(cash_in = as.vector(t(cash_full[,fx_nhedge_conv])), spot = as.vector(t(spot_ser[,fx_nhedge_conv])),
                                                          curr_in = rep(index_curr[fx_nhedge_conv], n_dates), spot_id = rep(quotes_curr[fx_nhedge_conv], n_dates), cash_conv),
                                                   ncol = sum(fx_nhedge_conv), byrow = TRUE)
      }

      ind_per<- index(ret_cash_port) > dec_dates[k] & index(ret_cash_port) <= dec_dates[k+1]
      ret_cash_matrix <- cash_full_conv - (rep(1, nrow(cash_full_conv)) %*% t(cash_ini_ref))
      ret_cash_port[ind_per] <- xts(apply(ret_cash_matrix,1,sum), index(cum_diff_index_temp))
      cash_full_conv_all[ind_per, ] <- cash_full_conv

      diff_cash_assets[ind_per, ] <- diff(rbind(xts(t(cash_ini_ref_update), order.by = dec_dates[k]), cash_full_conv))[-1,]
      capital_update <- as.numeric(tail(ret_cash_port[ind_per],1)) + capital_prev

      ##Rebalancing
      cash_ini_ref_update <- weights[asset_univ] * capital_update
      fx_ini <- as.numeric(series_backtest[,index_curr][dec_dates[k+1]])
      index_val_ini <- as.numeric(series_backtest[,asset_univ][dec_dates[k+1]])

      cash_ini <- mapply(cash_conv, cash_in = cash_ini_ref_update, spot = fx_ini, spot_id = quotes_curr, MoreArgs = list(curr_in = currency))
      cash_ini_hedge <- cash_ini * fx_hedge_asset[names(cash_ini)]

      # Transaction costs,
      index_units <- cash_ini/index_val_ini # Number of units depend on execution price.
      tc <- transaction_costs(index_units, index_val_ini, slippage = slippage, commission = commission)

      index_units <- cash_ini/tc$exec_price # Number of units depend on execution price.

      }
  }
  cash_port <- xts(c(capital, apply(cash_full_conv_all, 1, sum)), order.by = c(date_ini, index(ret_cash_port)))
  weights_port <- cash_full_conv_all / (cash_port[-1] %*% t(rep(1, n_assets)))
  ret_port <- ret_cash_port/capital
  cash_assets <- rbind(xts(t(cash_ini_ref), order.by = date_ini), cash_full_conv_all)

  return(list(ret_cash_port = ret_cash_port, ret_port = ret_port, cash_port = cash_port, cash_assets = cash_assets, diff_cash_assets = diff_cash_assets, weights_port = weights_port, rebal_dates = rebal_dates))
}

