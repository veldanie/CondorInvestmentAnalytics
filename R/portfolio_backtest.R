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
#' @param weights_xts Weigths xts ordered by rebalancing dates.
#' @param rebal_dates Rebalancing dates.
#' @param slippage Slippage basis points.
#' @param commission Commission basis points.
#' @param invest_assets Investable asset. By default: Index. It can be set to ETF or IA (investable asset).
#' @param fixed_curr Fixed tickers currency vector.
#' @return Backtesting results.
#' @export

portfolio_backtest <- function(weights, capital, currency, asset_data, series_backtest, fx_hedge_asset = rep(0, length(weights)), fwd_prem = NULL, hold_per = '1M', rebal_per_in_months = NA, weights_xts = NULL, rebal_dates = NULL, slippage = 5, commission = 5, invest_assets = NULL, fixed_curr = NULL, rebal_thres=0, weights_ini = NULL) {

  hold_per_days <- switch(hold_per, '1D' = 1, '1M' = 30, '3M' = 90, '1Y' = 360)
  n_assets <- length(weights)
  asset_univ <- names(weights)

  if(is.null(names(fx_hedge_asset))){
    names(fx_hedge_asset) <- asset_univ
  }
  if(!is.null(invest_assets) && invest_assets == 'ETF'){
    index_curr <- asset_data$CurrencyETF[match(asset_univ, asset_data$Asset)]
  }else if (!is.null(invest_assets) && invest_assets == 'IA'){
    index_curr <- asset_data$CurrencyIA[match(asset_univ, asset_data$Asset)]
    if(!is.null(fixed_curr)){
      index_curr[match(names(fixed_curr), asset_univ)] <- fixed_curr
    }
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

  if(!is.null(weights_xts)){
    weights <- as.vector(weights_xts[max(1,findInterval(date_ini, index(weights_xts))),])
    names(weights) <- colnames(weights_xts)
    weights_xts <- weights_xts[index(weights_xts)>date_ini & index(weights_xts)<date_last,]
  }

  if(is.null(weights_ini)){
    weights_ini <- weights
  }
  if(!is.null(weights_xts) && nrow(weights_xts)>0){
    rebal_dates <- index(weights_xts)
    rebal_dates <- index(series_backtest)[findInterval(rebal_dates, index(series_backtest))]
    index(weights_xts) <- rebal_dates
  }else if(!is.null(rebal_dates)){
    rebal_dates <- index(series_backtest)[findInterval(rebal_dates[rebal_dates>date_ini], index(series_backtest))]
    weights_xts <- xts(rep(1, length(rebal_dates)) %*% t(weights), order.by = rebal_dates)
  }else if(!is.na(rebal_per_in_months)){
    rebal_dates <- seq(from = date_ini, to = date_last, by = paste(rebal_per_in_months, "months"))[-1]
    rebal_dates <- index(series_backtest)[findInterval(rebal_dates, index(series_backtest))]
    weights_xts <- xts(rep(1, length(rebal_dates)) %*% t(weights), order.by = rebal_dates)
  }else{
    rebal_dates <- NA
  }
  non_rebal_dates <- c()
  dec_dates <- c(date_ini, date_last)
  if(any(is.na(rebal_dates))){
    series_assets <- series_backtest[,asset_univ][paste0(c(date_ini,date_last), collapse = '/')]
    cum_diff_index <- apply(diff(series_assets)[-1,], 2, cumsum) + (transaction_costs(0, series_assets[-1], slippage = slippage, purchase = FALSE)$exec_price - series_assets[-1])
  }else{
    dec_dates <- unique(c(date_ini, rebal_dates, date_last))
    cum_diff_index <- series_backtest[,asset_univ][paste0(c(date_ini,date_last), collapse = '/')][-1,]
    for(k in 1:(length(dec_dates)-1)){
      series_backtest_temp <- series_backtest[,asset_univ][paste0(c(dec_dates[k],dec_dates[k+1]), collapse = '/')]
      cum_diff_index[which(index(cum_diff_index) > dec_dates[k] & index(cum_diff_index) <= dec_dates[k+1]),] <- apply(diff(series_backtest_temp)[-1,], 2, cumsum) +
        (transaction_costs(0, series_backtest_temp[-1], slippage = slippage, purchase = FALSE)$exec_price - series_backtest_temp[-1])
    }
  }

  fx_ini <- as.numeric(series_backtest[,index_curr][date_ini])
  index_val_ini <- as.numeric(series_backtest[,asset_univ][date_ini])

  # Forward outrights:
  if(any(fx_hedge_asset != 0)){
    fx_hedge_asset <- fx_hedge_asset[asset_univ]
    fwd_names <- colnames(fwd_prem)
    fwd_prem <- fwd_prem[index(fwd_prem)>date_ini]
    fwd_prem <- rbind(xts(t(rep(0, ncol(fwd_prem))), order.by = date_ini), fwd_prem)
    fwd_prem[2,] <- fwd_prem[2,] * as.numeric(index(fwd_prem)[2]-date_ini)/hold_per_days
    colnames(fwd_prem) <- fwd_names

  }

  # Initial cash in reference currency:
  cash_ini_ref <- weights_ini[asset_univ] * capital

  cash_ini <- mapply(cash_conv, cash_in = cash_ini_ref, spot = fx_ini, spot_id = quotes_curr, MoreArgs = list(curr_in = currency))
  cash_ini_hedge <- cash_ini * fx_hedge_asset[names(cash_ini)]

  # Transaction costs,
  index_units <- cash_ini/index_val_ini # Number of units depend on execution price.
  tc <- transaction_costs(index_units, index_val_ini, slippage = slippage, commission = commission)
  index_units <- cash_ini/tc$exec_price # Number of units depend on execution price.
  index_units[is.na(index_units)] <- 0
  
  fx_hedge_ind <- fx_hedge_asset[asset_univ] != 0 # Indicator of assets that are hedged.
  fx_conv_ind <- index_curr != currency# Indicator of assets with index_curr != ref_curr
  fx_nhedge_conv <- !fx_hedge_ind & fx_conv_ind # Indicator of non-hedged assets with index_curr != ref_curr.

  if(any(is.na(rebal_dates))){
    spot_ser <- series_backtest[,index_curr, drop = FALSE][-1,]
    ret_cash <- cum_diff_index * (rep(1, nrow(cum_diff_index)) %*% t(index_units))
    cash_full <- ret_cash + (rep(1, nrow(ret_cash)) %*% t(cash_ini))
    cash_full_conv <- cash_full
    n_dates <- nrow(cash_full)

    #If there is hedging:
    if(any(fx_hedge_ind)){
      fwd_outright <- xts((rep(1, nrow(fwd_prem)) %*% t(fx_ini[match(substr(names(fwd_prem),1,3), index_curr)])) * apply(1 + fwd_prem,2, cumprod),
                          order.by = index(fwd_prem))
      colnames(fwd_outright) <- colnames(fwd_prem)
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
    cash_full_conv_all <- cash_full_conv
    diff_cash_assets <- rbind(xts(ret_cash_matrix[1,]), diff(ret_cash_matrix)[-1,])
    colnames(diff_cash_assets) <- colnames(cum_diff_index)
  }else{
    cash_ini_ref_update <- cash_ini_ref
    diff_cash_assets <- xts(matrix(0, nrow = nrow(cum_diff_index), ncol = n_assets), order.by = index(cum_diff_index))
    cash_full_conv_all <- xts(matrix(0, nrow(cum_diff_index), ncol = n_assets), order.by = index(cum_diff_index))
    colnames(cash_full_conv_all) <- colnames(diff_cash_assets) <- colnames(cum_diff_index)
    capital_prev <- capital

    for(k in 1:(length(dec_dates)-1)){
      spot_ser <- series_backtest[,index_curr, drop = FALSE][index(series_backtest) > dec_dates[k] & index(series_backtest) <= dec_dates[k+1]]
      cum_diff_index_temp <- cum_diff_index[index(cum_diff_index) > dec_dates[k] & index(cum_diff_index) <= dec_dates[k+1]]
      ret_cash <- cum_diff_index_temp * (rep(1, nrow(cum_diff_index_temp)) %*% t(index_units))
      cash_full <- ret_cash + (rep(1, nrow(ret_cash)) %*% t(cash_ini))
      cash_full_conv <- cash_full
      n_dates <- nrow(cash_full)

      #If there is hedging:
      if(any(fx_hedge_ind)){
        ind_fwd_per<- index(fwd_prem) > dec_dates[k]
        fwd_prem_temp <- rbind(xts(t(rep(0, ncol(fwd_prem))), order.by = dec_dates[k]), fwd_prem[ind_fwd_per,])
        fwd_prem_temp[2,] <- fwd_prem_temp[2,] * as.numeric(index(fwd_prem_temp)[2]-dec_dates[k])/hold_per_days


        fwd_outright <- xts((rep(1, nrow(fwd_prem_temp)) %*% t(fx_ini[match(substr(names(fwd_prem),1,3), index_curr)])) * apply(1 + fwd_prem_temp,2, cumprod),
                            order.by = index(fwd_prem_temp))
        colnames(fwd_outright) <- colnames(fwd_prem)
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

      ind_per<- index(cum_diff_index) > dec_dates[k] & index(cum_diff_index) <= dec_dates[k+1]
      cash_full_conv_all[ind_per, ] <- cash_full_conv
      diff_cash_assets[ind_per, ] <- diff(rbind(xts(t(cash_ini_ref_update), order.by = dec_dates[k]), cash_full_conv))[-1,]
      capital_update <- sum(diff_cash_assets) + capital_prev

      ##Rebalancing
      if(k < (length(dec_dates)-1)){
        if (k == 1){
          rebal_ind <- (sum(abs(as.vector(weights_xts[dec_dates[k+1],asset_univ])-as.vector(weights[asset_univ]))) > 0.001) | any(abs(as.vector(tail(cash_full_conv,1)/sum(tail(cash_full_conv,1))) - as.vector(weights_xts[dec_dates[k+1],asset_univ])) >= rebal_thres)
        }else{
          rebal_ind <- (sum(abs(as.vector(weights_xts[dec_dates[k+1],asset_univ])-as.vector(weights_xts[dec_dates[k],asset_univ]))) > 0.001) | any(abs(as.vector(tail(cash_full_conv,1)/sum(tail(cash_full_conv,1))) - as.vector(weights_xts[dec_dates[k+1],asset_univ])) >= rebal_thres)
        }
        rebal_ind <- any(abs(as.vector(tail(cash_full_conv,1)/sum(tail(cash_full_conv,1))) - as.vector(weights_xts[dec_dates[k+1],asset_univ])) >= rebal_thres)
        if (rebal_ind){
          cash_ini_ref_update <- as.vector(weights_xts[dec_dates[k+1],asset_univ]) * capital_update
        }else{
          cash_ini_ref_update <- coredata(tail(cash_full_conv,1))[1,]
          non_rebal_dates <- c(non_rebal_dates, dec_dates[k+1])
        }
        names(cash_ini_ref_update) <- colnames(weights_xts)
        cash_full_conv_all[dec_dates[k+1], ] <- cash_ini_ref_update
        fx_ini <- as.numeric(series_backtest[,index_curr][dec_dates[k+1]])
        index_val_ini <- as.numeric(series_backtest[,asset_univ][dec_dates[k+1]])

        cash_ini <- mapply(cash_conv, cash_in = cash_ini_ref_update, spot = fx_ini, spot_id = quotes_curr, MoreArgs = list(curr_in = currency))
        cash_ini_hedge <- cash_ini * fx_hedge_asset[names(cash_ini)]

        # Transaction costs,
        index_units <- cash_ini/index_val_ini # Number of units depend on execution price.
        tc <- transaction_costs(index_units, index_val_ini, slippage = slippage, commission = commission)
        index_units <- cash_ini/tc$exec_price # Number of units depend on execution price.
        index_units[is.na(index_units)] <- 0
      }
    }
  }
  # cash_port <- xts(c(capital, apply(cash_full_conv_all, 1, sum)), order.by = c(date_ini, index(diff_cash_assets)))
  cash_port <- xts(c(capital, capital + cumsum(rowSums(diff_cash_assets))), order.by = c(date_ini, index(diff_cash_assets)))
  weights_port <- rbind(xts(t(weights[colnames(cash_full_conv_all)]), order.by = date_ini), cash_full_conv_all / (cash_port[-1] %*% t(rep(1, n_assets))))
  ret_cash_port <- cash_port[2:length(cash_port),] - capital
  ret_port <- ret_cash_port/capital
  cash_assets <- rbind(xts(t(cash_ini_ref), order.by = date_ini), cash_full_conv_all)

  return(list(ret_cash_port = ret_cash_port, ret_port = ret_port, cash_port = cash_port, cash_assets = cash_assets, diff_cash_assets = diff_cash_assets, weights_port = weights_port, dec_dates = dec_dates, non_rebal_dates=non_rebal_dates, weights_xts = weights_xts))
}

