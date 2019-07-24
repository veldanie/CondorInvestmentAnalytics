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
#' @param fixed_tickers Fixed tickers vector.
#' @return Backtesting results.
#' @export

portfolio_backtest_compose <- function(capital, weights_xts, currency, asset_data, series_backtest_list, fx_hedge_asset = rep(0, length(weights)), fwd_prem = NULL, hold_per = '1M', rebal_per_in_months = NA, rebal_dates = NULL, slippage = 5, commission = 5, invest_assets_list = NULL, fixed_tickers_list = NULL) {
  ref_dates <- names(series_backtest_list)
  capital_ini <- capital
  ret_cash_port <- diff_cash_assets <- weights_port <- dec_dates <- NULL
  lw <- 0
  for(i in 1:length(ref_dates)){
    date_i <- ref_dates[i]
    weights <- as.vector(weights_xts[date_i])
    names(weights) <- colnames(weights_xts)
    range_dates <- ref_dates[i:(i+1)]
    range_dates[is.na(range_dates)] <- ""
    series_back <- series_backtest_list[[date_i]][paste0(range_dates, collapse = "/")]
    pb_i <- portfolio_backtest(weights, capital, currency, asset_data, series_back, fx_hedge_asset = fx_hedge_asset, fwd_prem = fwd_prem, hold_per = hold_per, rebal_per_in_months = rebal_per_in_months, weights_xts = weights_xts, rebal_dates = rebal_dates, slippage = slippage, commission = commission, invest_assets = invest_assets_list[[i]], fixed_tickers = fixed_tickers_list[[i]])
    ret_cash_port <- rbind(ret_cash_port, pb_i$ret_cash_port + capital - capital_ini)
    diff_cash_assets <- rbind(diff_cash_assets, pb_i$diff_cash_assets)

    weights_port <- rbind(weights_port[1:(lw-1),], pb_i$weights_port)
    lw <- nrow(weights_port)
    dec_dates <- c(dec_dates, pb_i$dec_dates)
    capital <- as.numeric(tail(ret_cash_port, 1)) + capital
  }
  dec_dates <- zoo::as.Date(unique(dec_dates))
  ret_port <- ret_cash_port/capital_ini
  cash_port <- rbind(xts(as.numeric(capital_ini), order.by = dec_dates[1]), ret_cash_port + capital_ini)
  return(list(ret_cash_port = ret_cash_port, ret_port = ret_port, cash_port = cash_port, diff_cash_assets = diff_cash_assets, weights_port = weights_port, dec_dates = dec_dates))
}

