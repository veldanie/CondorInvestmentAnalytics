
#' Active Porfolio Return Summary
#'
#' Estimates active portfolio return measures.
#' @param capital Initial capital.
#' @param currency Currency.
#' @param w_port Portfolio weights.
#' @param w_bench Benchmark weights.
#' @param asset_data Assets DF.
#' @param series_list Series list.
#' @param per Period.
#' @param rebal_per Rebalancing per. in months.
#' @param slippage Slippage.
#' @param commission Commission.
#' @param port_name Portfolio name.
#' @param invest_assets Invest assets.
#' @param fixed_tickers Fixed tickers list (dates and tickers per asset).
#' @param weights_tac Tactical weights xts.
#' @param weights_bench Benchmark weights xts.
#' @param sync_dates Bool, sync. dates.
#' @param fund_complete Bool, indicates if benchmark funds are used.
#' @param index_df Custom index dataframe
#' @return Active summary data frame.
#' @export

active_portfolio_factor_summary <- function(capital, currency, w_port, w_bench, ref_dates, asset_data, series_list, factor="AssetClassMarket", per = "monthly", rebal_per = 1, slippage = 0, commission = 0, port_name = NULL, invest_assets = NULL, fixed_tickers = NULL, weights_tac = NULL, weights_bench = NULL, sync_dates = NULL, total_ret = FALSE, fund_complete = FALSE, index_df=NULL) {
  freq <- switch(per, 'daily' = 252, 'monthly' = 12, 'quarterly' = 4)
  if(is.null(w_port) & is.null(w_bench)){ stop("Null portafolios. Check weights!")}

  if(is.null(w_port)){
    w_port <- w_bench
  }
  if(is.null(w_bench)){
    w_bench <- w_port
  }

  asset_names <- unique(names(c(w_port, w_bench)))
  fixed_curr <- NULL
  bench_curr <- unique(asset_data$Currency[match(asset_names, asset_data$Asset)])
  port_curr <- bench_curr
  if(!is.null(invest_assets) && invest_assets == 'ETF'){
    port_curr <- asset_data$CurrencyETF[match(asset_names, asset_data$Asset)]
  }else if (!is.null(invest_assets) && invest_assets == 'IA'){
    port_curr <- asset_data$CurrencyIA[match(asset_names, asset_data$Asset)]
    if(!is.null(fixed_tickers)){
      ref_tk <- unlist(sapply(names(fixed_tickers), function(x) tail(fixed_tickers[[x]]$tk,1)))
      if(!is.null(index_df)){
        custom_tk_ind <- ref_tk %in% (index_df %>% pull(Ticker))
      }else{
        custom_tk_ind <- rep(FALSE, length(ref_tk))
      }
      if(any(!custom_tk_ind)){
        port_curr[match(names(fixed_tickers)[!custom_tk_ind], asset_names)] <- asset_data$Currency[match(unlist(sapply(ref_tk[!custom_tk_ind], get_asset, asset_data)), asset_data$Asset)]
      }
      if(any(custom_tk_ind)){
        port_curr[match(names(fixed_tickers)[custom_tk_ind], asset_names)] <- currency
      }
    }
  }
  if(!is.null(invest_assets) && invest_assets %in% c("IA", "ETF")){
    port_curr <- unique(c(port_curr, currency))
  }else{
    port_curr <- unique(port_curr)
  }
  asset_names_diff <- setdiff(asset_names, names(fixed_tickers))
  series_back <- series_merge(series_list, ref_dates, asset_data, currency, asset_names_diff, port_curr, convert_to_ref = FALSE, invest_assets = invest_assets, fixed_tickers =  NULL)
  if(length(series_back)==0){
    summ_df <- t(rep(0, length(header_df)))
  }else{
    if(!is.null(fixed_tickers)){
      series_comp <- series_compose(series_list, asset_data, fixed_tickers, ref_dates, ref_curr=currency, join = 'inner', index_df=index_df)
      series_back <- merge.xts(series_back, series_comp$series, join = "inner")
      colnames(series_back) <- c(asset_names_diff, port_curr, names(fixed_tickers))
      series_back <- series_back[, c(asset_names, port_curr)]
      fixed_curr <- series_comp$currs
    }

    # the date 01012000 is added when completing with benchmark
    series_bench <- series_merge(series_list, c(index(series_back)[1], tail(index(series_back), 1)), asset_data, currency, asset_names, bench_curr, convert_to_ref = FALSE)
    intersect_dates <- zoo::as.Date(intersect(index(series_bench), index(series_back)))
    series_bench <- series_bench[intersect_dates]
    series_back <- series_back[intersect_dates]

    if(fund_complete && !is.null(weights_tac) && !is.null(invest_assets) &&
       index(weights_tac)[2] > ref_dates[1] && index(weights_tac)[1]==dmy("01012000")){
      col_bench <- colnames(series_bench)
      add_asset <- setdiff(colnames(weights_tac), col_bench)
      if(length(add_asset)>0){
        index_series_bench <- index(series_bench)
        series_bench_ext <- merge.xts(series_bench, xts(rep(0, length(index_series_bench)), order.by = index_series_bench), join = "inner")
        colnames(series_bench_ext) <- c(col_bench, add_asset)
      }else{
        series_bench_ext <- series_bench
      }

      series_back_list <- list(series_bench_ext, series_back)
      names(series_back_list) <- index(weights_tac)[1:2]
      invest_assets_list <- list(NULL, invest_assets)
      fixed_curr_list <- list(NULL, fixed_curr)
      port_back <- portfolio_backtest_compose(capital, weights_tac, currency, asset_data, series_back_list, rebal_per_in_months = rebal_per,  rebal_dates = NULL, slippage = slippage, commission = commission, invest_assets_list = invest_assets_list, fixed_curr_list = fixed_curr_list)
    }else{
      port_back <- portfolio_backtest(w_port, capital, currency, asset_data, series_back[,c(names(w_port), port_curr)], rebal_per_in_months = rebal_per, weights_xts = weights_tac, slippage = slippage, commission = commission, invest_assets = invest_assets, fixed_curr = fixed_curr)
    }
    total_port <- as.numeric(tail(port_back$ret_port,1))

    rebal_dates <- NULL
    if(sync_dates && !is.null(weights_tac)){
      tac_dates <- index(weights_tac)
      n_dates <- length(tac_dates)
      if (!is.null(weights_bench) && nrow(weights_bench)>0){
        weights_temp = xts(matrix(0, nrow=n_dates, ncol=ncol(weights_bench)), order.by = tac_dates)
        colnames(weights_temp) <- colnames(weights_bench)
        for (k in 1:n_dates){
          dif_days <- as.numeric(tac_dates[k] - index(weights_bench))
          if (any(dif_days>-3)){ # 3 days dif limit
            pos_port <- which.min(replace(dif_days, dif_days<(-3), NA))
          }else{
            pos_port <- which.max(dif_days)
          }
          weights_temp[k,] <- as.vector(weights_bench[pos_port,])
        }
        weights_bench <- weights_temp
      }else{
        weights_bench <- NULL
        rebal_dates <- index(weights_tac)
      }
    }
    bench_back <- portfolio_backtest(w_bench, capital, currency, asset_data, series_bench[,c(names(w_bench), bench_curr)], rebal_per_in_months = rebal_per, weights_xts = NULL,
                                     rebal_dates = rebal_dates, slippage = slippage, commission = commission)
    total_bench <-as.numeric(tail(bench_back$ret_port,1))

    summ_df <- total_return_attribution(w_port, w_bench, total_port, total_bench, port_back$cash_port, bench_back$cash_port, port_back$diff_cash_assets, bench_back$diff_cash_assets, port_back$weights_port, bench_back$weights_port, port_back$dec_dates, bench_back$dec_dates, factor=factor, asset_data=asset_data)$summ_factor_df
    rownames(summ_df) <- paste(port_name, "-", rownames(summ_df))
  }
  return(summ_df)
}
