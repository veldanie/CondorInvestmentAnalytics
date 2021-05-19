
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
#' @param header_df DF headers.
#' @return Active summary data frame.
#' @export

active_portfolio_summary <- function(capital, currency, w_port, w_bench, ref_dates, asset_data, series_list, per = "monthly", rebal_per = 1, slippage = 0, commission = 0, port_name = NULL, invest_assets = NULL, fixed_tickers = NULL, weights_tac = NULL, weights_bench = NULL, sync_dates = NULL, total_ret = FALSE, fund_complete = FALSE, index_df=NULL, header_df = c("Ret Total Bench", "Ret Total Port", "Ret Prom Bench", "Ret Prom Port", "Vol", "Sharpe", "Alpha", "TE", "RI", "AA", "SS/INTER"), dissag_ret_attrib=FALSE) {
  freq <- switch(per, 'daily' = 252, 'monthly' = 12, 'quarterly' = 4)
  if(is.null(w_port) & is.null(w_bench)){ stop("Null portafolios. Check weights!")}

  if(is.null(w_port)){
    w_port <- w_bench
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
    attrib_df <- NULL
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

    total_port <- round(100*as.numeric(tail(port_back$ret_port,1)), 3)
    rets_port <- returns(port_back$cash_port, period = per, leading = FALSE)
    avg_port <- mean(rets_port)
    vol_port <- sd(rets_port)
    ann_avg_port <- round(avg_port*freq*100, 3)
    ann_vol_port <- round(vol_port*sqrt(freq)*100, 3)
    sharpe_port <- round(avg_port/vol_port, 3)

    te <- active_ret <- ann_te <- info_ratio <- NA
    if(!is.null(w_bench)){
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
      bench_back <- portfolio_backtest(w_bench, capital, currency, asset_data, series_bench[,c(names(w_bench), bench_curr)], rebal_per_in_months = rebal_per, weights_xts = weights_bench,
                                       rebal_dates = rebal_dates, slippage = slippage, commission = commission)

      total_bench <- round(100*as.numeric(tail(bench_back$ret_port,1)), 3)
      rets_bench <- returns(bench_back$cash_port, period = per, leading = FALSE)
      avg_bench <- mean(rets_bench)
      ann_avg_bench <- round(avg_bench*freq*100, 3)

      te <- ifelse(nrow(rets_port)>1, sd(rets_port - rets_bench), 0)

      active_ret <- round(100*(avg_port - avg_bench) * freq, 3)
      active_total_ret <- round(total_port - total_bench, 3)
      ann_te <- round(te*sqrt(freq)*100,3)
      info_ratio <- 0
      if(te > 0){
        info_ratio <- round(active_ret/ann_te,3)
      }

      if(total_ret){
        active_ret_aa <- active_total_ret
      }else{
        active_ret_aa <- active_ret
      }

      if(!is.null(invest_assets)){
        series_back_aa <- series_merge(series_list, c(index(series_back)[1], tail(index(series_back), 1)), asset_data, currency, asset_names, bench_curr, convert_to_ref = FALSE)
        port_back_aa <- portfolio_backtest(w_port, capital, currency, asset_data, series_back_aa[,c(names(w_port), bench_curr)], rebal_per_in_months = rebal_per, weights_xts = weights_tac, slippage = slippage, commission = commission)
        total_port_aa <- round(100*as.numeric(tail(port_back_aa$ret_port,1)), 3)
        rets_port_aa <- returns(port_back_aa$cash_port, period = per, leading = FALSE)
        avg_port_aa <- mean(rets_port_aa)
        #ann_avg_port <- round(avg_port_aa*freq*100, 3)
        if(total_ret){
          active_ret_aa <- round(total_port_aa - total_bench, 3)
          active_ret_ss <- active_total_ret - active_ret_aa
        }else{
          active_ret_aa <- round(100*(avg_port_aa - avg_bench) * freq, 3)
          active_ret_ss <- active_ret - active_ret_aa
        }
      }else{
        active_ret_ss <- 0
      }
    }
    summ_df <- t(c(total_bench, total_port, ann_avg_bench, ann_avg_port, ann_vol_port, sharpe_port, active_ret, ann_te, info_ratio, active_ret_aa, active_ret_ss))
    if(dissag_ret_attrib){
      attrib_df <- total_return_attribution(w_port, w_bench, total_port, total_bench, port_back$cash_port, bench_back$cash_port, port_back$diff_cash_assets, bench_back$diff_cash_assets, port_back$weights_port, bench_back$weights_port, port_back$dec_dates, bench_back$dec_dates, factor=NULL, asset_data=asset_data)$summ_df
    }else{
      attrib_df <- NULL
    }
  }
  colnames(summ_df) <- header_df
  rownames(summ_df) <- port_name
  return(list(summ_df=summ_df, bench_series=bench_back$cash_port, port_series=port_back$cash_port, attrib_df=attrib_df))
}
