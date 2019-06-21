
#' Active Porfolio Return Summary
#'
#' Estimates active portfolio return measures.
#' @param series Asset series.
#' @param w_port Portfolio weights.
#' @param w_bench Benchmark weights.
#' @param ref_dates Reference dates.
#' @param asset_data Assets DF
#' @return Active summary data frame.
#' @export

active_portfolio_summary <- function(capital, currency, w_port, w_bench, ref_dates, asset_data, series_list, per = "monthly", rebal_per = 1, slippage = 0, commission = 0, port_name = NULL, header_df = c("Retorno Promedio Anual.", "Volatilidad Anual.", "Sharpe", "Retorno Activo", "Tracking Error Anual.", "Razón de Información")) {

  freq <- switch(per, 'daily' = 252, 'monthly' = 12, 'quarterly' = 4)

  asset_names <- unique(names(c(w_port, w_bench)))
  index_curr <- unique(asset_data$Currency[match(asset_names, asset_data$Asset)])
  series_back <- series_merge(series_list, ref_dates, asset_data, currency, asset_names, index_curr, convert_to_ref = FALSE)
  port_back <- portfolio_backtest(w_port, capital, currency, asset_data, series_back[,c(names(w_port), index_curr)], rebal_per_in_months = rebal_per, slippage = slippage, commission = commission)

  rets_port <- periodReturn(port_back$cash_port, period = per)
  avg_port <- mean(rets_port)
  vol_port <- sd(rets_port)
  ann_avg_port <- round(avg_port*freq*100, 3)
  ann_vol_port <- round(vol_port*sqrt(freq)*100, 3)
  sharpe_port <- round(avg_port/vol_port, 3)

  te <- active_ret <- ann_te <- info_ratio <- NA
  if(!is.null(w_bench)){
    bench_back <- portfolio_backtest(w_bench, capital, currency, asset_data, series_back[,names(w_bench)], rebal_per_in_months = rebal_per, slippage = slippage, commission = commission)
    rets_bench <- periodReturn(benck_back$cash_port, period = per)
    avg_bench <- mean(rets_bench)

    te <- sd(rets_port - rets_bench)
    active_ret <- avg_port - avg_bench
    ann_te <- round(te*sqrt(freq)*100,3)
    info_ratio <- round(active_ret/te,3)
  }

  summ_df <- t(c(ann_avg_port, ann_vol_port, sharpe_port, active_ret, ann_te, info_ratio))
  colnames(summ_df) <- header_df
  rownames(summ_df) <- port_name
  return(summ_df)
}
