
#' Expected return limits
#'
#' Calculates an asset expected return with upper and lower bounds
#' @param series_list series_list
#' @param funds Vector of assets (funds) names
#' @param ports Vector of portfolios
#' @param ref_currency ref_currency
#' @param period period - daily, monthly, quarterly, yearly
#' @param conn database connection
#' @param conf_interv Return conf. interval
#' @param horizon_in_months Returns horizon in months
#' @param method Interval gen. method
#' @return Datafame with asset name, expected return, upper bound, lower bound
#' @export

exp_ret_limits <- function(series_list, assets, ports, ref_currency, conn, asset_data, period = 'monthly', conf_interv = 0.95, ia = FALSE,
                           since_date = '01012008', horizon_in_months=NULL, method="chebyshev") {
  period_adjust <- c(252, 12, 4, 1)
  names(period_adjust) <- c('daily', 'monthly', 'quarterly', 'yearly')
  temp_series <- series_merge(series_list, dates = dmy(c(since_date, "31122050")), asset_data, ref_curr = ref_currency, assets = assets, convert_to_ref = TRUE)
  temp_returns <- returns(temp_series, period = period)
  if (is.null(horizon_in_months)){
    temp_df_asset <- return_interval(temp_returns, freq = period_adjust[period], conf_interv,
                                     header=c('portfolio', 'retornoEsperado', 'limiteInferior','limiteSuperior'), method=method)
  }else{
    temp_df_asset <- return_hor_interval(temp_series, horizon_in_months = horizon_in_months, conf_interv = conf_interv,
                                         header=c('portfolio', 'retornoEsperado', 'limiteInferior','limiteSuperior'))
  }

  DBI::dbBegin(conn)
  for(port in ports){
    port_weights <- conn %>% tbl("Weights") %>% filter(PortId == port)
    max_date <- port_weights %>% pull(DatePort) %>% max()
    assets_port <- data.frame(port_weights %>% filter(DatePort == max_date) %>% dplyr::select(Asset, Weight))

    invest_assets <- conn %>% tbl('InvestTickers') %>% filter(PortId == port)
    max_date <- invest_assets %>% pull(DateTicker) %>% max()
    invest_tickers <- data.frame(invest_assets %>% filter(DateTicker == max_date) %>% dplyr::select(Asset, Ticker))

    if(ia){
      assets_port$ticker <- invest_tickers$Ticker[match(assets_port$Asset, invest_tickers$Asset)]
      assets_port$ticker[is.na(assets_port$ticker)] <- asset_data$TickerInvestAsset[match(assets_port$Asset[is.na(assets_port$ticker)], asset_data$Asset)]
      series_port <- series_list[assets_port$ticker]
      names_assets <- names(series_port)
      series_port <- na.omit(do.call(merge, series_port))
      colnames(series_port) <- names_assets
    }else{
      series_port <- series_merge(series_list, dates = dmy(c(since_date, "31122050")), asset_data, ref_curr = ref_currency, assets = assets_port$Asset, convert_to_ref = TRUE, invest_assets = 'IA')
    }
    w <- assets_port$Weight
    names(w) <- assets_port$Asset
    port_series <- index_series(series_list, w, dates=dmy(c(since_date, "31122050")), val_ini = 100, ref_curr = ref_curr)
    port_rets <- returns(port_series, period = period)
    colnames(port_rets) <- colnames(port_series) <- port
    if (is.null(horizon_in_months)){
      temp_port_data <- return_interval(port_rets, freq = period_adjust[period], conf_interv,
                                       header=c('portfolio', 'retornoEsperado', 'limiteInferior','limiteSuperior'), method=method)
    }else{
      temp_port_data <- return_hor_interval(port_series, horizon_in_months = horizon_in_months, conf_interv = conf_interv,
                                           header=c('portfolio', 'retornoEsperado', 'limiteInferior','limiteSuperior'))
    }
    temp_df_asset <- rbind(temp_df_asset, temp_port_data)
  }

  DBI::dbCommit(conn)
  poolReturn(conn)

  return(temp_df_asset)
}


