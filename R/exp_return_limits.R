
#' Expected return limits
#'
#' Calculates an asset expected return with upper and lower bounds
#' @param series_list series_list
#' @param funds Vector of assets (funds) names
#' @param ports Vector of portfolios
#' @param ref_currency ref_currency
#' @param period period - daily, monthly, quarterly, yearly
#' @param conn database connection
#' @param quant Quantile to calculate intervals according to a standard normal distribution (0.95 = 1.64)
#' @return Datafame with asset name, expected return, upper bound, lower bound
#' @export


### Agregar un argumento de año para seleccionar a partir de que fecha hacer los cálculos

exp_ret_limits <- function(series_list, assets, ports, ref_currency, conn, asset_data, period = 'monthly', quant = 0.95, ia = FALSE,
                           since_date = '01012008') {
  period_adjust <- c(252, 12, 4, 1)
  names(period_adjust) <- c('daily', 'monthly', 'quarterly', 'yearly')
  temp_series <- series_merge(series_list, dates = dmy(c(since_date, "31122050")), asset_data, ref_curr = ref_currency, assets = assets, convert_to_ref = TRUE)
  temp_returns <- returns(temp_series, period = period)

  mean_asset_rets <- round(apply(temp_returns, 2, mean) * as.vector(period_adjust[period]), 3)
  sd_asset_rets <- apply(temp_returns, 2, sd) * as.vector(sqrt(period_adjust[period]))
  mean_asset_rets_ub <- round(mean_asset_rets + qnorm(quant) * sd_asset_rets / sqrt(nrow(temp_returns)), 3)
  mean_asset_rets_lb <- round(mean_asset_rets - qnorm(quant) * sd_asset_rets / sqrt(nrow(temp_returns)), 3)

  temp_df_asset <- data.frame(portfolio = assets, retornoEsperado = mean_asset_rets, limiteSuperior = mean_asset_rets_ub,
                              limiteInferior = mean_asset_rets_lb, row.names = NULL)

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
    returns_assets <- apply(returns(series_port, period = period), 2, mean) * period_adjust[period]
    covar_matrix <- cov(returns(series_port, period = period) * as.vector(period_adjust[period]))
    port_return <- round(as.numeric(t(assets_port$Weight) %*% returns_assets), 3)
    port_sd <- sqrt(as.numeric(t(assets_port$Weight) %*% covar_matrix %*% assets_port$Weight))
    port_ret_ub <- round(port_return + qnorm(quant) * port_sd / sqrt(nrow(series_port)), 3)
    port_ret_lb <- round(port_return - qnorm(quant) * port_sd / sqrt(nrow(series_port)), 3)
    temp_port_data <- c(port, port_return, port_ret_ub, port_ret_lb)
    temp_df_asset <- rbind(temp_df_asset, temp_port_data)
  }

  DBI::dbCommit(conn)
  poolReturn(conn)

  return(temp_df_asset)

}

