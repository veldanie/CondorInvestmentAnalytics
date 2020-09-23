
#' Aggregate assets
#'
#' Aggregate assets.
#' @param w portfolio vector
#' @param asset_data Assets DF.
#' @param series_list series list.
#' @param benchmarks Benchmarks DF.
#' @param ref_curr Reference currency.
#' @param dates Series dates.
#' @param period Return period.
#' @param factor Grouping variable.
#' @return Factor weigths and series
#' @export

assets_agg <- function(w, asset_data, series_list, benchmarks, ref_curr="USD", dates=dmy(c("01012000", "01012050")), period="monthly", factor="AssetClass") {

  w <- disagg_portfolio(w, benchmarks)
  asset_series <- series_merge(series_list = series_list, dates = dates, asset_data = asset_data, ref_curr = ref_curr, assets = names(w), convert_to_ref = TRUE)
  asset_rets <- returns(asset_series, period = period, type = 'arithmetic')
  asset_rets_daily <- returns(asset_series, type = 'log') # This return MUST be logarithmic.
  n_rets <- nrow(asset_rets)
  n_ser <- nrow(asset_series)

  asset_data_temp <- asset_data %>% filter(Asset %in% names(w)) %>% dplyr::select(Asset,factor)
  fact_vars <- unique(asset_data_temp %>% dplyr::pull(factor))
  n_vars <- length(fact_vars)
  fact_rets <- matrix(0, nrow = n_rets, ncol = n_vars)
  fact_series <- matrix(0, nrow = n_ser, ncol = n_vars)
  w_fact <- rep(0, n_vars)

  for(k in 1:n_vars){
    fi <- fact_vars[k]
    f_assets <- asset_data_temp$Asset[asset_data_temp[,2]== fi]
    w_fact[k] <- sum(w[f_assets])

    if(w_fact[k] == 0){
      fact_rets[,k] <- apply(asset_rets[,f_assets] * (rep(1, n_rets) %*% t(w[f_assets])),1,sum)
      fact_rets_daily <- apply(asset_rets_daily[,f_assets] * (rep(1, n_ser-1) %*% t(w[f_assets])),1,sum)
      f0 <- as.numeric(asset_series[1,f_assets] %*% w[f_assets])
    }else{
      fact_rets[,k] <- apply(asset_rets[,f_assets] * (rep(1, n_rets) %*% t(w[f_assets])),1,sum) / w_fact[k]
      fact_rets_daily <- apply(asset_rets_daily[,f_assets] * (rep(1, n_ser-1) %*% t(w[f_assets])),1,sum) / w_fact[k]
      f0 <- as.numeric((asset_series[1,f_assets] %*% w[f_assets]) / w_fact[k])
    }

    fact_series[,k] <- c(f0, f0 * exp(cumsum(fact_rets_daily)))
  }
  fact_rets <- xts(fact_rets, order.by = index(asset_rets))
  fact_series <- xts(fact_series, order.by = index(asset_series))
  colnames(fact_rets) <- colnames(fact_series) <- fact_vars
  names(w_fact) <- fact_vars
  return(list(w_fact=w_fact, fact_series=fact_series, fact_rets=fact_rets))
}
