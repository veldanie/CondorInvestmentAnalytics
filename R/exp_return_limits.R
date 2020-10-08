
#' Expected return limits
#'
#' Calculates an asset expected return with upper and lower bounds
#' @param series_list series_list
#' @param funds Vector of assets (funds) names
#' @param ports Vector of portfolios
#' @param ref_currency ref_currency
#' @param period period - daily, monthly, quarterly, yearly
#' @param db database connection
#' @param conf_interv Return conf. interval
#' @param horizon_in_months Returns horizon in months
#' @param method Interval gen. method
#' @param views_abs Absolute views
#' @return Datafame with asset name, expected return, upper bound, lower bound
#' @export

exp_ret_limits <- function(series_list, assets, ports, ref_currency, db, asset_data, period = 'monthly', conf_interv = 0.95,
                           since_date = '01012008', horizon_in_months=NULL, method="chebyshev", views_abs=NULL) {
  conn <- poolCheckout(db)
  period_adjust <- c(252, 12, 4, 1)
  names(period_adjust) <- c('daily', 'monthly', 'quarterly', 'yearly')
  if (!is.null(views_abs)){
    assets_temp <- unique(c(assets, views_abs$asset))
    currs <- asset_data$Currency[match(assets_temp, asset_data$Asset)]
    add_currs <- unique(asset_data$Currency[match(assets, asset_data$Asset)])
  }else{
    assets_temp <- assets
    add_currs <- NULL
  }

  temp_series <- series_merge(series_list, dates = dmy(c(since_date, "31122050")), asset_data, ref_curr = ref_currency, assets = assets_temp, currencies = unique(add_currs), convert_to_ref = TRUE)
  temp_returns <- returns(temp_series, period = period)

  if (is.null(horizon_in_months)){
    temp_df_asset <- return_interval(temp_returns, freq = period_adjust[period], conf_interv,
                                     header=c('portfolio', 'retornoEsperado', 'limiteInferior','limiteSuperior'), method=method)
  }else{
    temp_df_asset <- return_hor_interval(temp_series, horizon_in_months = horizon_in_months, conf_interv = conf_interv,
                                         header=c('portfolio', 'retornoEsperado', 'limiteInferior','limiteSuperior'))
  }

  ##Views
  if (!is.null(views_abs)){
    for (curr_i in add_currs){
      ind_assets_curr <- currs==curr_i
      mu <- temp_df_asset[assets_temp,]$retornoEsperado[ind_assets_curr]
      names(mu) <- temp_df_asset[assets_temp,]$portfolio[ind_assets_curr]
      Sigma <- covar(temp_returns[,assets_temp])$cov_matrix_ann[ind_assets_curr,ind_assets_curr]
      views_abs_curr <- views_abs[views_abs$asset %in% assets_temp[ind_assets_curr],]
      q <- (1+temp_df_asset$retornoEsperado[temp_df_asset$portfolio==curr_i]) * views_abs_curr$ret
      P <- matrix(0, length(q), length(mu))
      for (i in 1:length(q)){P[i,match(views_abs_curr$asset[i], names(mu))] <- 1}
      post_ret <- posterior_params(mu,q,1,Sigma,P,conf = 0.75)$post_ret
      ret_shift <- post_ret - mu
      pos_assets <- match(names(mu),temp_df_asset$portfolio)
      temp_df_asset[pos_assets,2:4] <- temp_df_asset[pos_assets,2:4] + ret_shift
    }
  }
  temp_df_asset <- temp_df_asset[match(assets,rownames(temp_df_asset)),]
  ##

  DBI::dbBegin(conn)
  for(port in ports){
    port_weights <- conn %>% tbl("Weights") %>% filter(PortId == port)
    max_date <- port_weights %>% pull(DatePort) %>% max()
    assets_port <- data.frame(port_weights %>% filter(DatePort == max_date) %>% dplyr::select(Asset, Weight))

    invest_assets <- conn %>% tbl('InvestTickers') %>% filter(PortId == port)
    max_date <- invest_assets %>% pull(DateTicker) %>% max()
    invest_tickers <- data.frame(invest_assets %>% filter(DateTicker == max_date) %>% dplyr::select(Asset, Ticker))

    w <- assets_port$Weight
    names(w) <- assets_port$Asset
    port_series <- index_series(series_list, w, dates=dmy(c(since_date, "31122050")), val_ini = 100, ref_curr = ref_currency)
    port_rets <- returns(port_series, period = period)
    colnames(port_rets) <- colnames(port_series) <- port
    if (is.null(horizon_in_months)){
      temp_port_data <- return_interval(port_rets, freq = period_adjust[period], conf_interv,
                                        header=c('portfolio', 'retornoEsperado', 'limiteInferior','limiteSuperior'), method=method)
    }else{
      temp_port_data <- return_hor_interval(port_series, horizon_in_months = horizon_in_months, conf_interv = conf_interv,
                                           header=c('portfolio', 'retornoEsperado', 'limiteInferior','limiteSuperior'))
    }
    if (!is.null(views_abs)){
      assets_temp <- unique(c(assets_port$Asset, views_abs$asset))
      currs <- asset_data$Currency[match(assets_temp, asset_data$Asset)]
      add_currs <- unique(asset_data$Currency[match(assets_port$Asset, asset_data$Asset)])
      temp_series <- series_merge(series_list, dates = dmy(c(since_date, "31122050")), asset_data, ref_curr = ref_currency, assets = assets_temp, currencies = add_currs, convert_to_ref = TRUE)
      temp_returns <- returns(temp_series, period = period)

      if (is.null(horizon_in_months)){
        temp_df_asset_port <- return_interval(temp_returns, freq = period_adjust[period], conf_interv,
                                         header=c('portfolio', 'retornoEsperado', 'limiteInferior','limiteSuperior'), method=method)
      }else{
        temp_df_asset_port <- return_hor_interval(temp_series, horizon_in_months = horizon_in_months, conf_interv = conf_interv,
                                             header=c('portfolio', 'retornoEsperado', 'limiteInferior','limiteSuperior'))
      }
      for (curr_i in add_currs){
        ind_assets_curr <- currs==curr_i
        mu <- temp_df_asset_port[assets_temp,]$retornoEsperado[ind_assets_curr]
        names(mu) <- temp_df_asset_port[assets_temp,]$portfolio[ind_assets_curr]
        Sigma <- covar(temp_returns[,assets_temp])$cov_matrix_ann[ind_assets_curr,ind_assets_curr,drop=FALSE]
        views_abs_curr <- views_abs[views_abs$asset %in% assets_temp[ind_assets_curr],]
        if(nrow(views_abs_curr)>0){
          q <- (1+temp_df_asset_port$retornoEsperado[temp_df_asset_port$portfolio==curr_i]) * views_abs_curr$ret
          P <- matrix(0, length(q), length(mu))
          for (i in 1:length(q)){P[i,match(views_abs_curr$asset[i], names(mu))] <- 1}
          post_ret <- posterior_params(mu,q,1,Sigma,P,conf = 0.5)$post_ret
          ret_shift <- post_ret - mu
          pos_assets <- match(names(mu),temp_df_asset_port$portfolio)
          temp_df_asset_port[pos_assets,2:4] <- temp_df_asset_port[pos_assets,2:4] + ret_shift
        }
      }

      ret_shift = sum(w * temp_df_asset_port$retornoEsperado[match(names(w), temp_df_asset_port$portfolio)]) - temp_port_data$retornoEsperado
      temp_port_data[,2:4] <- temp_port_data[,2:4] + ret_shift
    }

    temp_df_asset <- rbind(temp_df_asset, temp_port_data)
  }

  DBI::dbCommit(conn)
  poolReturn(conn)

  return(temp_df_asset)
}


