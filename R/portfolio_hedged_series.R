#' portfolio_hedged_series
#'
#' Calculates hedged portfolio index series.

#' @param w Portfolio weights.
#' @param ref_curr Reference currency.
#' @param asset_data Datafrane with info regarding each asset.
#' @param series_list xts series (indexes and currencies).
#' @param series_fxfwd_list xts fx forward series. Outrights and forward premiums.
#' @param dates Initial and final dates.
#' @param hold_per Holding period return. 1M or 3M.
#' @param val_ini Initial index value.
#' @param invest_assets Investable asset. By default: Index. It can be set to ETF or IA (investable asset).
#' @param hedge_ratio hedge ratio.
#' @return Fx hedge returns.
#' @export

portfolio_hedged_series <- function(w, ref_curr, asset_data, series_list, series_fxfwd_list, dates, hold_per = '1M', val_ini = 100, invest_assets = NULL, hedge_ratio = 1) {

  if(!(hold_per %in% c('1M', '3M'))){stop('Holding period not supported!')}
  per <- switch(hold_per, '1M' = 'monthly', '3M' = 'quarterly')
  days_per <- switch(hold_per, '1M' = 30, '3M' = 90)
  n_assets <- length(w)
  asset_univ <- names(w)

  if(!is.null(invest_assets) && invest_assets == 'ETF'){
    index_curr <- asset_data$CurrencyETF[match(asset_univ, asset_data$Asset)]
  }else if (!is.null(invest_assets) && invest_assets == 'IA'){
    index_curr <- asset_data$CurrencyIA[match(asset_univ, asset_data$Asset)]
  }else{
    index_curr <- asset_data$Currency[match(asset_univ, asset_data$Asset)]
  }

  currencies <- unique(index_curr)
  currencies_h <- c(currencies[currencies != ref_curr], ref_curr)
  #Adjust dates to fx forwards available:
  if(length(currencies_h)>1){
    min_fx_date <- max(sapply(currencies_h, function(fx) index(series_fxfwd_list[[paste0(ifelse(any(c(fx, ref_curr) == 'USD'), c(fx, ref_curr)[c(fx, ref_curr)!= "USD"], fx), hold_per)]])[1]))
    dates[1] <- max(dates[1], min_fx_date)
  }
  #Prices in foreign currency, and currencies using foreign as numeraire.
  series <- series_merge(series_list, dates, asset_data, ref_curr, asset_univ, currencies, convert_to_ref = FALSE, ref_per_unit_foreign = TRUE)

  #Index Prices converted to ref curr.
  series_ref <- series_merge(series_list, dates, asset_data, ref_curr, asset_univ, convert_to_ref = TRUE, ref_per_unit_foreign = FALSE)

  rets <- returns(series, period = "daily", type = 'arithmetic') #Returns in foreign (local) currency
  rets_ref <- returns(series_ref, period = "daily", type = 'arithmetic') #Returns in reference currency

  #Forward premium: Forward premium per available currency with respect to ref curr. It builds cross outrights when required.
  fwd_active <- currencies[currencies != ref_curr]
  if(length(fwd_active) == 0){warning("No existe riesgo de tasa de cambio en el portafolio."); return(NULL)}

  for (fx in currencies){
    i_curr <- ifelse(any(c(fx, ref_curr) == 'USD'), c(fx, ref_curr)[c(fx, ref_curr)!= "USD"], fx)
    i_iso <- iso_quote(fx, ref_curr)
    i_fxfwd <- paste0(i_curr, hold_per)
    ref_fxfwd <- paste0(ref_curr, hold_per)
    if(fx == ref_curr){
        next
    }else{
      if(any(c(fx, ref_curr) == 'USD')){
        if(substr(i_iso,1,3)==ref_curr){ #Forward and premium with foreign currency as numeraire.
          series_fxfwd_list[[i_fxfwd]][,1] <- 1/series_fxfwd_list[[i_fxfwd]][,1]
          series_fxfwd_list[[i_fxfwd]][,2] <- 1/(1+series_fxfwd_list[[i_fxfwd]][,2])-1
        }
        fwd_prem <- as.vector(series_fxfwd_list[[i_fxfwd]][findInterval(index(rets),index(series_fxfwd_list[[i_fxfwd]])),2])/days_per
      }else{
          iso_cross <- paste0(fx, ref_curr) #In this case cross rates are built as ref currency per unit of foreign.
          series_temp_fxsp <- merge.xts(series_fxfwd_list[[i_fxfwd]][,1]/(1+series_fxfwd_list[[i_fxfwd]][,2]),
                                        series_fxfwd_list[[ref_fxfwd]][,1]/(1+series_fxfwd_list[[ref_fxfwd]][,2]), join = 'inner')
          series_temp_fxfwd <- merge.xts(series_fxfwd_list[[i_fxfwd]][,1], series_fxfwd_list[[ref_fxfwd]][,1], join = 'inner')
          series_fxsp_cross <- xts(x = as.vector(mapply(fx_cross, fx_base = series_temp_fxsp[,1], fx_ref = series_temp_fxsp[,2],
                                                         MoreArgs = list(base_curr = substr(iso_cross,1,3), ref_curr = substr(iso_cross,4,6),
                                                         curr_mkt_base = iso_quote(substr(iso_cross,1,3)),
                                                         curr_mkt_ref = iso_quote(substr(iso_cross,4,6))))), order.by = index(series_temp_fxsp))
          series_fxfwd_cross <- xts(x = as.vector(mapply(fx_cross, fx_base = series_temp_fxfwd[,1], fx_ref = series_temp_fxfwd[,2],
                                                         MoreArgs = list(base_curr = substr(iso_cross,1,3), ref_curr = substr(iso_cross,4,6),
                                                         curr_mkt_base = iso_quote(substr(iso_cross,1,3)),
                                                         curr_mkt_ref = iso_quote(substr(iso_cross,4,6))))), order.by = index(series_temp_fxfwd))
          series_premfwd_cross <-series_fxfwd_cross[,1]/series_fxsp_cross[,1]-1
          series_fxfwd_list[[i_fxfwd]] <- merge.xts(series_fxfwd_cross, series_premfwd_cross)
          names(series_fxfwd_list[[i_fxfwd]]) <- c("outright", "premium")

          fwd_prem <- as.vector(series_fxfwd_list[[i_fxfwd]][findInterval(index(rets),index(series_fxfwd_list[[i_fxfwd]])),2])/days_per
      }
    }
    rets <- merge.xts(rets, fwd = fwd_prem)# Remove last row to sync with return data.
  }
  names(rets) <- c(asset_univ, currencies, paste0(fwd_active, hold_per))


  #Hedgeable assets. Asset than can be hedged bec. 1) i_curr != ref_curr, 2) available fwd.
  if(!is.null(invest_assets) && invest_assets == 'ETF'){
    h_asset_univ <- asset_data %>% filter(Asset %in% asset_univ)  %>%
                    filter(CurrencyETF != ref_curr) %>% filter(CurrencyETF %in% fwd_active) %>%
                    dplyr::select(Asset) %>% unlist()
  }else if (!is.null(invest_assets) && invest_assets == 'IA'){
    h_asset_univ <- asset_data %>% filter(Asset %in% asset_univ)  %>%
                    filter(CurrencyIA != ref_curr) %>% filter(CurrencyIA %in% fwd_active) %>%
                    dplyr::select(Asset) %>% unlist()
  }else{
    h_asset_univ <- asset_data %>% filter(Asset %in% asset_univ)  %>%
                    filter(Currency != ref_curr) %>% filter(Currency %in% fwd_active) %>%
                    dplyr::select(Asset) %>% unlist()
  }

  #Unhedged returns in ref_curr:
  rets_u <- rets_ref[, h_asset_univ]

  #Matrix: fx_ret - fwd_premium.
  e_f <- rets_u
  for (asset in h_asset_univ){
    if(!is.null(invest_assets) && invest_assets == 'ETF'){
      i_curr <- asset_data %>% filter(Asset == asset) %>% dplyr::select(CurrencyETF) %>% unlist()
    }else if (!is.null(invest_assets) && invest_assets == 'IA'){
      i_curr <- asset_data %>% filter(Asset == asset) %>% dplyr::select(CurrencyIA) %>% unlist()
    }else{
      i_curr <- asset_data %>% filter(Asset == asset) %>% dplyr::select(Currency) %>% unlist()
    }
    e_f[, asset] <- rets[, i_curr] - rets[, paste0(i_curr, hold_per)]
  }

  w_fact <- w[h_asset_univ]

  #Portfolio hedged return:
  rets_pu <- xts(rets_ref %*% w[asset_univ], order.by = index(rets_ref))
  ser_pu <- xts(cumprod(c(val_ini, 1 + as.vector(coredata(rets_pu)))), order.by = c(index(series_ref)[1], index(rets_pu)))
  # Hedged Portfolio return:

  rets_ph <- rets_pu - e_f %*%  (hedge_ratio*w_fact)
  ser_ph <- xts(cumprod(c(val_ini, 1 + as.vector(coredata(rets_ph)))), order.by = c(index(series_ref)[1], index(rets_ph)))

  return(list(port_hedged_series = ser_ph, port_unhedged_series = ser_pu, port_hedged_returns = rets_ph, port_unhedged_returns = rets_pu))
}
