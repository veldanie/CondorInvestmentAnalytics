#' Porfolio fx hedge
#'
#' Estimates the static hedging ratio per group. See Schmittmann (2010).

#' @param w Portfolio weights.
#' @param ref_curr Reference currency.
#' @param asset_data Datafrane with info regarding each asset.
#' @param series_list xts series (indexes and currencies).
#' @param series_fxfwd_list xts fx forward series. Outrights and forward premiums.
#' @param dates Initial and final dates.
#' @param hold_per Holding period return. 1M or 3M.
#' @param exp_ret Expected return over initial capital (1) over investment horizon.Given the difficulties to estimate the this value, it is assumed to be cero.
#' @param group.by Set of assets that share the same hedging ratio. By default the grouping is per currency. c("All", "Asset_Class", "Asset", "Currency", "Country")
#' @param bounded Hedge ratio bounded to the range 0-1.
#' @param invest_assets Investable asset. By default: Index. It can be set to ETF or IA (investable asset).
#' @return Fx hedge returns.
#' @export


portfolio_fx_hedge <- function(w, ref_curr, asset_data, series_list, series_fxfwd_list, dates, hold_per = '1M', exp_ret = 0, group.by = 'Asset', bounded = TRUE, invest_assets = NULL) {

  if(!(hold_per %in% c('1M', '3M'))){stop('Holding period not supported!')}
  per <- switch(hold_per, '1M' = 'monthly', '3M' = 'quarterly')
  n_assets <- length(w)
  asset_univ <- names(w)

  if(!is.null(invest_assets) && invest_assets == 'ETF'){
    index_curr <- asset_data$CurrencyETF[match(asset_univ, asset_data$Asset)]
  }else if (!is.null(invest_assets) && invest_assets == 'IA'){
    index_curr <- asset_data$CurrencyIA[match(asset_univ, asset_data$Asset)]
  }else{
    index_curr <- asset_data$Currency[match(asset_univ, asset_data$Asset)]
  }

  quotes_curr <- sapply(index_curr, iso_quote, curr2 = ref_curr)

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

  rets <- returns(series, period = per, type = 'arithmetic') #Returns in foreign (local) currency
  rets_ref <- returns(series_ref, period = per, type = 'arithmetic') #Returns in reference currency


  #Forward premium: Forward premium per available currency with respect to ref curr. It builds cross outrights when required.
  fwd_active <- currencies[currencies != ref_curr]
  if(length(fwd_active) == 0){warning("No existe riesgo de tasa de cambio en el portafolio."); return(NULL)}

  series_fwd_prem <- NULL

  for (fx in currencies){
    i_curr <- ifelse(any(c(fx, ref_curr) == 'USD'), c(fx, ref_curr)[c(fx, ref_curr)!= "USD"], fx)
    i_iso <- iso_quote(fx, ref_curr)
    i_fxfwd <- paste0(i_curr, hold_per)
    ref_fxfwd <- paste0(ref_curr, hold_per)
    if(fx == ref_curr){
        next
    }else{
      series_fwd_prem <- merge.xts(series_fwd_prem, xts(series_fxfwd_list[[i_fxfwd]][,2][findInterval(index(rets),index(series_fxfwd_list[[i_fxfwd]]))], order.by = index(rets)))
      if(any(c(fx, ref_curr) == 'USD')){
        if(substr(i_iso,1,3)==ref_curr){ #Forward and premium with foreign currency as numeraire.
          series_fxfwd_list[[i_fxfwd]][,1] <- 1/series_fxfwd_list[[i_fxfwd]][,1]
          series_fxfwd_list[[i_fxfwd]][,2] <- 1/(1+series_fxfwd_list[[i_fxfwd]][,2])-1
          fwd_prem <- as.vector(series_fxfwd_list[[i_fxfwd]][findInterval(index(rets),index(series_fxfwd_list[[i_fxfwd]])),2])

        }else{
          fwd_prem <- as.vector(series_fxfwd_list[[i_fxfwd]][findInterval(index(rets),index(series_fxfwd_list[[i_fxfwd]])),2])
        }
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

          fwd_prem <- series_fxfwd_list[[i_fxfwd]][findInterval(index(rets),index(series_fxfwd_list[[i_fxfwd]])),2]
      }
    }
    rets <- merge.xts(rets, fwd = c(NA, fwd_prem[-length(fwd_prem)]))# Remove last row to sync with return data.

  }

  rets <- rets[-1,]
  rets_ref <- rets_ref[-1,]
  names(rets) <- c(asset_univ, currencies, paste0(fwd_active, hold_per))

  fwd_active_iso <- sapply(fwd_active, function(x) ifelse(any(c(x, ref_curr) == 'USD') && (x != ref_curr), c(x, ref_curr)[c(x, ref_curr)!= "USD"], x))
  names(series_fwd_prem) <- paste0(fwd_active, hold_per)


  #Hedgeable assets. Asset than can be hedged bec. 1) i_curr != ref_curr, 2) available fwd.
  if(!is.null(invest_assets) && invest_assets == 'ETF'){
    h_asset_univ <- asset_data %>% filter(Asset %in% asset_univ)  %>%
                    filter(CurrencyETF != ref_curr) %>% filter(CurrencyETF %in% fwd_active) %>%
                    select(Asset) %>% unlist()
  }else if (!is.null(invest_assets) && invest_assets == 'IA'){
    h_asset_univ <- asset_data %>% filter(Asset %in% asset_univ)  %>%
                    filter(CurrencyIA != ref_curr) %>% filter(CurrencyIA %in% fwd_active) %>%
                    select(Asset) %>% unlist()
  }else{
    h_asset_univ <- asset_data %>% filter(Asset %in% asset_univ)  %>%
                    filter(Currency != ref_curr) %>% filter(Currency %in% fwd_active) %>%
                    select(Asset) %>% unlist()
  }


  #Unhedged returns in ref_curr:
  rets_u <- rets_ref[, h_asset_univ]

  #Matrix: fx_ret - fwd_premium.
  e_f <- rets_u
  for (asset in h_asset_univ){
    if(!is.null(invest_assets) && invest_assets == 'ETF'){
      i_curr <- asset_data %>% filter(Asset == asset) %>% select(CurrencyETF) %>% unlist()
    }else if (!is.null(invest_assets) && invest_assets == 'IA'){
      i_curr <- asset_data %>% filter(Asset == asset) %>% select(CurrencyIA) %>% unlist()
    }else{
      i_curr <- asset_data %>% filter(Asset == asset) %>% select(Currency) %>% unlist()
    }
    e_f[, asset] <- rets[, i_curr] - rets[, paste0(i_curr, hold_per)]
  }

  #Matrix: fx_ret - fwd_premium considering grouping factor.
  #c("All", "Asset_Class", "Asset", "Currency", "Country")
  factors <- h_asset_univ
  e_f_factors <- e_f
  w_fact <- w[h_asset_univ]

  if(group.by == 'ALL'){
    factors = 'all'
    e_f_factors <- xts(apply(e_f, 1, sum), order.by = index(e_f))
    w_fact <- 1
    names(e_f_factors) <- w_fact <- 'all'
  }else if(group.by != 'Asset'){
    e_f_factors <- w_fact <- NULL

    factors <- unique(asset_data %>% filter(Asset %in% h_asset_univ) %>% select_(group.by) %>% unlist())
    for (fac in factors){
      asset_data_fac <- asset_data %>% filter(Asset %in% h_asset_univ)
      fac_assets <- asset_data_fac[asset_data_fac[[group.by]]==fac,] %>% select(Asset) %>% unlist()
      e_f_factors <- merge.xts(e_f_factors, xts(as.numeric(apply(e_f[, fac_assets], 1, sum)), order.by = index(e_f)), check.names=FALSE)
      w_fact <- c(w_fact, sum(w[fac_assets]))
    }
    names(e_f_factors) <- names(w_fact) <- factors
  }


  hedge <- rep(0, n_assets)#Hedge vector per asset

  #Portfolio hedged return:
  rets_pu <- xts(rets_ref %*% w[asset_univ], order.by = index(rets_ref))

  # Option 1. Regress all factor. Problem of multicol. It might not allow us to separate the hedge ratio of each factor if they have the same currency.
  # fit <- lm(rets_pu ~ ., data = e_f_factors)
  # coeff <- fit$coefficients[-1]
  # names(coeff) <- gsub("`", "", names(coeff))

  # Option 2. Run a regression for each factor.

  coeff <- NULL
  for (fi in 1:length(factors)){
    fit <- lm(rets_pu ~ e_f_factors[,fi])
    coeff <- c(coeff, fit$coefficients[-1])
  }
  names(coeff) <- factors
  hedge_ratio <- coeff/w_fact[names(coeff)]
  hedge_ratio[is.infinite(hedge_ratio)] <- 0

  if (bounded){
    hedge_ratio <- hedge_ratio * (!(hedge_ratio < 0 | hedge_ratio > 1)) + 1*(hedge_ratio > 1)
  }

  asset_group <- asset_data %>% filter(Asset %in% asset_univ)  %>% select_(~Asset, group.by) %>% mutate(Hedge = 0)

  for (fac in names(hedge_ratio)){
    asset_group$Hedge[which(asset_group[[group.by]] == fac & asset_group$Asset %in% h_asset_univ)] <- hedge_ratio[fac]
  }
  # Hedged Portfolio return:
  rets_ph <- rets_pu - e_f_factors %*% (hedge_ratio * w_fact)

  port_hp_returns <- merge.xts(rets_pu, rets_ph, join = 'inner')

  port_ret_summ <- apply(port_hp_returns, 2, function(x) c(mean(x), sd(x), min(x), max(x)))
  rownames(port_ret_summ) <- c('Mean', 'Std_Dev', 'Min', 'Max')

  return(list(port_hp_returns = port_hp_returns, port_ret_summ = port_ret_summ, hedge_tab = asset_group, fwd_prem = series_fwd_prem))
}
