
#' Series merge
#'
#' Series merge
#' @param series_list List of xts series.
#' @param dates Dates.
#' @param asset_data Datafrane with info regarding each asset.
#' @param ref_curr Id (iso) of reference currency.
#' @param assets Id of assets of interest.
#' @param currecies Id (iso) of currencies of interest.
#' @param convert_to_ref Indicator to define if series are converted to ref_curr.
#' @param ref_per_unit_foreign Indicator if currencies are converted to Base (ref) currency per unit of foreign currency.
#' @param invest_assets Investable asset. By default: Index. It can be set to ETF or IA (investable asset).
#' @return xts series.
#' @export

series_merge <- function(series_list, dates, asset_data, ref_curr, assets, currencies = NULL, convert_to_ref = FALSE, ref_per_unit_foreign = FALSE, invest_assets = NULL) {
  n_assets <- length(assets)
  n_curr <- length(currencies)

  if(!is.null(invest_assets) && invest_assets == 'ETF'){
    ticker <- asset_data$TickerETF[match(assets, asset_data$Asset)]
  }else if (!is.null(invest_assets) && invest_assets == 'IA'){
    ticker <- asset_data$TickerInvestAsset[match(assets, asset_data$Asset)]
  }else{
    ticker <- asset_data$TickerBenchmark[match(assets, asset_data$Asset)]
  }

  missing_ticker <- !(ticker %in% names(series_list))
  if(any(missing_ticker)){
    warning(paste0('Missing tickers: ', ticker[missing_ticker]))
    ticker[missing_ticker] <- asset_data$TickerBenchmark[match(assets, asset_data$Asset)]
  }

  series_out <- NULL
  if(!is.null(assets)){
    if (convert_to_ref){
      for (ind in ticker){
        if(!is.null(invest_assets) && invest_assets == 'ETF'){
          i_curr <- asset_data$CurrencyETF[match(ind, asset_data$TickerETF)]
        }else if (!is.null(invest_assets) && invest_assets == 'IA'){
          i_curr <- asset_data$CurrencyIA[match(ind, asset_data$TickerInvestAsset)]
        }else{
          i_curr <- asset_data$Currency[match(ind, asset_data$TickerBenchmark)]
        }
        if(i_curr == ref_curr){
          # Case 1: No currency issue.
          series_out <- merge.xts(series_out, series_list[[ind]][paste0(dates, collapse = '/')], join = 'inner')
        } else {
          if(any(c(i_curr, ref_curr) == 'USD')){
            # Case 2: Convert series to reference currency.
            i_curr_temp <- c(i_curr, ref_curr)[c(i_curr, ref_curr)!= "USD"]
            series_temp <- merge.xts(series_list[[ind]],  series_list[[i_curr_temp]], join = 'inner')
            series_conv <- xts(x = as.vector(mapply(cash_conv, cash_in = series_temp[,1], spot = series_temp[,2], MoreArgs = list(curr_in = i_curr, spot_id = iso_quote(i_curr_temp)))),
                              order.by = index(series_temp))
            series_out <- merge.xts(series_out, series_conv[paste0(dates, collapse = '/')], join = 'inner') #Merge con serie convertida a moneda de referencia.
          } else { # Se crean tasas cruzadas si se requiere
            # Case 3: Convert series to reference currency using cross rate.
            iso_cross <- paste0(ref_curr, i_curr)
            series_temp_fx <- merge.xts(series_list[[ind]],  merge.xts(series_list[[ref_curr]], series_list[[i_curr]], join = 'inner'), join = 'inner')
            series_fx_cross <- xts(x = as.vector(mapply(fx_cross, fx_base = series_temp_fx[,2], fx_ref = series_temp_fx[,3],
                                                        MoreArgs = list(base_curr = ref_curr, ref_curr = i_curr,
                                                                        curr_mkt_base = iso_quote(ref_curr),
                                                                        curr_mkt_ref = iso_quote(i_curr)))), order.by = index(series_temp_fx))
            series_temp <- merge.xts(series_list[[ind]],  series_fx_cross, join = 'inner')
            series_conv <- xts(x = as.vector(mapply(cash_conv, cash_in = series_temp[,1], spot = series_temp[,2], MoreArgs = list(curr_in = i_curr, spot_id = iso_cross))),
                               order.by = index(series_temp))
            series_out <- merge.xts(series_out, series_conv[paste0(dates, collapse = '/')], join = 'inner') #Merge con serie convertida a moneda de referencia.
          }
        }
      }
    }else{
      for(i in 1:n_assets){
        series_out<-merge.xts(series_out, series_list[[ticker[i]]][paste0(dates, collapse = '/')], join = "inner")
      }
    }
}
  if (!is.null(currencies)){
    for(i in 1:n_curr){
      if(currencies[i]==ref_curr){
        series_out <- merge.xts(series_out, 1)
      }else{
        if(any(c(currencies[i], ref_curr) == 'USD')){
            i_curr_temp <- c(currencies[i], ref_curr)[c(currencies[i], ref_curr)!= "USD"]
             #Currrency as base per unit of foreign:
            if(ref_per_unit_foreign && substr(iso_quote(i_curr_temp),1,3)==ref_curr){
              series_out <- merge.xts(series_out, 1/series_list[[i_curr_temp]][paste0(dates, collapse = '/')], join = "inner")
            }else{
              series_out <- merge.xts(series_out, series_list[[i_curr_temp]][paste0(dates, collapse = '/')], join = "inner")
            }
        }else{#Se construye tasa cruzada.
          iso_cross <- paste0(currencies[i], ref_curr) #In this case cross rates are built as ref currency per unit of foreign.
          series_temp_fx <- merge.xts(series_list[[currencies[i]]], series_list[[ref_curr]], join = 'inner')
          series_fx_cross <- xts(x = as.vector(mapply(fx_cross, fx_base = series_temp_fx[,1], fx_ref = series_temp_fx[,2],
                                                      MoreArgs = list(base_curr = substr(iso_cross,1,3), ref_curr = substr(iso_cross,4,6),
                                                                      curr_mkt_base = iso_quote(substr(iso_cross,1,3)),
                                                                      curr_mkt_ref = iso_quote(substr(iso_cross,4,6))))), order.by = index(series_temp_fx))
          series_out <- merge.xts(series_out, series_fx_cross[paste0(dates, collapse = '/')], join = "inner")
        }
      }
    }
  }
  names(series_out) <- c(assets, currencies)
  return(series_out)
}
