
#' Series compose
#'
#' Series merge
#' @param series_list List of xts series.
#' @param asset_data Datafrane with info regarding each asset.
#' @param dates Dates.
#' @param tickers Tickers.
#' @param ref_curr Id (iso) of reference currency. If missing, we use currency of last ticker.
#' @return xts series.
#' @export

series_compose <- function(series_list, asset_data, assets_list, dates, ref_curr=NULL, join = 'inner') {
  n_list <- length(assets_list)
  ids <- names(assets_list)
  assets_list_out <- as.list(rep(NA, n_list))
  names(assets_list_out) <- ids
  series_out <- NULL
  currs <- rep("", n_list)
  for(i in 1:n_list){
    id <- ids[i]
    tickers_unordered <- assets_list[[id]]$tk
    dates_tk_unordered <- assets_list[[id]]$dates
    dates_tk <- dates_tk_unordered[order(dates_tk_unordered)]
    tickers <- tickers_unordered[order(dates_tk_unordered)]

    n_tk <- length(tickers)
    assets <- sapply(tickers, get_asset, asset_data)
    missing_ticker <- !(tickers %in% names(series_list))
    if(any(missing_ticker)){
      warning(paste0('Missing tickers: ', tickers[missing_ticker]))
      tickers[missing_ticker] <- asset_data$TickerBenchmark[match(assets, asset_data$Asset)]
    }

    if(is.null(ref_curr)){
      currs[i] <- asset_data$Currency[match(tail(assets, 1), asset_data$Asset)]
    }else{
      currs[i] <- ref_curr
    }
    series_tail <- series_merge(series_list = series_list, dates = c(tail(dates_tk, 1), Sys.Date()), asset_data = asset_data, ref_curr = currs[i], assets = tail(assets, 1), currencies = NULL, convert_to_ref = TRUE)

    rets <- NULL
    if(length(dates_tk)>1){
      for(j in 1:(length(dates_tk)-1)){
        rets <- rbind(rets, returns(series_merge(series_list = series_list, dates = c(dates_tk[j], dates_tk[j+1]), asset_data = asset_data, ref_curr = currs[i], assets = assets[j], currencies = NULL, convert_to_ref = TRUE), leading = FALSE))
      }
      backup_series <- xts(100 * cumprod(1 + as.vector(rets)), order.by = index(rets))
      if(length(series_tail) > 0){
        series_id <- complete_index_series(series_tail, backup_series)
      }else{
        series_id <- backup_series
      }
    }else{
      series_id <- series_tail
    }
    assets_list_out[[id]] <- list(series = series_out, curr = currs[i])
    series_out<-merge.xts(series_out, series_id[paste0(dates, collapse = '/')], join = join)
  }
  colnames(series_out) <- names(currs) <- ids
  return(list(series = series_out, currs = currs))
}
