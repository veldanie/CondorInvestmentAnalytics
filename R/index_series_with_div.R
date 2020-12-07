
#' Index Series
#'
#' Estimates total return index series.
#' @param series Asset series.
#' @param backup_series Backup series.
#' @param threshold Value allow us to identify a div payment.
#' @return index series
#' @export

index_series_with_div <- function(series, backup_series, threshold=0, ref_years='2005/2020', discard_zeros=TRUE, complete_method_rets=TRUE, all_dates = FALSE){
  n_row <- nrow(series)
  diff_pr <- diff(as.vector(series))
  if (discard_zeros){
    pos_ret_all <- c(1, which(diff_pr!=0)+1)
    pos_ret <- which(diff_pr>threshold & diff_pr!=0)+1
  }else{
    pos_ret_all <- c(1, which(diff_pr!=0)+1)
    pos_ret <- which(diff_pr>threshold & diff_pr!=0)+1
  }
  rets <- (as.vector(series[pos_ret]) - as.vector(series[intersect(pos_ret, pos_ret_all)-1]))/as.vector(series[intersect(pos_ret, pos_ret_all)-1])
  if(tail(pos_ret, 1)!=n_row){
    rets <- c(rets, tail(rets, 1) * as.numeric(tail(index(series),1) - index(series)[tail(pos_ret,1)])/as.numeric(index(series)[tail(pos_ret,1)] - index(series)[tail(pos_ret,2)[1]]))
    index_monthly <- xts(1000 * cumprod(1+c(0,rets)), order.by = index(series)[c(1, pos_ret, n_row)])
  }else{
    index_monthly <- xts(1000 * cumprod(1+c(0,rets)), order.by = index(series)[c(1, pos_ret)])
  }
  if(is.null(backup_series)){
    series_out <- na.approx(index_monthly, xout = index(series))
  }else{
    series_index <- index(backup_series[paste0(c(index(index_monthly)[1], tail(index(series),1)), collapse = "/")])
    index_dayly <- na.approx(index_monthly, xout = series_index)
    if(complete_method_rets){
      series_out <- rets_complete_index_series(index_dayly, backup_series, "2000/2020")[ref_years]
    }else{
      series_out <- complete_index_series(index_dayly, backup_series)[ref_years]
    }
  }

  if(all_dates){
    first_date <- index(series_out)[1]
    last_date <- index(series_out)[length(index(series_out))]
    date_range <- seq(as.Date(first_date), by = "day", length.out = as.numeric(as.Date(last_date) - as.Date(first_date)))
    df_complete <- xts(x = rep(NA, length(date_range)), order.by = date_range)
    series_out <- na.locf(merge.xts(df_complete, series_out)[, 'series_out'])
  }

  return(series_out)
}
