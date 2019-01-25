
#' Index Series
#'
#' Estimates portfolio mean return, variance and annualized volatility.
#' @param series Original series,
#' @param backup_series Series used to complete te original series.
#' @return index series
#' @export

complete_index_series <- function(series, backup_series){
  date_ser <- index(series)[1]
  date_ini <- index(backup_series)[1]
  if(date_ser <= date_ini){
    comp_series <- series
  }else{
    rets <- as.vector(returns(backup_series, type='log')[paste0(c(date_ini, date_ser), collapse = '/')])
    series_ini <- xts(as.numeric(series[date_ser])*rev(cumprod(rev(exp(-rets)))), order.by=index(backup_series[paste0(c(date_ini, date_ser-1), collapse = '/')]))
    comp_series <- rbind(series_ini, series)
  }
  return(comp_series)
}
