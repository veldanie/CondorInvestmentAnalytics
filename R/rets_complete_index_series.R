
#' Index Series
#'
#' Estimates portfolio mean return, variance and annualized volatility.
#' @param series Original series,
#' @param backup_series Series index used to complete original series index.
#' @param ref_dates reference dates
#' @return index series
#' @export

rets_complete_index_series <- function(series, backup_series, ref_dates='2000/2017'){

  all_dates <- index(backup_series)
  ini_date <-index(series)[1]
  ini_val <- as.numeric(series[1])

  target_dates <- all_dates[all_dates<ini_date]

  set.seed(42)
  random_rets <- rnorm(length(target_dates), mean(ingrtas_ret), sd(ingrtas_ret))
  rets <- as.vector(coredata(returns(series[ref_dates], type = 'log')))
  series_complete <- rbind(series, xts(rev(ini_val * exp(-cumsum(random_rets))), order.by = target_dates))
  return(series_complete)
}
