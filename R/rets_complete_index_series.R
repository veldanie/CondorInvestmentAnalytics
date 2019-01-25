
#' Index Series
#'
#' Estimates portfolio mean return, variance and annualized volatility.
#' @param series Original series,
#' @param backup_series Series index used to complete original series index.
#' @param ref_dates reference dates
#' @param mu Returns mean
#' @param sigma Return sd
#' @return index series
#' @export

rets_complete_index_series <- function(series, backup_series, ref_dates='2000/2017', mu = NA, sigma = NA){

  all_dates <- index(backup_series)
  ini_date <-index(series)[1]
  ini_val <- as.numeric(series[1])

  target_dates <- all_dates[all_dates<ini_date]
  rets <- as.vector(coredata(returns(series[ref_dates], type = 'log')))
  mu_rets <- ifelse(is.na(mu), mean(rets), mu)
  sigma_rets <- ifelse(is.na(sigma), sd(rets), sigma)

  set.seed(42)
  random_rets <- rnorm(length(target_dates), mu_rets, sigma_rets)
  series_complete <- rbind(series, xts(rev(ini_val * exp(-cumsum(random_rets))), order.by = target_dates))
  return(series_complete)
}
