
#' Index Series
#'
#' Estimates portfolio mean return, variance and annualized volatility.
#' @param ref_dates reference dates
#' @param mu Returns mean
#' @param sigma Return sd
#' @param index_val Index initial value
#' @return index series
#' @export

rets_create_index_series <- function(ref_dates, mu, sigma, index_val = 100){
  set.seed(42)

  random_rets <- rnorm(length(ref_dates)-1, mu, sigma)
  series <- xts(index_val * cumprod(c(1, 1+random_rets)), order.by = ref_dates)
  return(series)
}
