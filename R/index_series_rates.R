
#' Index Series from market rates
#'
#' Index Series from market rates.
#' @param rates Market rates.
#' @param val_ini Initial value.
#' @param base Index, ETF.
#' @return index series
#' @export

index_series_rates <- function(rates, val_ini = 100, base = 365){
  index <- xts(cumprod(c(val_ini, 1+as.vector(coredata(rates))[-length(rates)] * diff(index(rates))/(base*100))), order.by = index(rates))
  return(index)
}
