
#' Series normalization.
#'
#' Series normalization.
#' @param series Xts series
#' @param val_ini Initial value.
#' @return Normalized series.
#' @export

series_norm <- function(series, val_ini = 100){
  rets <- returns(series, period = "daily", type = "log")
  cum_rets <- rbind(xts(t(rep(0,ncol(series))), order.by = index(series)[1]), apply(rets, 2, cumsum))
  series_norm <- val_ini * exp(cum_rets)

  return(series_norm)
}
