#' Max Drawdown Return
#'
#' Estimates de covariance using weighted averages of products of past returns.
#' @param per Dates period.
#' @param w Portfolio weights.
#' @param series data frame series.
#' @param type Type of returns: arithmetic (discrete) or log (continuous)
#' @return Max Drawdown for particular period.
#' @export

max_drawdown <- function(per, w, series, type){
  series_per <- series[per]
  w_mat <- rep(1, nrow(series_per)) %*% t(w)

  if(type == 'log')
    series_ret <- w_mat * log(series_per/(rep(1, nrow(series_per)) %*% head(series[per], 1)))
  else{
    series_ret <- w_mat * (series_per/(rep(1, nrow(series_per)) %*% head(series[per], 1)) - 1)
  }
  port_ret <- as.numeric(apply(series_ret,1,sum))
  dd_obs <- max(port_ret) - tail(port_ret,1)
  return(dd_obs)
}
