
#' Porfolio return
#'
#' Estimates portfolio mean return, variance and annualized volatility.
#' @param weights Portfolio weights.
#' @param mean_returns Portfolio mean returns.
#' @param covar_mat Covariance matrix of returns.
#' @return Portfolio mean, variance and annualized volatility.
#' @export

portfolio_return <- function(w, mean_returns, covar_mat) {
  port_mean_ret <- as.numeric(t(weights) %*% mean_returns)
  port_var <- as.numeric(t(weights) %*% covar_mat %*% weights)
  port_vol <- sqrt(port_var)
  return(list(port_mean_ret = port_mean_ret, port_var = port_var, port_vol = port_vol))
}
