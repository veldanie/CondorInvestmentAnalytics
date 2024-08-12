
#' Porfolio return
#'
#' Estimates portfolio mean return, variance and annualized volatility.
#' @param weights Portfolio weights.
#' @param mean_returns Portfolio mean returns.
#' @param covar_mat Covariance matrix of returns.
#' @return Portfolio mean, variance and annualized volatility.
#' @export

portfolio_return <- function(weights, mean_returns, covar_mat) {
  if(sum(weights)==0){
    port_mean_ret <- port_var <- port_vol <- 0
  }else{
    id <- names(weights)
    if(is.null(id)){
      port_mean_ret <- as.numeric(t(weights) %*% mean_returns)
      port_var <- as.numeric(t(weights) %*% covar_mat %*% weights)
    }else{
      port_mean_ret <- as.numeric(t(weights) %*% mean_returns[id])
      port_var <- as.numeric(t(weights) %*% covar_mat[id, id] %*% weights)
    }
    port_vol <- sqrt(port_var)
  }
  return(list(port_mean_ret = port_mean_ret, port_var = port_var, port_vol = port_vol))
}
