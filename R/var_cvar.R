
#' Value at Risk and Conditional Value at risk.
#'
#' Estimates losses distribution, VaR amd CVaR.
#' @param series data frame with return series.
#' @param w Portfolio weights.
#' @param quant Quantile.
#' @param normal Indicator if returns are normaly distributed.
#' @return Losses distribution, covar matrix, volatility, VaR and CVaR.
#' @export

var_cvar <- function(series, w, quant, normal = FALSE) {
  neg_rets <- -1 * as.numeric(series %*% w)
  covar_mat <- cov(series)
  port_vol <- sqrt(as.numeric(t(w) %*% covar_mat %*% w))

  if(normal){
    var <- qnorm(quant) * port_vol
    cvar <- dnorm(qnorm(quant))/(1-quant) * port_vol
  }else{
    var <- quantile(neg_rets, probs = quant)
    cvar <- mean(neg_rets[neg_rets > quantile(neg_rets, probs = quant)])
  }
  return(list(covar_mat = covar_mat, port_neg_rets = neg_rets, port_vol = port_vol, var = var, cvar = cvar))
}
