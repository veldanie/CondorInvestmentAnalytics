
#' Value at Risk and Conditional Value at risk.
#'
#' Estimates losses distribution, VaR amd CVaR.
#' @param series returns series.
#' @param quant Quantile.
#' @param normal Indicator if returns are normaly distributed.
#' @return Losses distribution, covar matrix, volatility, VaR and CVaR.
#' @export

series_var_cvar <- function(series, quant, normal = FALSE) {
  neg_rets <- -1 * series
  vol <- sd(series)

  if(normal){
    var <- qnorm(quant) * vol
    cvar <- dnorm(qnorm(quant))/(1-quant) * vol
  }else{
    var <- quantile(neg_rets, probs = quant)
    cvar <- sapply(var, function(x) mean(neg_rets[neg_rets > x]))
  }
  return(list(neg_rets = neg_rets, vol = vol, var = var, cvar = cvar))
}
