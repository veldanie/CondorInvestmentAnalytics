
#' Value at Risk and Conditional Value at risk.
#'
#' Estimates losses distribution, VaR amd CVaR.
#' @param series data frame with return series.
#' @param w Portfolio weights.
#' @param quant Quantile
#' @return Losses distribution, VaR and CVaR.
#' @export

var_cvar <- function(series, w, quant) {
  rets <- -1 * as.vector(apply(series, 1, function(x){as.numeric(x %*% w)}))
  var <- quantile(neg_rets, probs = quant)
  cvar <- mean(neg_rets[neg_rets > quantile(rets, probs = quant)])
  return(list(port_neg_rets = neg_rets, var = var, cvar = cvar))
}
