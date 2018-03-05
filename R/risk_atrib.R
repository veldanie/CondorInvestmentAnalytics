
#' Estimates risk attributions per factor for volatility, VaR and CVaR assuming normality.
#'
#' Estimates risk attributions per factor for volatility, VaR and CVaR assuming normality..
#' @param series Portfolio weights.
#' @param w Portfolio weights.
#' @param covar_mat Covariance matrix
#' @param quant Quantile
#' @return Losses distribution, VaR and CVaR.
#' @export

risk_atrib <- function(series, w, quant, normal = FALSE) {
  # Given the returns estimation error, we assume cero mean,

  risk <- var_cvar (series, w, quant, normal)

  c_ip <- sapply(1:length(w), function(x) (w[x]*risk$covar_mat[x,x] + sum(w[-x] * risk$covar_mat[,x][-x])))

  vol_contrib <- w * c_ip / risk$port_vol ^ 2
  vol_marg <- c_ip / risk$port_vol

  #var_marg <- qnorm(quant) * vol_marg
  #var_contrib <- w * var_marg /port_var

  ind_es <- risk$port_neg_rets > risk$cvar
  es_marg <- apply((rep(-1, sum(ind_es)) %*% t(w)) * series[ind_es], 2, mean)
  es_contrib <- es_marg / risk$cvar
  es_contrib <- es_contrib/sum(es_contrib) #Normalized

  risk_marg <- rbind(vol_marg, es_marg)
  risk_contrib <- rbind(vol_contrib, es_contrib)

  return(list(risk_contrib = risk_contrib, risk_marg = risk_marg))
}
