
#' Estimates tracking error attributions.
#'
#' Estimates te attributions per asset.
#' @param series Asset returns series.
#' @param w_act active weights.
#' @param covar_mat Covariance matrix
#' @return TE Atributions.
#' @export

te_atrib <- function(series, w, w_act) {
  # Given the returns estimation error, we assume cero mean,
  te_contrib <- w_act
  port_rets <- as.numeric(series %*% w)
  covar_mat <- cov(series)
  te_var <- as.numeric(t(w_act) %*% covar_mat %*% w_act)
  c_ip <- sapply(1:length(w_act), function(x) (w_act[x]*covar_mat[x,x] + sum(w_act[-x] * covar_mat[,x][-x])))

  if(te_var > 0){
    te_contrib <- w_act * c_ip / te_var
  }
  return(te_contrib)
}
