
#' Estimates volatility and TE risk attributions and contribution.
#'
#' Estimates risk attributions per factor for volatility, VaR and CVaR assuming normality..
#' @param w Portfolio weights.
#' @param series Asset series.
#' @param Sigma Covariance matrix
#' @return Vol risk attribution.
#' @export

vol_atrib <- function(w, series=NULL, Sigma=NULL, period='monthly') {
  # Given the returns estimation error, we assume cero mean,
  if(is.null(series) & is.null(Sigma)){stop("Both series and Sigma canno be NULL!")}
  if(is.null(Sigma)){
    freq <- switch(period, 'daily' = 252, 'monthly' = 12, 'quarterly' = 4, 'semiannualy' = 2)
    ret_series <- returns(series, period = period)
    Sigma <- cov(ret_series) * freq
  }
  port_vol <- sqrt(t(w) %*% Sigma %*% w)
  c_ip <- w * (t(w) %*% Sigma)
  vol_marg <- c_ip / port_vol
  vol_contrib <- vol_marg / port_vol

  return(list(vol_contrib = vol_contrib, vol_marg = vol_marg))
}
