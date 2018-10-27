
#' Mean vector and Covariance matrix.
#'
#' Takes the risk aversion coefficient, the covariance matrix and the market cap weights.
#' @param series data frame with series.
#' @param asset_df Assets data frame with characteristics.
#' @param period Returns period .
#' @return Mean vector and Covariance matrix.
#' @export


params_simul <- function(series, mu_in = NA, sigma_in = NA, period="monthly"){
  freq <- switch(period, 'monthly' = 12, 'quarterly' = 4, 'yearly' = 1)
  rets <- returns(series, period = period)
  asset_names <- colnames(series)

  mu <- colMeans(rets) * freq
  SIGMA <- covar(rets)$cov_matrix # No se reescala en este paso.
  sigma <- sqrt(diag(SIGMA))

  if(any(!is.na(mu_in))){
    pos_mu <- !is.na(mu_in)
    mu_new <- mu_in[pos_mu]

    mu[names(mu_new)] <- mu_new
  }
  if(any(!is.na(sigma_in))){
    pos_sigma <- !is.na(sigma_in)
    sigma_new <- sigma_in[pos_mu]/sqrt(freq)
    sigma[names(sigma_new)] <- sigma_new
    SIGMA <-diag(sigma)%*%covar(rets)$cor_matrix%*%diag(sigma)
  }
  colnames(SIGMA) <- rownames(SIGMA) <- names(sigma)
  return(list(mu = mu, SIGMA = SIGMA))
}
