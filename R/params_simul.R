
#' Mean vector and Covariance matrix.
#'
#' Takes the risk aversion coefficient, the covariance matrix and the market cap weights.
#' @param series data frame with series.
#' @param asset_df Assets data frame with characteristics.
#' @param period Returns period .
#' @return Mean vector and Covariance matrix.
#' @export

params_simul <- function(series, asset_df, period="monthly"){

  rets <- returns(series, period = period)
  mu <- colMeans(rets)*12
  mu_df <- asset_df$PROM[match(names(mu), asset_df$ACTIVO)]
  mu[!is.na(mu_df)] <- mu_df[!is.na(mu_df)]

  SIGMA <- covar(rets)$cov_matrix # No se reescala en este paso.
  sigma <- sqrt(diag(SIGMA))
  sigma_df <- asset_df$DESV[match(names(mu), asset_df$ACTIVO)]/sqrt(12)
  sigma[!is.na(sigma_df)] <- sigma_df[!is.na(sigma_df)]

  SIGMA <-diag(sigma)%*%covar(rets)$cor_matrix%*%diag(sigma)
  colnames(SIGMA) <- rownames(SIGMA) <- names(sigma)
  return(list(mu = mu, SIGMA = SIGMA))
}
