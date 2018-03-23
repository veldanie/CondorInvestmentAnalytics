
#' Covariance matrix and volatilities.
#'
#' Takes the risk aversion coefficient, the covariance matrix and the market cap weights.
#' @param series data frame with return series.
#' @param per returns period expressed as number of periods per year.
#' @return Covariance matrix and volatility of each variable.
#' @export

covar <- function(series, per = 12) {
  cov_matrix <- cov(as.matrix(series))
  cor_matrix <- cor(as.matrix(series))
  cov_matrix_ann <- cov_matrix * per
  vol <- sqrt(diag(cov_matrix)*per)
  return(list(cov_matrix = cov_matrix, cor_matrix = cor_matrix, cov_matrix_ann = cov_matrix_ann, vol = vol))
}
