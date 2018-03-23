
#' Porfolio returns
#'
#' Estimates de covariance using weighted averages of products of past returns.
#' @param series data frame with return series.
#' @param weights Weights. It can be a declining function of time.
#' @param horizon Horizon
#' @return Covariance matrix and volatility of each variable.
#' @export

port_return <- function(series, weights, horizon) {
  if (length(weights)!=nrow(series)){stop('Vector of weights does not concide with series matrix')}
  series_mat <- as.matrix(series)
  n_assets <- ncol(series)
  weights_mat <- matrix(rep(weights,each=n_assets), ncol=n_assets, byrow=TRUE)
  cov_matrix <- (weights_mat*series_mat)%*%series_mat/sum(weights)
  vol <- sqrt(diag(cov_matrix)*per)
  return(list(cov_matrix = cov_matrix, vol = vol))
}
