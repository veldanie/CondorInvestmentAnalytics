
#' Covariance matrix and volatilities.
#'
#' Takes the risk aversion coefficient, the covariance matrix and the market cap weights.
#' @param series data frame with return series.
#' @param per returns period expressed as number of periods per year.
#' @param shrink Shrink estimator. Lendoit and Wolf (2003).
#' @return Covariance matrix and volatility of each variable.
#' @export


covar <- function(series, per = 12, shrink = FALSE) {
  cov_matrix <- cov(as.matrix(series))
  cor_matrix <- cor(as.matrix(series))

  if(shrink){
    n_obs <- nrow(series)
    n_assets <- ncol(cov_matrix)
    r_avg <- 2 * sum(cor_matrix[row(cor_matrix) > col(cor_matrix)])/((n_assets - 1) * n_assets)
    f_matrix <- pi_matrix <- theta_matrix <- cov_matrix

    for (i in 1:(n_assets-1)){
      for (j in (i+1):n_assets){
        f_matrix[i,j] <- f_matrix[j,i] <- r_avg * sqrt(cov_matrix[i,i] * cov_matrix[j,j])
        pi_matrix[i,j] <- pi_matrix[j,i] <- sum((as.numeric(series[,i] - mean(series[,i])) * as.numeric(series[,j] - mean(series[,j])) - cov_matrix[i,j])**2)/n_obs
        theta_matrix [i, j] <- sum((as.numeric(series[,i] - mean(series[,i]))**2 - cov_matrix[i,i])*(as.numeric(series[,i] - mean(series[,i])) * as.numeric(series[,j] - mean(series[,j])) - cov_matrix[i,j]))/n_obs
        theta_matrix [j, i] <- sum((as.numeric(series[,j] - mean(series[,j]))**2 - cov_matrix[j,j])*(as.numeric(series[,i] - mean(series[,i])) * as.numeric(series[,j] - mean(series[,j])) - cov_matrix[i,j]))/n_obs
      }
    }

    rho_temp <- 0
    for (i in 1:n_assets){
      for (j in 1:n_assets){
        if(i!=j){
          rho_temp <- rho_temp + sqrt(cov_matrix[j,j]/cov_matrix[i,i]) * theta_matrix [i, j] + sqrt(cov_matrix[i,i]/cov_matrix[j,j])* theta_matrix [j, i]
        }
      }
    }
    pi_hat <- sum(pi_matrix)
    rho_hat <- sum(diag(pi_matrix)) + 0.5 * r_avg * rho_temp
    gamma_hat <- sum((f_matrix - cov_matrix)**2)
    k <- (pi_hat - rho_hat)/gamma_hat
    delta <- max(0, min(k/n_obs,1))
    cov_matrix <- delta * f_matrix + (1-delta)*cov_matrix
  }

  cov_matrix_ann <- cov_matrix * per
  vol <- sqrt(diag(cov_matrix)*per)
  return(list(cov_matrix = cov_matrix, cor_matrix = cor_matrix, cov_matrix_ann = cov_matrix_ann, vol = vol))
}


