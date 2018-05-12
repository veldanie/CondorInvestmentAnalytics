#' Asset Universe
#'
#' Suggests a set of assets.
#' @param mean_rets Named vector of mean returns. Can be implied returns.
#' @param cov_matrix (Annualized) Covariance matrix.
#' @param n_assets_univ Number of assets in the ivestable universe
#' @param lambda Correlation penalty.
#' @return Vector of asset names selected. The names are in order, meaning the i position in the output vector is the i-th asset selected.
#' @export

asset_universe <- function(mean_rets, cov_matrix, n_assets_univ, lambda = 0.1){

  vol <- sqrt(diag(cov_matrix))

  D <- diag(vol)
  DInv <- solve(D);
  cor_matrix <- DInv %*% cov_matrix %*% DInv
  colnames(cor_matrix) <- rownames(cor_matrix) <- colnames(cov_matrix)

  n_assets <- length(mean_rets)
  rank <- rep(0, n_assets)
  names(rank) <- names(mean_rets)

  for(n_var in 1:n_assets_univ){
    fitness_fun <- function(x) {
      if(sum(x) != n_var){res <- -Inf
      } else { res <- sum(mean_rets[x == 1]/vol[x == 1]) - lambda*sum(cor_matrix[x == 1,x == 1][lower.tri(cor_matrix[x == 1,x == 1])])}
      return(res)
    }
  asset_ind <- ga(type = "binary", fitness = fitness_fun, popSize = 100, pmutation = .1, nBits = n_assets)@solution
  rank <- rank + asset_ind
  }

  rank_order <- order(rank, decreasing = TRUE)[1:sum(asset_ind)]
  selected_assets <- names(mean_rets[rank_order])
  return(selected_assets)
}
