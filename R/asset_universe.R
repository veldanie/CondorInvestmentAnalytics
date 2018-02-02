#' Asset Universe
#'
#' Estimates returns with a given period.
#' @param mean_rets Named vector of mean returns.
#' @param cov_matrix Covariance matrix.
#' @param lambda Penalty.
#' @return Vector of asset names selected.
#' @export

asset_universe <- function(mean_rets, cov_matrix, lambda) {

  n_assets <- length(mean_rets)
  fitness_fun <- function(x) sum(mean_rets[x == 1])+lambda*sum(cov_matrix[x == 1,x == 1])

  asset_ind <- ga(type = "binary", fitness = fitness_fun, popSize = 100, pmutation = .1, nBits = n_assets)@solution
  names(mean_rets[asset_ind == 1])
  return(names(mean_rets[asset_ind == 1]))
}
