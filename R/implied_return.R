
#' Equilibrium returns
#'
#' Takes the risk aversion coefficient, the covariance matrix and the market cap weights.
#' @param lambda risk aversion coefficient.
#' @param w Named vector of market capitalization weigths.
#' @param Sigma covariance matrix.
#' @param per returns period expressed as number of periods per year.
#' @param rfr Risk free rate conrresponding to the same period.
#' @return Named vector of equilibrium returns. Annualized and not. The return is assumed to be continuous, i.e. estimated using natural log.
#' @export

implied_return <- function(lambda, Sigma, w, per = 12, rfr = 0) {
  w_sort <- w[match(names(w), colnames(Sigma))]
  implied_ret <- as.vector((lambda * Sigma %*% w_sort))
  names(implied_ret) <- names(w_sort)
  implied_ret_annual <- implied_ret * per


  return(list(implied_ret = implied_ret, implied_ret_ann = implied_ret_annual))
}
