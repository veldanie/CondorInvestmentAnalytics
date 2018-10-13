#' States simulation by Geometric Brownian Motion
#'
#' Takes some initial states and smulates trajectories.
#' @param spot Vector of initial states.
#' @param times Future times in years to simulate.
#' @param mu Drift.
#' @param SIGMA Volatility.
#' @param M Correlation matrix.
#' @param per Number of trajectories.
#' @return Simulated trajectories.
#' @export

spot_simul_mult <-function(spot, times, mu, SIGMA, M = 1000, per = 12){
  nact <- length(spot)
  t_mat <- times %*% t(rep(1, nact))
  sigma <- sqrt(diag(SIGMA)) * sqrt(per)
  nu <- mu - sigma^2/2
  N <- length(times)
  R <- chol(SIGMA)*sqrt(per)
  st <- array(0, dim = c(N, M, nact))

  for (i in 1:M) {
    x <- matrix(rnorm(N * nact), ncol = nact) * sqrt(diff(c(0, times)))
    w <- x %*% R
    wt <- apply(w, 2, cumsum)
    st[, i, ] <- exp((rep(1,N) %*% t(nu)) * t_mat + wt) %*% diag(spot)
  }
  return(st)
}
