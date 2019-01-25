#' States simulation by Geometric Brownian Motion
#'
#' Takes some initial states and smulates trajectories.
#' @param S0 Vector of initial states.
#' @param times Future times in years to simulate.
#' @param mu Drift.
#' @param sigma Volatility.
#' @param rho Correlation matrix.
#' @param M Number of trajectories.
#' @return Simulated trajectories.
#' @export

state_simul_mult <-function(S0, times, mu, sigma, rho, M=1000){
  n_states <- length(S0)
  t_rep  <- times%*%t(rep(1,n_states))
  SIGMA <- diag(sigma)%*%rho%*%diag(sigma)
  nu <- mu - sigma^2/2
  N <- length(times)
  R <- chol(SIGMA)
  S <- array(0, dim=c(N,M,n_states))

  for(i in 1:M){
    x <- matrix(rnorm(N*n_states),ncol=n_states)*sqrt(diff(c(0,times)))
    w <- x%*%R
    wt <- apply(w,2,cumsum)
    S[,i,] <- exp((rep(1, N) %*% t(nu))*t_rep+wt) %*% diag(S0)
  }
  return(S)
}
