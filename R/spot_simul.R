#' State simulation by Geometric Brownian Motion
#'
#' Takes some initial state and simulates trajectories.
#' @param s0 Initial state.
#' @param times Future times in years to simulate.
#' @param mu Drift.
#' @param sigma Volatility.
#' @param M Number of trajectories.
#' @return Simulated trajectories.
#' @export

state_simul <- function(s0, times, mu, sigma, M){
  led=length(times)
  bm=apply(matrix(rnorm(M*led), ncol=M)*sqrt(c(times[1],diff(times))),2,cumsum)
  st=s0*exp((mu-0.5*sigma^2)*times+sigma*bm)
  return(st)
}
