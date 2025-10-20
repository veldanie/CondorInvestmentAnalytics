
#' PE simulation.
#'
#' Takes the risk aversion coefficient, the covariance matrix and the market cap weights.
#' @param cc Contribution.
#' @param M Number of simulations.
#' @param V Number of vintages.
#' @param rc_1 Contrib rate year 1.
#' @param rc_2 Contrib rate year 2.
#' @param rc_3 Contrib rate year 3 onwards.
#' @param L_contrib Contribution years.
#' @param L Fund Horizon
#' @param B Param distribution over time.
#' @param Y Yield.
#' @param gr_1 Growth rate 2nd year onwards.
#' @param gr_2 Growth rate 2nd year onwards.
#' @return PE simulation.
#' @export

private_equity_simul <- function(cc, M, V, rc_1 = 0.25, rc_2 = 0.333 , rc_3 = 0.5, L_contrib = NA, L = 12, B = 2.5, Y = 0, gr_1, gr_2){
  # Simula cada vintage.
  ##cc: vector contrib per tray
  cf <- dt <- nav <- array(0, dim = c(L, V, M))
  ct <- matrix(0, nrow = L, ncol = M)

  if(is.na(L_contrib)){
    rc <- c(rc_1, rc_2, rep(rc_3,L-2))
  }else{
    rc <- c(rc_1, rc_2, rep(rc_3,L_contrib-2), rep(1,L-L_contrib))
  }
  t_l <- (1:L)/L
  rd <- sapply(sapply(t_l ^ B, max, Y), min, 1)

  ct[1,] <- rc_1 * cc
  for (i in 2:L){
    ct[i,] <- rc[i] * (cc - colSums(ct[1:(i-1),,drop = FALSE]))
  }

  dt[1,,] <- 0
  nav[1,,] <- (rep(1, V) %*% t(ct[1,])) * (1 + gr_1[1:V,])
  cf[1,,] <- -(rep(1, V) %*% t(ct[1,])) + dt[1,,]

  for (i in 2:L){
    if(i <= 2){gr_t <- gr_1[i:(i+V-1),]}else{gr_t <- gr_2[i:(i+V-1),]}
    dt[i,,] <- nav[i-1,,] * rd[i]
    nav[i,,] <- nav[i-1,,] * (1 + gr_t) + (rep(1, V) %*% t(ct[i, ])) - dt[i,,]
    cf[i,,] <- -(rep(1, V) %*% t(ct[i, ])) + dt[i,,]
  }
  return(list(nav = nav, cf = cf, dt=dt))
}
