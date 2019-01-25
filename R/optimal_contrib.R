
#' optimal contribution to reach target.
#'
#' Takes the risk aversion coefficient, the covariance matrix and the market cap weights.
#' @param pr_simul Price Simul.
#' @param port Portfolio.
#' @param rc_1 Contrib rate year 1.
#' @param rc_2 Contrib rate year 2.
#' @param rc_3 Contrib rate year 3 onwards.
#' @param L_contrib Contribution years.
#' @param L Fund Horizon
#' @param B Param distribution over time.
#' @param Y Yield.
#' @param gr_1 Growth rate 2nd year onwards.
#' @param gr_2 Growth rate 2nd year onwards.
#' @param pe_target PE target.
#' @return optimal contribution to reach target.
#' @export

optimal_contrib <- function(pr_simul, port, rc_1 = 0.25, rc_2 = 0.333 , rc_3 = 0.5, L_contrib = NA, L = 12, B = 2.5, Y = 0,
                            gr_1, gr_2, pe_target = c(0.1,0.2,0.3)){
  dim_pr <- dim(pr_simul)
  N <- dim_pr[1]
  M <- dim_pr[2]
  V <- dim_pr[1] - L
  n_assets <- ncol(port) - 1
  n_port <- nrow(port)
  cc_port <- rep(0, nrow = n_port)

  for (k in 1:n_port){
    weights_mat <- as.numeric(port[k,-1])
    obj_cc <- function(cc){
      pe_simul <- private_equity_simul_const_contrib(cc, M, V, rc_1, rc_2, rc_3, L_contrib, L, B, Y, gr_1 = gr_1, gr_2 = gr_2)
      port_val <- matrix(0, nrow = L, ncol = M)
      pe_ti <- pe_t(pe_simul, 1)
      port_val[1,] <- 100 + pe_ti$cf + pe_ti$nav
      for (i in 2:L){
        dist_asset <- ((port_val[i - 1,] - pe_ti$nav) %*% t(rep(1, n_assets))) * (rep(1, M) %*% t(weights_mat))
        asset_units <- dist_asset/pr_simul[i - 1,,]
        pe_ti <- pe_t(pe_simul, i)

        port_val[i, ] <- rowSums(asset_units * pr_simul[i,,]) + pe_ti$cf + pe_ti$nav
      }
      port_val_L <- as.vector(tail(port_val, 1))
      pe_val <- pe_consol(pe_simul)$nav[L,]
      pe_part <- sum((pe_val / port_val_L - pe_target[k])**2)
      return(pe_part)
    }
    #ptm <- proc.time()
    sol <- nlm(obj_cc, 0, steptol=1e-3, gradtol=1e-3)
    #proc.time() - ptm
    cc_port[k] <- sol$estimate
  }
  return(cc_port)
}
