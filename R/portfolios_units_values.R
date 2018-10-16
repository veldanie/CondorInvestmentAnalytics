
#' Portfolio unit values
#'
#' Takes the risk aversion coefficient, the covariance matrix and the market cap weights.
#' @param pr_simul Price Simul.
#' @param port Portfolio.
#' @param port_ini_cc Initial Contributions
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
#' @param delta_contrib Contribution change.
#' @param pers_to_target Periods to reach targe.
#' @return Portfolio unit values.
#' @export

portfolios_units_values <- function(pr_simul, port, port_ini_cc, rc_1 = 0.25, rc_2 = 0.333 , rc_3 = 0.5, L_contrib = NA, L = 12, B = 2.5, Y = 0,
                                    gr_1, gr_2, pe_target = c(0.1,0.2,0.3), delta_contrib = 0.2, pers_to_target = 1){

  dim_pr <- dim(pr_simul)
  N <- dim_pr[1]
  M <- dim_pr[2]
  V <- dim_pr[1] - L
  n_assets <- ncol(port) - 1
  n_port <- nrow(port)
  port_units <- array(0, dim = c(N, M, n_port))
  pe_nav <- array(0, dim = c(N, M, n_port))
  pe_perc <- array(0, dim = c(N, M, n_port))
  port_cc <- array(0, dim = c(N, M, n_port))
  sample_vintage_nav <- matrix(0, nrow=L, ncol=n_port)
  sample_vintage_cf <- matrix(0, nrow=L, ncol=n_port)
  for(h in 1:L){port_cc[h,,] <- t(port_ini_cc)}

  port_units_list <- as.list(rep(NA,n_port))
  names(port_units_list) <- port[,1]

  for (k in 1:n_port){
    #print(k)
    weights_mat <- as.numeric(port[k,-1])
    pe_simul <- private_equity_simul(port_ini_cc[k,], M, V, rc_1, rc_2, rc_3, L_contrib, L, B, Y, gr_1, gr_2)

    sample_vintage_nav[,k] <- pe_simul$nav[,1,1]
    sample_vintage_cf[,k] <- pe_simul$cf[,1,1]

    port_val <- matrix(0, nrow = N, ncol = M)
    pe_ti <- pe_t(pe_simul, 1)
    port_val[1,] <- 100 + pe_ti$cf + pe_ti$nav
    for (i in 2:N){
      #print(i)
      dist_asset <- ((port_val[i - 1,] - pe_ti$nav) %*% t(rep(1, n_assets))) * (rep(1, M) %*% t(weights_mat))
      asset_units <- dist_asset/pr_simul[i - 1,,]
      pe_ti <- pe_t(pe_simul, i)
      port_val[i, ] <- rowSums(asset_units * pr_simul[i,,]) + pe_ti$cf + pe_ti$nav

      pe_nav_perc <- pe_ti$nav/port_val[i, ]

      if(i>L & i <= V){
        pe_above_limit <- pe_nav_perc > pe_target[k]
        nav_surplus <- (pe_ti$nav - port_val[i, ] * pe_target[k])/pers_to_target
        prev_cc <- port_cc[i-1,,k]
        prev_cc[prev_cc == 0] <- apply(port_cc[(1:(i-1)),prev_cc == 0,k, drop=FALSE],2,max)
        max_cont <- prev_cc * (1 + delta_contrib)

        cc_new <- mapply(function(x,y,z) min(x + y,z), x = port_cc[i-1,,k], y = -nav_surplus / (rc_1 * (1+gr_1[i,])),  z = max_cont)
        cc_new[cc_new < 0] <- 0

        pe_simul_vintage <- private_equity_simul(cc_new, M, 1, rc_1, rc_2, rc_3, L_contrib, L,  B, Y, gr_1[i:(i+L-1),, drop = FALSE], gr_2[i:(i+L-1),, drop = FALSE])
        port_cc[i,,k] <- cc_new

        pe_simul$nav[,i,] <- pe_simul_vintage$nav[,1,]
        pe_simul$cf[,i,] <- pe_simul_vintage$cf[,1,]

        pe_ti <- pe_t(pe_simul, i)
        port_val[i, ] <- rowSums(asset_units * pr_simul[i,,]) + pe_ti$cf + pe_ti$nav
        pe_nav_perc <- pe_ti$nav/port_val[i, ]
      }
      pe_nav[i,,k] <- pe_ti$nav
      pe_perc[i,,k] <- pe_nav_perc
    }
    port_units_list[[k]] <- port_val
    port_units [,,k]<-port_val
  }
  return(list(port_units_list = port_units_list, port_units = port_units, pe_nav = pe_nav, pe_perc = pe_perc, sample_vintage_nav=sample_vintage_nav, sample_vintage_cf=sample_vintage_cf))
}
