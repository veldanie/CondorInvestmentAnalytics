
#' PE Consol.
#'
#' Takes the risk aversion coefficient, the covariance matrix and the market cap weights.
#' @param pe_simul PE simulation.
#' @return PE Consol
#' @export
#'
pe_consol <- function(pe_simul){
  pe_dim <- dim(pe_simul$nav)
  cf <- nav <- matrix(0, nrow = pe_dim[1] + pe_dim[2] - 1, ncol = pe_dim[3])
  V <- 1
  for (i in 1:nrow(cf)){
    sum_nav <- 0
    sum_cf <- 0
    k <- i
    for (v in 1:i){
      if(v > pe_dim[2]){next}
      if(k > pe_dim[1]){
        k <- k - 1; next
      }else{
        sum_nav <- sum_nav + pe_simul$nav[k,v,]
        sum_cf <- sum_cf + pe_simul$cf[k,v,]
      }
      k <- k - 1
    }
    nav [i,] <- sum_nav
    cf [i,] <- sum_cf
  }
  return(list(cf = cf, nav = nav))
}
