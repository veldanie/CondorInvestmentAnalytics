
#' PE at time ti.
#'
#' PE at time ti.
#' @param pe_simul PE Simulation.
#' @param ti Time.
#' @return PE at time ti.
#' @export

pe_t <- function(pe_simul, ti){
  pe_dim <- dim(pe_simul$nav)
  sum_nav <- 0
  sum_cf <- 0
  k <- ti
  for (v in 1:ti){
    if(v > pe_dim[2]){next}
    if(k > pe_dim[1]){
      k <- k - 1; next
    }else{
      sum_nav <- sum_nav + pe_simul$nav[k,v,]
      sum_cf <- sum_cf + pe_simul$cf[k,v,]
    }
    k <- k - 1
  }
  return(list(cf = sum_cf, nav = sum_nav))
}
