
#' Rebalance port under limit
#'
#' Proportionally rebalances portfolio weights cutting off those under limit
#' @param port_w port weights
#' @param limit cutoff limit
#' @return rebalances portfolio weights
#' @export

rebalance_under_limit <- function(port_w, limit){
  reb_weight <- sum(port_w[port_w < limit])
  port_w[port_w < limit] <- 0
  port_w[port_w > limit] <- port_w[port_w > limit] + (port_w[port_w > limit] / sum(port_w[port_w > limit]) * reb_weight)
  return(port_w)
}
