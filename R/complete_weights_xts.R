
#' complete_weights_xts
#'
#' Completes weights_xts objecto for rebalancing.
#' @param weights_xts Original series,
#' @param rebal_dates Vector of rebal dates.
#' @param min_diff_days Min. diff in days between rebal dates and original dates.
#' @return list. New weights_xts
#' @export


complete_weights_xts <- function(weights_xts, rebal_dates, min_diff_days = 25){
  orig_dates <- index(weights_xts)
  valid_rebal_dates <- rebal_dates[sapply(rebal_dates, function(x) all(as.vector(abs(orig_dates - x)) > min_diff_days))]
  all_dates <- sort(c(orig_dates, valid_rebal_dates))
  weights_xts_new <- xts(matrix(0, length(all_dates), ncol(weights_xts)), order.by =  all_dates)
  colnames(weights_xts_new) <- colnames(weights_xts)
  pos_ports <- match(orig_dates, all_dates)
  weights_xts_new[pos_ports,] <- coredata(weights_xts)
  sorted_pos_ports <- sort(pos_ports)
  # Head
  if (sorted_pos_ports[1]!=1){
    pos_add <- 1:(sorted_pos_ports[1]-1)
    weights_xts_new[pos_add,] <- rep(1, length(pos_add))%*% weights_xts_new[sorted_pos_ports[1],]
  }

  for (i in 2:length(sorted_pos_ports)){
    if((sorted_pos_ports[i] - sorted_pos_ports[i-1])>=2){
      pos_add <- (sorted_pos_ports[i-1]+1):(sorted_pos_ports[i]-1)
      weights_xts_new[pos_add,] <- rep(1, length(pos_add))%*% weights_xts_new[sorted_pos_ports[i-1],]
    }
  }

  #Tail
  if (tail(sorted_pos_ports,1)!=length(all_dates)){
    pos_add <- (tail(sorted_pos_ports,1)+1):length(all_dates)
    weights_xts_new[pos_add,] <- rep(1, length(pos_add))%*% weights_xts_new[tail(sorted_pos_ports,1),]
  }

  return(weights_xts_new)
}
