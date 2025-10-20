
#' Index Series
#'
#' Estimates total return index series.
#' @param series Asset series.
#' @param backup_series Backup series.
#' @param threshold Value allow us to identify a div payment.
#' @ref_years Reference years
#' @return index series
#' @export

index_series_with_jumps <- function(series, backup_series, threshold=0, ref_years='2005/2020', complete_method_rets=TRUE){
  nrow_series <- nrow(series)
  diff_pr <- diff(as.vector(series))
  pos_jump <- which(diff_pr>threshold)
  val_jump <- as.vector(series[pos_jump+1]) - as.vector(series[pos_jump])
  avg_jump_dist <- mean(diff(pos_jump))
  
  if(tail(pos_jump, 1) != nrow_series){
    pos_jump <- append(pos_jump, nrow_series)
    val_jump <- append(val_jump, as.vector(mean(val_jump)))
    series_out <- rbind(series, xts(x = tail(coredata(series), 1), order.by = tail(index(series), 1) + 1))
    adjust_factor <- max(1, ((tail(pos_jump, 1) - tail(pos_jump, 2)[1]) / avg_jump_dist))
  } else {
    series_out <- series
  } 
  
  diff_pr[pos_jump] <- 0
  cumsum_diff_pr <- cumsum(diff_pr)

  for (i in 1:length(pos_jump)){
    if(tail(pos_jump, 1) != nrow_series){
      if(i==1){
        series_out[2:(pos_jump[i]+1)] <- as.vector(series[1]) + cumsum(diff_pr[1:pos_jump[i]]) +
        (1:pos_jump[i])*val_jump[i]/pos_jump[i]
      }else if (i == length(pos_jump)) {
        series_out[(pos_jump[i-1]+2):(pos_jump[i]+1)] <- as.vector(series_out[pos_jump[i-1]+1]) + 
        cumsum(diff_pr[(pos_jump[i-1]+1):pos_jump[i]]) + 
        (1:(pos_jump[i]-pos_jump[i-1]))*(val_jump[i]/adjust_factor)/(pos_jump[i]-pos_jump[i-1])
      }else {
        series_out[(pos_jump[i-1]+2):(pos_jump[i]+1)] <- as.vector(series_out[pos_jump[i-1]+1]) + 
        cumsum(diff_pr[(pos_jump[i-1]+1):pos_jump[i]]) + 
        (1:(pos_jump[i]-pos_jump[i-1]))*val_jump[i]/(pos_jump[i]-pos_jump[i-1])
      }
    }else {
      if(i==1){
        series_out[2:(pos_jump[i]+1)] <- as.vector(series[1]) + cumsum(diff_pr[1:pos_jump[i]]) +
        (1:pos_jump[i])*val_jump[i]/pos_jump[i]
      }else {
        series_out[(pos_jump[i-1]+2):(pos_jump[i]+1)] <- as.vector(series_out[pos_jump[i-1]+1]) + 
        cumsum(diff_pr[(pos_jump[i-1]+1):pos_jump[i]]) + 
        (1:(pos_jump[i]-pos_jump[i-1]))*val_jump[i]/(pos_jump[i]-pos_jump[i-1])
      }
    }  
  }
  if (!is.null(backup_series)) {
    if(complete_method_rets){
      series_out <- rets_complete_index_series(series_out, backup_series, "2000/2020")[ref_years]
    }else{
      series_out <- complete_index_series(series_out, backup_series)[ref_years]
    }
  }
  return(series_out)
}
