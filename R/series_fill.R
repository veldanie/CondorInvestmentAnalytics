
#' fill_series

#' Checks if series is complete and if not, output will be the completed.
#' @param series_to_fill Series to be completed
#' @param ref_dates Ref dates to fill
#' @param constant Fill with last value
#' @return xts object
#' @export


series_fill <- function(series_to_fill, ref_dates, constant=TRUE, max_gap_days=10){
  series <- series_to_fill
  if(constant){
    pos_ini <- which(diff(index(series_to_fill))>max_gap_days)
    pos_last <- pos_ini + 1
    if (length(pos_ini)>0){
      for(i in pos_ini){
        new_dates <- ref_dates[ref_dates>index(series_to_fill)[i] & ref_dates<index(series_to_fill)[i+1]]
        series <- rbind(series, xts(rep(as.numeric(series_to_fill[i]), length(new_dates)), order.by = new_dates))
      }
    }
  }else{
    stop('Not supported')
  }
  return(series)
}
