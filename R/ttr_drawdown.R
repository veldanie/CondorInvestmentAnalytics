
#' Time to recovery Drawdown
#'
#' Estimates aproximate number of monthos to recover from drawdown.
#' @param series data frame series.
#' @param w Portfolio weights.
#' @param dd_ret Drawdown return
#' @param dd_date Drawdown date (lower bound).
#' @param target_rec Target recovery perc.
#' @return Time to Recovery in months target return(s).
#' @export


ttr_drawdown <- function(series, w, dd_ret, dd_date, target_rec = 1) {
  if(is.null(series)){
    return(list(rec_time = NA, target_ret = NA))
  }else{
    rec_time <- rec_times <- NA
    l_dd <- length(dd_ret)
    if(l_dd==1){
      target_ret <- (1-dd_ret*(1-target_rec))/(1-dd_ret)-1
      series_post <- series[paste0(c(dd_date, Sys.Date()), collapse = '/')]
      w_mat <- rep(1, nrow(series_post)) %*% t(w)
      series_post_ret <- w_mat * (series_post/(rep(1, nrow(series_post)) %*% head(series_post, 1)) - 1)
      port_post_ret <- as.numeric(apply(series_post_ret,1,sum))
      rec_date <- index(series_post_ret)[port_post_ret > target_ret][1]
      if(!is.na(rec_date)){
        rec_time <- as.numeric(rec_date - dd_date)/30
      }
    }else if(l_dd>1){
      target_ret <- (1-dd_ret*(1-target_rec))/(1-dd_ret)-1
      rec_times <- rep(0, l_dd)
      for(i in 1:l_dd){
        series_post <- series[paste0(c(dd_date[i], Sys.Date()), collapse = '/')]
        w_mat <- rep(1, nrow(series_post)) %*% t(w)
        series_post_ret <- w_mat * (series_post/(rep(1, nrow(series_post)) %*% head(series_post, 1)) - 1)
        port_post_ret <- as.numeric(apply(series_post_ret,1,sum))
        rec_date <- index(series_post_ret)[port_post_ret >= target_ret[i]][1]
        rec_times[i] <- as.numeric(rec_date - dd_date[i])/30
      }
      rec_time <- mean(rec_times, na.rm=TRUE)
    }
    return(list(rec_time = rec_time, target_ret = target_ret, rec_times = rec_times))
  }
}
