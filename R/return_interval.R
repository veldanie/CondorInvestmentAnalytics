
#' Anualized returns intervals.
#'
#' Anualized returns intervals.
#' @param rets returns data frame.
#' @param freq return frequency.
#' @param conf_interv Confidence interval.
#' @param header DF Colnames
#' @param method n:normal, t: t-dist, c:chebyshev.
#' @return Anualized returns intervals DF.
#' @export


return_interval <- function(rets, freq = 12, conf_interv = 0.95, header=NULL, method="n") {

  n_obs <- nrow(rets)
  delta <- (1-conf_interv)/2
  mean_rets <- colMeans(rets) * freq
  sd_rets <- colSds(rets) * sqrt(freq)

  if (substr(method,1,1)=="n"){
    err <- qnorm(conf_interv+delta)*sd_rets/sqrt(n_obs)
  }else if(substr(method,1,1)=="t"){
    err <- qt(conf_interv+delta, n_obs-1)*sd_rets/sqrt(n_obs)
  }else{
    err <- (sd_rets/sqrt(n_obs * (1-conf_interv)))
  }
  lb <- mean_rets - err
  ub <- mean_rets + err

  ret_df <- data.frame(Portfolio=colnames(rets), Mean=round(mean_rets,5), LB=round(lb,5), UB=round(ub,5))
  if(!is.null(header)){
    colnames(ret_df) <- header
  }
  return(ret_df)
}
