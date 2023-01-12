
#' Rebuild series
#'
#' Rebuilds series adjusting the drift and volatility.
#' @param series_orig Original series
#' @param spr_anual Anualized return spread
#' @param vol_add Approximate Volatiliyy increment.
#' @return rebuilt series

rebuild_series_spr <- function(series_orig, spr_anual=0, vol_add=NULL){
  
  spr_daily <- spr_anual/days_year
  series_rets <- returns(series_orig)
  mean_orig <- mean(series_rets)
  vol_orig <- sd(series_rets)
  series_norm_rets <- (series_rets - mean_orig)/vol_orig
  
  if (is.null(vol_add)){
    vol_target <- vol_orig*(1+vol_add)
  }else{
    vol_target <- vol_orig + spr_daily*vol_orig/mean_orig
  }
  vol_target <- vol_orig*(1+vol_add)
  mean_target <- mean_orig + spr_daily
  series_rets_udt <-  series_norm_rets*vol_target + mean_target
  series_udt <- 100*cumprod(1+series_rets_udt) 
  
  sd(returns(series_orig))*sqrt(252)
  sd(returns(series_udt))*sqrt(252)
  return(series_udt)   
}
