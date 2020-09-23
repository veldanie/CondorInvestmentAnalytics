
#' Anualized returns intervals.
#'
#' Anualized returns intervals.
#' @param series data frame series.
#' @param horizon_in_months Return horizon in months.
#' @param conf_interv Return conf. interval.
#' @param header DF Header
#' @return Anualized returns intervals DF.
#' @export

return_hor_interval <- function(series, horizon_in_months = 36, conf_interv = 0.95,  header=NULL) {

  date_ini <- index(series)[1]
  date_last <- tail(index(series),1)
  if(date_last %m+% -months(horizon_in_months) < date_ini){break}
  months_seq <- seq(date_ini, date_last %m+% -months(horizon_in_months), by = "months")

  series_t0 <- series[findInterval(months_seq, index(series))]
  series_t1 <- series[findInterval(months_seq %m+% months(horizon_in_months), index(series))]
  n_obs <- nrow(series_t0)
  rets <- coredata(series_t1)/coredata(series_t0)
  rets[is.na(rets)] <- 0
  delta <- (1-conf_interv)/2

  rets_means <- colMeans(rets)**(1/(horizon_in_months/12)) - 1
  ret_df <- data.frame(Portfolio=colnames(series), round(cbind(rets_means, t((apply(rets, 2, quantile, c(delta, conf_interv+delta)))**(1/(horizon_in_months/12)) - 1)),4))
  if(is.null(header)){
    colnames(ret_df) <- c('Portfolio', 'Mean', 'LB', 'UB')
  }else{
    colnames(ret_df) <- header
  }
  return(ret_df)
}
