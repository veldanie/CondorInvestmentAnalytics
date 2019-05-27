
#' Negative returns percentage in horizon.
#'
#' Negative returns percentage in horizon. When counting negative returns it does not matter if returns are log or arthmetic.
#' @param series data frame series.
#' @param w Portfolio weights.
#' @param horizons Vector of horizons in number of months.
#' @param lb Lower bound
#' @param conf_interv Confidence interval
#' @return Negative returns percentage in horizon.
#' @export


neg_rets_perc <- function(series, w, horizons = '12M', lb = 0, conf_interv = 0.9) {
  date_ini <- index(series)[1]
  date_last <- tail(index(series),1)
  lhor <- length(horizons)
  perc <- rep(0, lhor)
  conf_ints <- matrix(0, nrow = lhor, ncol = 2, dimnames = list(c(), c("LB", "UB")))
  for(i in 1:lhor){
    h <- horizons[i]
    num_months <- as.numeric(gsub('M', '', h))
    if(date_last %m+% -months(num_months) < date_ini){break}
    months_seq <- seq(date_ini, date_last %m+% -months(num_months), by = "months")

    series_t0 <- series[,names(w)][findInterval(months_seq, index(series))]
    series_t1 <- series[,names(w)][findInterval(months_seq %m+% months(num_months), index(series))]
    n_obs <- nrow(series_t0)
    rets <- coredata(series_t1)/coredata(series_t0)-1
    rets[is.na(rets)] <- 0
    delta <- (1-conf_interv)/2
    conf_ints[i,] <- quantile(rets, c(delta, conf_interv+delta))
    perc[i] <- sum(as.numeric(rets %*% w) < lb)/n_obs
  }
  names(perc) <- horizons
  summ_df = data.frame(HOR=horizons, PROB=perc, conf_ints)
  return(list(perc = perc, df = summ_df))
}
