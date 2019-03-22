
#' Negative returns percentage in horizon.
#'
#' Negative returns percentage in horizon. When counting negative returns it does not matter if returns are log or arthmetic.
#' @param series data frame series.
#' @param w Portfolio weights.
#' @param horizons Vector of horizons in number of months.
#' @param lb Lower bound
#' @return Negative returns percentage in horizon.
#' @export


neg_rets_perc <- function(series, w, horizons = '12M', lb = 0) {
  date_ini <- index(series)[1]
  date_last <- tail(index(series),1)
  perc_neg_rets <- rep(0, length(horizons))
  for(i in 1:length(horizons)){
    h <- horizons[i]
    num_months <- as.numeric(gsub('M', '', h))
    if(date_last %m+% -months(num_months) < date_ini){break}
    months_seq <- seq(date_ini, date_last %m+% -months(num_months), by = "months")

    series_t0 <- series[,names(w)][findInterval(months_seq, index(series))]
    series_t1 <- series[,names(w)][findInterval(months_seq %m+% months(num_months), index(series))]
    n_obs <- nrow(series_t0)
    log_rets <- log(coredata(series_t1)/coredata(series_t0))
    log_rets[is.na(log_rets)] <- 0
    perc_neg_rets[i] <- sum(as.numeric(log_rets %*% w) < lb)/n_obs
  }
  names(perc_neg_rets) <- horizons
  return(perc_neg_rets)
}
