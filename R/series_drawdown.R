
#' Drawdown Returns returns
#'
#' Estimates de covariance using weighted averages of products of past returns.
#' @param series data frame series.
#' @param horizon Drawdown horizon.
#' @param quant Quantile.
#' @param atribution Risk atribution
#' @param type Type of returns: arithmetic (discrete) or log (continuous)
#' @return Drawdown distribution, and mean, max and conditional drawdown.
#' @export

series_drawdown <- function(series, horizon = '12M', quant = 0.9, type = 'arit') {
  date_ini <- index(series)[1]
  date_last <- tail(index(series),1)

  num_months <- as.numeric(gsub('M', '', horizon))

  months_seq <- seq(date_ini, date_last %m+% -months(num_months), by = "months")
  n_per <- length(months_seq)
  dd_obs <- rep(0, n_per)
  pers <- rep('', n_per)
  for (i in 1:n_per){ # For each period
    per <- paste(c(months_seq[i], months_seq[i] %m+% months(num_months)), collapse = '/')
    pers[i] <- paste(format(ymd(base::strsplit(per, '/')[[1]]), '%b%Y'), collapse = '/')
    series_per <- series[per]# series_w[per] # Segmentation according to period

    if(type == 'log')
      series_ret <- log(series_per/(rep(1, nrow(series_per)) %*% head(series[per], 1)))
    else{
      series_ret <- (series_per/(rep(1, nrow(series_per)) %*% head(series[per], 1)) - 1)
    }
    series_ret[is.na(series_ret)] <- 0
    dd_obs[i] <- max(series_ret) - tail(series_ret,1)
  }

  mean_dd <- mean(dd_obs)
  max_dd <- quantile(dd_obs, probs = quant)
  cond_dd <- mean(dd_obs[dd_obs >= max_dd])

  names(dd_obs) <- pers
  max_dd_per <- pers[dd_obs == max(dd_obs)]
  return(list(dd_obs = dd_obs, max_dd = max_dd, mean_dd = mean_dd, cond_dd = cond_dd, max_dd_per = max_dd_per))
}
