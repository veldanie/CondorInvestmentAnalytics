
#' Drawdown Returns returns
#'
#' Estimates de covariance using weighted averages of products of past returns.
#' @param series data frame series.
#' @param w Portfolio weights.
#' @param horizon Drawdown horizon.
#' @param quant Quantile.
#' @return Drawdown distribution, and mean, max and conditional drawdown.
#' @export


drawdown <- function(series, w, horizon, quant) {

  series_w <- (rep(1, dim(series)[1]) %*% t(w)) * series
  date_ini <- index(series_w)[1]
  date_last <- tail(index(series_w),1)

  num_months <- as.numeric(gsub('M', '', horizon))

  months_seq <- seq(date_ini, date_last %m+% -months(num_months), by = "months")
  n_per <- length(months_seq)
  dd_obs <- rep(0, n_per)

  for (i in 1:n_per){ # For each period
    per <- paste(c(months_seq[i], months_seq[i] %m+% months(num_months)), collapse = '/')
    series_per <- series_w[per] # Segmentation according to period
    port_ret <- as.numeric(apply(series_per/(rep(1, nrow(series_per)) %*% head(series, 1)),1,sum))-1
    dd_obs[i] <- max(port_ret) - tail(port_ret, 1)
  }

  mean_dd <- mean(dd_obs)
  max_dd <- quantile(dd_obs, probs = quant)
  cond_dd <- mean(dd_obs[dd_obs >= max_dd])
  return(list(dd_obs = dd_obs, max_dd = max_dd, mean_dd = mean_dd, cond_dd = cond_dd))
}
