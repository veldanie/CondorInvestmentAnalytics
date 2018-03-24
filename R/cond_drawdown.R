#' Conditional Drawdown
#'
#' Estimates de covariance using weighted averages of products of past returns.
#' @param series data frame series.
#' @param w Portfolio weights.
#' @param pers Periods.
#' @param quant Quantile.
#' @param type Type of returns: arithmetic (discrete) or log (continuous)
#' @return Conditional Drawdown.
#' @export

months_seq <- seq(index(series)[1], tail(index(series),1) %m+% -months(as.numeric(gsub('M', '', horizon))), by = "months")
pers <- sapply(months_seq, function(x) paste(c(x, x %m+% months(as.numeric(gsub('M', '', horizon)))), collapse = '/'))

cond_drawdown <- function(series, w, pers, quant = 0.9, type = 'log') {
  if(is.null(series)){ cond_dd <- 0
  }else{
    w_mat <- rep(1, nrow(series)) %*% t(w)
    series_ret <- w_mat * log(series/(rep(1, nrow(series)) %*% head(series, 1)))
    port_ret <- xts(as.numeric(apply(series_ret,1,sum)), order.by = index(series_ret))

    dd_obs <- sapply(pers, function(x)max(port_ret[x])) - as.vector(port_ret[findInterval(ymd(substr(pers, 12,21)),index(port_ret))])

    max_dd <- quantile(dd_obs, probs = quant)
    cond_dd <- mean(dd_obs[dd_obs >= max_dd])
  }
    return(cond_dd)
}
