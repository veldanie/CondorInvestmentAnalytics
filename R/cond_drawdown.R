#' Conditional Drawdown
#'
#' Estimates de covariance using weighted averages of products of past returns.
#' @param series data frame series.
#' @param w Portfolio weights.
#' @param horizon Drawdown horizon.
#' @param quant Quantile.
#' @param type Type of returns: arithmetic (discrete) or log (continuous)
#' @return Conditional Drawdown.
#' @export

cond_drawdown <- function(series, w, horizon = '12M', quant = 0.9, type = 'log') {
    date_ini <- index(series)[1]
    date_last <- tail(index(series),1)

    num_months <- as.numeric(gsub('M', '', horizon))

    months_seq <- seq(date_ini, date_last %m+% -months(num_months), by = "months")
    n_per <- length(months_seq)
    dd_obs <- rep(0, n_per)
    pers <- sapply(months_seq, function(x) paste(c(x, x %m+% months(num_months)), collapse = '/'))

    dd_obs <- sapply(pers, max_drawdown, w, series, type)
    max_dd <- quantile(dd_obs, probs = quant)
    cond_dd <- mean(dd_obs[dd_obs >= max_dd])

    return(cond_dd)
}
