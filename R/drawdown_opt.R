
#' Drawdown Returns returns
#'
#' Estimates de covariance using weighted averages of products of past returns.
#' @param series data frame series.
#' @param w Portfolio weights.
#' @param horizon Drawdown horizon.
#' @param quant Quantile.
#' @param type Type of returns: arithmetic (discrete) or log (continuous)
#' @return Drawdown distribution, and mean, max and conditional drawdown.
#' @export


drawdown_opt <- function(series, w, horizon = '3M', quant = 0.9, type = 'log') {
  if(is.null(series)){
    return(list(dd_obs = 0, dd_factor = 0, max_dd = 0, mean_dd = 0, cond_dd = 0, dd_marg = 0, dd_contrib = 0))
  }else{
    date_ini <- index(series)[1]
    date_last <- tail(index(series),1)
    num_months <- as.numeric(gsub('M', '', horizon))
    months_seq <- seq(date_ini, date_last %m+% -months(num_months), by = "months")
    per_last <- as_date(sapply(months_seq, function(x) x %m+% months(num_months)))
    per <- paste0(months_seq, "/", per_last)
    n_per <- length(per)
    pos_ini <- findInterval(months_seq,index(series), rightmost.closed = TRUE)
    pos_last <- findInterval(per_last,index(series), rightmost.closed = FALSE)

    if(type == 'log'){
      dd_obs <- sapply(1:n_per, function(i) max(log(series[per[i]]/(rep(1, nrow(series[per[i]]))%*% series[pos_ini[i]]))%*%w)- log(as.vector(series[pos_last[i]])/as.vector(series[pos_ini[i]])))
    }else{
      dd_obs <- sapply(1:n_per, function(i) max((series[per[i]]/(rep(1, nrow(series[per[i]]))%*% series[pos_ini[i]])-1)%*%w)-(as.vector(series[pos_last[i]])/as.vector(series[pos_ini[i]])-1)%*%w)
    }

    mean_dd <- mean(dd_obs)
    max_dd <- quantile(dd_obs, probs = quant)
    cond_dd <- mean(dd_obs[dd_obs >= max_dd])
    names(dd_obs) <- per
    return(list(dd_obs = dd_obs, max_dd = max_dd, mean_dd = mean_dd, cond_dd = cond_dd))
  }
}