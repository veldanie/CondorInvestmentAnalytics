
#' Drawdown Returns returns
#'
#' Estimates de covariance using weighted averages of products of past returns.
#' @param series data frame series.
#' @param w Portfolio weights.
#' @param horizon Drawdown horizon.
#' @param quant Quantile.
#' @param type Type of returns: arithmetic (discrete) or log (continuous)
#' @param t_dd Indicator calculate T-drawdown.
#' @param lb_months Lower bound months
#' @param end_of_month Indicatio. Use end-of-month dates to estimate drawdown.
#' @return Drawdown distribution, and mean, max and conditional drawdown.
#' @export


drawdown_opt <- function(series, w, horizon = '3M', quant = 0.9, type = 'log', t_dd = FALSE, lb_months = 12, end_of_month = FALSE) {
  if(is.null(series)){
    return(list(dd_obs = 0, dd_factor = 0, max_dd = 0, mean_dd = 0, cond_dd = 0, dd_marg = 0, dd_contrib = 0))
  }else{
    date_ini <- index(series)[1]
    date_last <- tail(index(series),1)
    num_months <- as.numeric(gsub('M', '', horizon))
    if(end_of_month){
      months_seq_all <- unique(c(date_ini, seq(ceiling_date(date_ini, "month")-1, date_last, by = "months"), date_last))
    }else{
      months_seq_all <- seq(date_ini, date_last, by = "months")
    }
    n_months_seq <- length(months_seq_all)
    months_seq <- months_seq_all[1:(n_months_seq - num_months)]
    per_last <- as_date(sapply(1:length(months_seq), function(x) months_seq_all[x + num_months]))
    if(num_months >= lb_months){ #Se incorporan periodos de menor plazo al inicio para no descartar datos relevantes.
     months_seq <- c(rep(date_ini, num_months-1), months_seq)
     per_last <- c(months_seq_all[2:num_months], per_last)
    }
    per <- paste0(months_seq, "/", per_last)
    n_per <- length(per)
    pos_ini <- findInterval(months_seq,index(series), rightmost.closed = TRUE)
    pos_last <- findInterval(per_last,index(series), rightmost.closed = FALSE)

    if(type == 'log'){
      #dd_obs <- sapply(1:n_per, function(i) max(log(series[per[i]]/(rep(1, nrow(series[per[i]]))%*% series[pos_ini[i]]))%*%w)- log(as.vector(series[pos_last[i]])/as.vector(series[pos_ini[i]])))
      dd_obs <- sapply(1:n_per, function(i) -min(log((rep(1, nrow(series[per[i]]))%*% series[pos_last[i]])/series[per[i]])%*%w))
    }else{
      #dd_obs <- sapply(1:n_per, function(i) max((series[per[i]]/(rep(1, nrow(series[per[i]]))%*% series[pos_ini[i]])-1)%*%w)-(as.vector(series[pos_last[i]])/as.vector(series[pos_ini[i]])-1)%*%w)
      dd_obs <- sapply(1:n_per, function(i) -min(((rep(1, nrow(series[per[i]]))%*% series[pos_last[i]])/series[per[i]]-1)%*%w))
    }

    mean_dd <- mean(dd_obs)
    max_dd <- quantile(dd_obs, probs = quant)
    cond_dd_obs <- dd_obs[dd_obs >= max_dd]
    cond_dd <- mean(cond_dd_obs)
    names(dd_obs) <- per
    cond_dd_dates <- per_last[dd_obs >= max_dd]
    max_dd_obs <- max(dd_obs)
    ind_dd_obs <- dd_obs == max_dd_obs
    max_dd_date <- per_last[dd_obs == max_dd_obs]

    series_max_dd <- series[per[ind_dd_obs]]
    max_dd_ini_date <- index(series_max_dd)[which.max((series_max_dd/(rep(1, nrow(series_max_dd))%*% tail(series_max_dd, 1))-1)%*%w)]

    t_dd_obs <- as.numeric(max_dd_date - max_dd_ini_date)/30 ## Temporal dd observed

    return(list(dd_obs = dd_obs, max_dd = max_dd, mean_dd = mean_dd, cond_dd = cond_dd, max_dd_obs=max_dd_obs, cond_dd_obs=cond_dd_obs, max_dd_ini_date = max_dd_ini_date, max_dd_date=max_dd_date, cond_dd_dates=cond_dd_dates, t_dd_obs=t_dd_obs))
  }
}
