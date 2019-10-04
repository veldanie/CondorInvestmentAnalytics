
#' Drawdown Returns returns
#'
#' Estimates de covariance using weighted averages of products of past returns.
#' @param series data frame series.
#' @param horizon Drawdown horizon.
#' @param quant Quantile.
#' @param atribution Risk atribution
#' @param type Type of returns: arithmetic (discrete) or log (continuous)
#' @param lb_months Lower bound months
#' @param end_of_month Indicatio. Use end-of-month dates to estimate drawdown.
#' @return Drawdown distribution, and mean, max and conditional drawdown.
#' @export

series_drawdown <- function(series, horizon = '12M', quant = 0.9, type = 'arit', lb_months = 12, end_of_month = FALSE) {
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


  pos_ini <- findInterval(months_seq,index(series), rightmost.closed = TRUE)
  pos_last <- findInterval(per_last,index(series), rightmost.closed = FALSE)
  n_per <- length(months_seq)
  per <- paste0(months_seq, "/", per_last)

  if(type == 'log'){
    dd_obs <- sapply(1:n_per, function(i) -min(log(as.numeric(series[pos_last[i]])/as.numeric(series[per[i]]))))
  }else{
    dd_obs <- sapply(1:n_per, function(i) -min(as.numeric(series[pos_last[i]])/as.numeric(series[per[i]])-1))
  }

  mean_dd <- mean(dd_obs)
  max_dd <- quantile(dd_obs, probs = quant)
  cond_dd <- mean(dd_obs[dd_obs >= max_dd])
  names(dd_obs) <- per
  max_dd_per <- per[dd_obs == max(dd_obs)]

  max_dd_obs <- max(dd_obs)
  ind_dd_obs <- dd_obs == max_dd_obs
  max_dd_date <- per_last[ind_dd_obs]
  series_max_dd <- series[per[ind_dd_obs]]
  max_dd_ini_date <- index(series_max_dd)[which.max(as.numeric(series_max_dd)/as.numeric(tail(series_max_dd, 1))-1)]
  t_dd_obs <- round(as.numeric(max_dd_date - max_dd_ini_date)/30, 2) ## Temporal dd observed

  return(list(dd_obs = dd_obs, max_dd = max_dd, mean_dd = mean_dd, cond_dd = cond_dd, max_dd_per = max_dd_per, max_dd_obs = max_dd_obs, max_dd_date = max_dd_date, max_dd_ini_date = max_dd_ini_date, t_dd_obs = t_dd_obs))
}
