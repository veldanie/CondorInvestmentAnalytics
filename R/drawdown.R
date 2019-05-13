
#' Drawdown Returns returns
#'
#' Estimates de covariance using weighted averages of products of past returns.
#' @param series data frame series.
#' @param w Portfolio weights.
#' @param horizon Drawdown horizon.
#' @param quant Quantile.
#' @param atribution Risk atribution
#' @param type Type of returns: arithmetic (discrete) or log (continuous)
#' @param ttr Indicator. Estimate time to recovery drawdown
#' @return Drawdown distribution, and mean, max and conditional drawdown.
#' @export


drawdown <- function(series, w, horizon = '12M', quant = 0.9, atribution = FALSE, type = 'log', ttr = FALSE, target_rec = 1) {
  if(is.null(series)){
    return(list(dd_obs = 0, dd_factor = 0, max_dd = 0, mean_dd = 0, cond_dd = 0, dd_marg = 0, dd_contrib = 0))
  }else{
    date_ini <- index(series)[1]
    date_last <- tail(index(series),1)

    num_months <- as.numeric(gsub('M', '', horizon))

    months_seq <- seq(date_ini, date_last %m+% -months(num_months), by = "months")
    n_per <- length(months_seq)
    dd_obs <- rep(0, n_per)
    dd_factor <- matrix(0, nrow = n_per, ncol = length(w))
    pers <- ttr_dates <- rep('', n_per)
    for (i in 1:n_per){ # For each period
      per_dates <- c(months_seq[i], months_seq[i] %m+% months(num_months))
      per <- paste(per_dates, collapse = '/')
      series_per <- series[per]# series_w[per] # Segmentation according to period
      w_mat <- rep(1, nrow(series_per)) %*% t(w)

      if(type == 'log')
        series_ret <- w_mat * log(series_per/(rep(1, nrow(series_per)) %*% head(series_per, 1)))
      else{
        series_ret <- w_mat * (series_per/(rep(1, nrow(series_per)) %*% head(series_per, 1)) - 1)
      }
      series_ret[is.na(series_ret)] <- 0
      port_ret <- as.numeric(apply(series_ret,1,sum))

      pos_max <- which.max(port_ret)
      dd_factor[i,] <- as.vector(series_ret[pos_max,]) - as.vector(tail(series_ret,1))

      #dd_obs[i] <- max(port_ret) - tail(port_ret,1)
      dd_obs[i] <- sum(dd_factor[i,])

      pers[i] <- paste(format(per_dates, '%b%Y'), collapse = '/')
      ttr_dates[i] <- per_dates[2]
    }
    ttr_dates <- as.Date(as.numeric(ttr_dates))
    mean_dd <- mean(dd_obs)
    max_dd <- quantile(dd_obs, probs = quant)
    cond_dd <- mean(dd_obs[dd_obs >= max_dd])

    dd_marg <- NULL
    dd_contrib <- NULL

    if(atribution){
      dd_marg <- apply(dd_factor [dd_obs>= max_dd,, drop = FALSE],2,mean)
      dd_contrib <- dd_marg/cond_dd
      names(dd_marg)<-names(dd_contrib)<-names(w)
    }
    names(dd_obs) <- pers
    ref_dd <- max(dd_obs)
    ind_max_dd <- dd_obs == ref_dd
    max_dd_per <- pers[ind_max_dd]

    rec_time <- NA
    if(ttr){
      ref_date <- ttr_dates[ind_max_dd]
      target_ret <- (1-ref_dd*(1-target_rec))/(1-ref_dd)-1
      series_post <- series[paste0(c(ref_date, Sys.Date()), collapse = '/')]# series_w[per] # Segmentation according to period
      w_mat <- rep(1, nrow(series_post)) %*% t(w)
      series_post_ret <- w_mat * (series_post/(rep(1, nrow(series_post)) %*% head(series_post, 1)) - 1)
      port_post_ret <- as.numeric(apply(series_post_ret,1,sum))
      rec_date <- index(series_post_ret)[port_post_ret > target_ret][1]
      if(!is.na(rec_date)){
        rec_time <- round(as.numeric(rec_date - ref_date)/30)
      }
    }
    return(list(dd_obs = dd_obs, dd_factor=dd_factor, max_dd = max_dd, mean_dd = mean_dd, cond_dd = cond_dd, dd_marg = dd_marg, dd_contrib = dd_contrib, max_dd_per = max_dd_per, rec_time = rec_time))
  }
}
