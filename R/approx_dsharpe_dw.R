
#' Estimates derivative of portfolio volatility w.r.t. w.
#'
#' Estimates derivative of portfolio volatility w.r.t. w.
#' @param series xts return series.
#' @param w Portfolio weights.
#' @param delta_val weight variation.
#' @param freq times per year.
#' @return Discrete derivative for each asset.
#' @export

approx_dsharpe_dw <- function(series, w, delta_val = 0.01, freq = 12){

  if(delta_val > min(w) | delta_val > (1 - max(w))){
    stop('Please modify delta value.')
  }
  lw <- length(w)
  ds <- ds_up <- ds_down <- rep(0, lw)
  w_up <- w_down <- w

  port_vol <- sd(as.numeric(series %*% w))
  port_mean <- mean(as.numeric(series %*% w))
  port_sharpe <- port_mean * freq/(port_vol * sqrt(freq))
  for (i in 1:length(w)){
    w_up[i] <- w[i] + delta_val
    w_up[-i] <- w[-i] - delta_val * w[-i]/sum(w[-i])
    w_down[i] <-  w[i] - delta_val
    w_down[-i] <- w[-i] + delta_val * w[-i]/sum(w[-i])
    port_sharpe_up <- mean(as.numeric(series %*% w_up)) * freq / (sd(as.numeric(series %*% w_up)) * sqrt(freq))
    port_sharpe_down <- mean(as.numeric(series %*% w_down)) * freq / (sd(as.numeric(series %*% w_down)) * sqrt(freq))
    ds_up[i] <- port_sharpe_up - port_sharpe
    ds_down[i] <-  port_sharpe_down - port_sharpe

    ds[i] <- mean(abs(c(ds_up[i], ds_down[i])))
  }

  return(list(ds = ds, ds_up = ds_up, ds_down = ds_down))
}
