
#' Estimates derivative of portfolio volatility w.r.t. w.
#'
#' Estimates derivative of portfolio volatility w.r.t. w.
#' @param series xts return series.
#' @param w Portfolio weights.
#' @param delta_val weight variation.
#' @param freq times per year.
#' @return Discrete derivative for each asset.
#' @export

approx_dret_dw <- function(series, w, delta_val = 0.01, freq = 12){

  if(delta_val > min(w) | delta_val > (1 - max(w))){
    stop('Please modify delta value.')
  }
  lw <- length(w)
  ds <- ds_up <- ds_down <- rep(0, lw)
  w_up <- w_down <- w

  port_mean <- mean(as.numeric(series %*% w)) * freq
  for (i in 1:length(w)){
    w_up[i] <- w[i] + delta_val
    w_up[-i] <- w[-i] - delta_val * w[-i]/sum(w[-i])
    w_down[i] <-  w[i] - delta_val
    w_down[-i] <- w[-i] + delta_val * w[-i]/sum(w[-i])
    port_ret_up <- mean(as.numeric(series %*% w_up)) * freq
    port_ret_down <- mean(as.numeric(series %*% w_down)) * freq
    ds_up[i] <- port_ret_up - port_mean
    ds_down[i] <-  port_ret_down - port_mean

    ds[i] <- mean(abs(c(ds_up[i], ds_down[i])))
  }

  return(list(ds = ds, ds_up = ds_up, ds_down = ds_down))
}
