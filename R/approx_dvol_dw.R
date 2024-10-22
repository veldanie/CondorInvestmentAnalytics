
#' Estimates derivative of portfolio volatility w.r.t. w.
#'
#' Estimates derivative of portfolio volatility w.r.t. w.
#' @param series xts return series.
#' @param w Portfolio weights.
#' @param delta_val weight variation.
#' @param freq times per year.
#' @return Discrete derivative for each asset.
#' @export

approx_dvol_dw <- function(series, w, delta_val = 0.01, freq = 12){

  if(delta_val > min(w) | delta_val > (1 - max(w))){
    stop('Please modify delta value.')
  }
  lw <- length(w)
  dvol <- dvol_up <- dvol_down <- rep(0, lw)
  w_up <- w_down <- w
  port_vol <- sd(as.numeric(series %*% w))
  for (i in 1:length(w)){
    w_up[i] <- w[i] + delta_val
    w_up[-i] <- w[-i] - delta_val * w[-i]/sum(w[-i])
    w_down[i] <-  w[i] - delta_val
    w_down[-i] <- w[-i] + delta_val * w[-i]/sum(w[-i])
    port_vol_up <- sd(as.numeric(series %*% w_up))
    port_vol_down <- sd(as.numeric(series %*% w_down))
    dvol_up[i] <- port_vol_up - port_vol
    dvol_down[i] <-  port_vol_down - port_vol

    dvol[i] <- mean(abs(c(dvol_up[i], dvol_down[i])))
  }

  dvol_annual <- dvol * freq
  dvol_up_annual <- dvol_up * freq
  dvol_down_annual <- dvol_down * freq
  return(list(dvol = dvol, dvol_up = dvol_up, dvol_down = dvol_down, dvol_annual = dvol_annual, dvol_up_annual = dvol_up_annual, dvol_down_annual = dvol_down_annual))
}
