
#' Estimates lower and upper limits of portfolio.
#'
#' Estimates lower and upper limits of portfolio.
#' @param port_series xts portfolio return series.
#' @param series xts return series.
#' @param w Portfolio weights.
#' @param port_vol_ann_lim Anualized volatility portfolio limits.
#' @param freq Returns period as times per year.
#' @param N Sample size to estimate vol. of each weight.
#' @param vol_factor Constant that multiplies portfolio volatility to perturbate portfolio returns for bootstrapping procedure.
#' @param min_max_q Min and max probability associated to min and max weights standard deviation.
#' @param bias_bound_lim If prob. of certain weight is larger than this param., then no bias-limit is imposed.
#' @param dw weight variation.
#' @param sharpe User Sharpe or vol.
#' @return lower and upper limits of portfolio.
#' @export

port_limits <- function(port_series, series, w, port_vol_lim, perc_ret_var = 0.2, freq = 12, N = 1e3, vol_factor = 0.1, min_max_q = c(0.975, 0.6), bias_bound_lim = 0.9, dw = 0.01){

  # Bias Bound.
  port_vol <- sd(port_series)

  port_df <- data.frame(port_series, series); colnames(port_df) <- c('Port', colnames(series))
  n_row <- nrow(port_df)
  port_coef <- matrix(0, ncol = ncol(port_df), nrow = N)

  for (i in 1:N){
    df_temp <- port_df
    df_temp$Port <- port_df$Port + rnorm(nrow(port_df)) * port_vol * vol_factor
    port_coef[i,] <- lm(Port~., data = df_temp)$coefficients
  }

  w_sd <- apply(port_coef, 2, sd)[-1]
  lim_q <- approx(x = c(min(w_sd), max(w_sd)), y = min_max_q, xout = w_sd)$y

  w_bias_upper <- sapply(w + w_sd * qnorm(lim_q), min, 1)
  w_bias_lower <- sapply(w - w_sd * qnorm(lim_q), max, 0)

  w_bias_upper[lim_q > bias_bound_lim] <- 1
  w_bias_lower[lim_q > bias_bound_lim] <- 0

  # Risk Bound
  ## Sharpe
  port_mean <- mean(port_series) * freq
  ds_up <- approx_dret_dw(series, w, dw)$ds_up
  ds_down <- approx_dret_dw(series, w, dw)$ds_down

  port_ret_var <- port_mean * perc_ret_var

  delta_w_up <- dw * port_ret_var[1] / ds_up
  delta_w_down <- dw * port_ret_var[1] / ds_down

  w_ret_upper <- sapply(w + mapply(max, delta_w_up, delta_w_down), min, 1)
  w_ret_lower <- sapply(w + mapply(min, delta_w_up, delta_w_down), max, 0)

  ## Volatility
  d_up <- approx_dvol_dw(series, w, dw)$dvol_up_annual
  d_down <- approx_dvol_dw(series, w, dw)$dvol_down_annual
  port_vol_ann <- port_vol * freq
  port_var <- port_vol_lim - port_vol_ann

  delta_w_up <- dw * port_var[1] / d_up
  delta_w_down <- dw *port_var[1] / d_down

  w_risk_upper <- sapply(w + mapply(max, delta_w_up, delta_w_down), min, 1)
  w_risk_lower <- sapply(w + mapply(min, delta_w_up, delta_w_down), max, 0)

  w_upper <- mapply(min, mapply(min, w_bias_upper, w_risk_upper), w_ret_upper)
  w_lower <- mapply(max, mapply(max, w_bias_lower, w_risk_lower), w_ret_lower)

  w_limits_table <- cbind(w, w_lower, w_upper, w_bias_lower,  w_bias_upper, w_risk_lower, w_risk_upper,  w_ret_lower, w_ret_upper)

  return(list(w_upper = w_upper, w_lower = w_lower, w_limits_table = w_limits_table))
}
