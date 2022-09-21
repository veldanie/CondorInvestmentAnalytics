#' rolling_windows_metrics 
#'
#' Create a rolling window from the data, applying function. Get distribution by quantile.
#' @param data_series xts portfolios series.
#' @param returns boolean the data are returns
#' @param period returns period, as string ('daily','monthly','quarterly','semiannualy')
#' @param port_names Header names.
#' @param funct Function to apply 
#' @param windows_width integer specifying the window width (in numbers of observations) which is aligned to the original sample according to the align argument
#' @param align specifyies whether the index of the result should be left- or right-aligned or centered [c("center", "left", "right")] compared to the rolling window of observations.
#' @param quantile_dist Quantile for the distribution
#' @param factor_val Constant thar multiplies output metrics
#' @return list with data and metrics
#' @export

rolling_windows_metrics <- function(data_series, returns = FALSE, period = "monthly", port_names = NULL, func = mean, windows_width = 12, align = c('right'), quantile_dist = 5, factor_val = 100){
  if (!returns){
    data_series <- returns(data_series, period=period)
  }
  ports_rolling_fun <- na.omit(rollapply(data_series, windows_width, func,  align = align))
  if(!is.null(port_names)){
    colnames(ports_rolling_fun) <- port_names
  }
  var_rets_dw <- apply(ports_rolling_fun, 2, function (x) round(quantile(x, probs = quantile_dist/100)*factor_val, 3))
  var_rets_up <- apply(ports_rolling_fun, 2, function (x) round(quantile(x, probs = 1 - quantile_dist/100)*factor_val, 3))
  var_date <- format(index(ports_rolling_fun[apply(abs(ports_rolling_fun-var_rets_dw), 2, which.min)]), "%b%Y")
  mean_ret <- apply(ports_rolling_fun, 2, function(x) round(mean(x)*factor_val,3))
  xticks <- lapply(ports_rolling_fun, function(x) pretty(x)*factor_val)
  return(list(rolling_data = ports_rolling_fun*factor_val, var_rets_dw = var_rets_dw, var_rets_up = var_rets_up, var_date = var_date, mean_ret = mean_ret, xticks = xticks))
}
