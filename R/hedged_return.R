
#' Hedged_return.
#'
#' Estimates hedged return.
#' @param hedge Hedged percentage.
#' @param ret Return in foreign (domectic) currency.
#' @param fx_ret Fx return.
#' @param fwd_prem Forward premium.
#' @param exp_ret Expected return over initial capital (1) over investment horizon.Given the difficulties to estimate the this value, it is assumed to be cero.
#' @return
#' @export

hedged_return <- function(hedge, ret, fx_ret, fwd_prem, exp_ret = 0) {
  h_ret <- hedge * (1 + exp_ret) * (1 + fwd_prem) + (1 - hedge) * (1 + exp_ret) * (1 + fx_ret) + (ret - exp_ret) * (1 + fx_ret) - 1
  return(h_ret)
}
