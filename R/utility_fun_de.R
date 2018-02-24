#' Objective function
#'
#' Utility function.
#' @param type Type of objective function. absolute or relative.
#' @param mu Expected return
#' @param Sigma covariance matrix
#' @param lambda risk aversion coefficient
#' @param w_bench Benchmark weigths
#' @param lb Lower bound per asset
#' @param ub Upper bound per asset
#' @param lb_act Lower bound active weight per asset
#' @param ub_act Upper bound active weight per asset
#' @param min_var Min-Var portfolio indicator
#' @param vol_obj Volatility objective. Default: Inf
#' @param f_const Matrix of factor constraints. Each entry is 0 or 1. Dimesion is NxM where N is the number of constraints and M is the number of assets.
#' @param f_const_lb Vector of lower bounds of each constraint.
#' @param f_const_ub Vector of upper bounds of each constraint.
#' @param same_assets_bench Indicator function. If the portfolio and the benchmark assets are the same TRUE, otherwise FALSE.
#' @param Sigma_bench covariance matrix of benchmark asset. Only takes values when the benchmark assets are different from portfolio assets.
#' @return Objective function.
#' @export

utility_fun_de <- function(type = 'absolute', mu, Sigma, lambda, w_bench = NULL, lb = rep(0, length(mu)), ub = rep(1, length(mu)), lb_act = rep(0, length(mu)), ub_act = rep(1, length(mu)), min_var = FALSE, vol_obj = Inf, f_const = rep(1, length(mu)), f_const_lb = 0, f_const_ub = Inf, same_assets_bench = TRUE, Sigma_bench = NULL) {
  if(type == 'absolute'){
    if (min_var == TRUE){
      function(w){
        if (sum(w)!=1){w <- w/sum(w)}
        if(any(w<lb) || any(w>ub)) {return(Inf)}
        util <- t(w)%*%Sigma%*%w
        as.numeric(util)
      }
    }else{
      function(w) {
        if (sum(w)!=1){w <- w/sum(w)}
        if(any(w<lb) || any(w>ub) || as.numeric(sqrt(t(w) %*% Sigma %*% w))>vol_obj || any((f_const %*% w) < f_const_lb) || any((f_const %*% w) > f_const_ub)) {return(Inf)}
        util <- -(t(w) %*% mu - 0.5 * lambda * (!is.finite(vol_obj)) * t(w) %*% Sigma %*% w)
        return(as.numeric(util))
      }
    }
  }else if(type == 'relative'){
    if(same_assets_bench){
      function(w_act){
        if (sum(w_act)!=0){w_act[w_act > 0] <- abs(w_act[w_act > 0] * sum(w_act[w_act < 0]) / sum(w_act[w_act > 0]))}
        if(any(w_act<lb_act) || any(w_act>ub_act) || any((w_act + w_bench) < lb) || any((w_act + w_bench)>ub) || as.numeric(sqrt(t(w_act) %*% Sigma %*% w_act))>vol_obj || any((f_const %*% (w_act+w_bench)) < f_const_lb) || any((f_const %*% (w_act+w_bench)) > f_const_ub)) {return(Inf)}
        util <- -(t(w_act+w_bench)%*%mu - 0.5 * lambda * (!is.finite(vol_obj)) * t(w_act)%*%Sigma%*%w_act)
        as.numeric(util)
      }
    }else{
      function(w) {
        if (sum(w)!=1){w <- w/sum(w)}
        if(any(w<lb) || any(w>ub) || (as.numeric(sqrt(t(w) %*% Sigma %*% w)) - as.numeric(sqrt(t(w_bench) %*% Sigma_bench %*% w_bench)))>vol_obj || any((f_const %*% w) < f_const_lb) || any((f_const %*% w) > f_const_ub)) {return(Inf)}
        util <- -(t(w) %*% mu - 0.5 * lambda * (!is.finite(vol_obj)) * t(w) %*% Sigma %*% w)
        return(as.numeric(util))
      }
    }
  }
}
