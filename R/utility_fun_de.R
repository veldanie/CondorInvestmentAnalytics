#' Objective function
#'
#' Utility function.
#' @param type Type of objective function. absolute or relative.
#' @param mu Expected return
#' @param Sigma covariance matrix
#' @param lambda risk aversion coefficient
#' @param risk_fun Risk as a function of w.
#' @param w_bench Benchmark weigths
#' @param lb Lower bound per asset
#' @param ub Upper bound per asset
#' @param lb_act Lower bound active weight per asset
#' @param ub_act Upper bound active weight per asset
#' @param min_var Min-Var portfolio indicator
#' @param risk_obj Risk objective. Default: Inf
#' @param f_const_fun Function of factor constraints. Inside there is a NxM matrix where N is the number of constraints and M is the number of assets.
#' @param f_const_lb Vector of lower bounds of each constraint.
#' @param f_const_ub Vector of upper bounds of each constraint.
#' @param same_assets_bench Indicator function. If the portfolio and the benchmark assets are the same TRUE, otherwise FALSE.
#' @param Sigma_bench covariance matrix of benchmark asset. Only takes values when the benchmark assets are different from portfolio assets.
#' @param dd_obj Drawdown objective. Default: Inf
#' @param series Assets prices series. Requiered to estimate drawdown.
#' @param dd_pers_end_ind Drawdown periods endpoint index..
#' @param dd_pers_matrix Drawdown periods matrix.
#' @param dd_quant Quantile for max drawdown.
#' @param vol_diff When type=relative and benchmark and port. assets are different, it is possible to make the risk target the vol. difference.
#' @return Objective function. If type == 'absolute', portfolio risk is controlled by the risk function. If type == 'relative' risk measure can only be volatility.
#' @export

utility_fun_de <- function(type = 'absolute', mu, Sigma, lambda, risk_fun = NULL, w_bench = NULL, lb = rep(0, length(mu)), ub = rep(1, length(mu)), lb_act = rep(0, length(mu)), ub_act = rep(1, length(mu)), min_var = FALSE, risk_obj = Inf, f_const_fun = function(x) rep(1, length(mu)) %*% x, f_const_lb = 0, f_const_ub = Inf, same_assets_bench = TRUE, Sigma_bench = NULL, dd_obj = Inf, series = NULL, dd_pers_end_ind = NULL, dd_pers_matrix = NULL, dd_quant = 0.9, vol_diff = FALSE) {
  if(type == 'absolute'){
    if (min_var == TRUE){
      function(w){
        if (sum(w)!=1){w <- w/sum(w)}
        if(any(w<(lb-1e-7)) || any(w>(ub+1e-7))) {return(1000)}
        util <- t(w)%*%Sigma%*%w
        as.numeric(util)
      }
    }else{
      function(w) {
        if (sum(w)!=1){
          #w <- w_norm_fun(w)
          #w <- w/sum(w)
          w <- w-(sum(w)-1)*(w-lb)/sum(w-lb)
        }
        if(any(w<(lb-1e-7)) || any(w>(ub+1e-7)) || risk_fun(w)>risk_obj || any(f_const_fun(w) < f_const_lb) || any(f_const_fun(w) > f_const_ub) || cond_drawdown(series, w, dd_pers_end_ind, dd_pers_matrix, dd_quant) > dd_obj) {return(1000)}
        util <- -(t(w) %*% mu - 0.5 * lambda * (!is.finite(risk_obj)) * t(w) %*% Sigma %*% w)
        return(as.numeric(util))
      }
    }
  }else if(type == 'relative'){
    if(same_assets_bench){
      lower_act <- -mapply(min, w_bench - lb, abs(lb_act))
      upper_act <- mapply(min, sapply(ub - w_bench, max, 0), ub_act)

      function(w_act){
        if (sum(w_act)!=0){
          #w_act[w_act > 0] <- abs(w_act[w_act > 0] * sum(w_act[w_act < 0]) / sum(w_act[w_act > 0]))
          w_act <- w_act-sum(w_act)*(w_act-lower_act)/sum(w_act-lower_act)
        }
        if(any(w_act<(lower_act-1e-7)) || any(w_act>(upper_act+1e-7)) || any((w_act + w_bench) < (lb-1e-7)) || any((w_act + w_bench)>(ub+1e-7)) || as.numeric(sqrt(t(w_act) %*% Sigma %*% w_act))>risk_obj || any(f_const_fun (w_act+w_bench) < f_const_lb) || any(f_const_fun(w_act+w_bench) > f_const_ub) || cond_drawdown(series, w_act+w_bench, dd_pers_end_ind, dd_pers_matrix, dd_quant) > dd_obj) {return(1000)}
        util <- -(t(w_act+w_bench)%*%mu - 0.5 * lambda * (!is.finite(risk_obj)) * t(w_act)%*%Sigma%*%w_act)
        as.numeric(util)
      }
    }else{
      if(vol_diff){
        function(w) {
          if (sum(w)!=1){
            # w <- w/sum(w)
            w <- w-(sum(w)-1)*(w-lb)/sum(w-lb)
          }
          if(any(w<(lb-1e-7)) || any(w>(ub+1e-7)) || (as.numeric(sqrt(t(w) %*% Sigma %*% w)) - as.numeric(sqrt(t(w_bench) %*% Sigma_bench %*% w_bench)))>risk_obj || any(f_const_fun(w) < f_const_lb) || any(f_const_fun(w) > f_const_ub) || cond_drawdown(series, w, dd_pers_end_ind, dd_pers_matrix, dd_quant) > dd_obj) {return(1000)}
          util <- -(t(w) %*% mu - 0.5 * lambda * (!is.finite(risk_obj)) * t(w) %*% Sigma %*% w)
          return(as.numeric(util))
        }
      }else{
        function(w) {
          if (sum(w)!=1){
            # w <- w/sum(w)
            w <- w-(sum(w)-1)*(w-lb)/sum(w-lb)
          }
          if(any(w<(lb-1e-7)) || any(w>(ub+1e-7)) || as.numeric(sqrt(t(c(w, -w_bench)) %*% Sigma_bench %*% c(w, -w_bench))) >risk_obj || any(f_const_fun(w) < f_const_lb) || any(f_const_fun(w) > f_const_ub) || cond_drawdown(series, w, dd_pers_end_ind, dd_pers_matrix, dd_quant) > dd_obj) {return(1000)}
          util <- -(t(w) %*% mu - 0.5 * lambda * (!is.finite(risk_obj)) * t(w) %*% Sigma %*% w)
          return(as.numeric(util))
        }
      }
    } #else
  }
}
