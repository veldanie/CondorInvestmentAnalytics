#' Objective function
#'
#' Utility function.
#' @param type of objective function. absolute or relative.
#' @param mu Expected return
#' @param Sigma covariance matrix
#' @param lambda risk aversion coefficient
#' @param w_bench Benchmark weigths
#' @param min_var Min-Var portfolio indicator
#' @return equilibrium returns. Annualized and not. The return is assumed to be continuous, i.e. estimated using natural log.
#' @export

utility_fun <- function(type = 'absolute', mu, Sigma, lambda, w_bench = NULL, min_var = FALSE) {
  if(type == 'absolute'){
    if (min_var == TRUE){
      function(w){
        util <- t(w)%*%Sigma%*%w
        as.numeric(util)
      }
    }else{
      function(w){
        util <- -(t(w)%*%mu - 0.5 * lambda * t(w)%*%Sigma%*%w)
        as.numeric(util)
      }
    }
  }else if(type == 'relative'){
    function(w_act){
      util <- -(t(w_act+w_bench)%*%mu - 0.5 * lambda * t(w_act)%*%Sigma%*%w_act)
      as.numeric(util)
    }
  }
}
