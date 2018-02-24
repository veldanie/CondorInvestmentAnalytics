#' Objective function
#'
#' Utility function.
#' @param type of objective function. absolute or relative.
#' @param mu Expected return
#' @param Sigma covariance matrix
#' @param lambda risk aversion coefficient
#' @param w_bench Benchmark weigths
#' @param min_var Indicator Min-Var portfolio
#' @param same_assets_bench Indicator function. If the portfolio and the benchmark assets are the same TRUE, otherwise FALSE.
#' @return Objective function.
#' @export

utility_fun <- function(type = 'absolute', mu, Sigma, lambda, w_bench = NULL, min_var = FALSE, same_assets_bench = TRUE) {
  if(type == 'absolute' | !same_assets_bench){
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
