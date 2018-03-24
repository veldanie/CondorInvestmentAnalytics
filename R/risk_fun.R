
#' Risk functional
#'
#' Risk function.
#' @param Sigma covariance matrix
#' @param type Type of risk measure. ('volatility','var', 'cvar')
#' @param quant Quantile.
#' @return Risk function
#' @export

risk_fun <- function(Sigma, type = 'volatility', quant = 0.9) {
  if(substr(type,1,3) == 'vol'){
    function(w) as.numeric(sqrt(t(w) %*% Sigma %*% w))
  }else if (substr(type,1,3) == 'var'){
    function(w) qnorm(quant) * as.numeric(sqrt(t(w) %*% Sigma %*% w))
  }else if(substr(type,1,3) == 'cva'){
    function(w) dnorm(qnorm(quant))/(1-quant) * as.numeric(sqrt(t(w) %*% Sigma %*% w))
  }
}
