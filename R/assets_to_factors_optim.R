
#' Factors portfolio.
#'
#' Converts assets port to factors portfolio.
#' @param w_a Assets weights.
#' @param assets_rets Assets returns xts.
#' @param factors_rets Factors returns xts.
#' @param lambdas Regression penal. parameters.
#' @param gamma Active risk penalty.
#' @return Factors portfolio vector.
#' @export
#'

assets_to_factors_optim <- function(w_a, assets_rets, factors_rets, lambdas=NULL, gamma=0){
  if(nrow(assets_rets)!=nrow(factors_rets)){
    stop("assets and factors series must be sync!")
  }
  n_factors <- ncol(factors_rets)
  A = asset_to_factor_exp(assets_rets[,names(w_a)], factors_rets, lambdas=lambdas)
  Sigma <- cov(factors_rets)
  Q <- cov(assets_rets[,names(w_a)])
  lb = rep(0, n_factors)
  ub = rep(1, n_factors)
  w_ini <- rep(1/n_factors, n_factors)
  obj_fun <- assets_to_factors_obj(w_a, Sigma, Q, A, gamma=0.5)
  sol <- try(auglag(x0 = w_ini, fn = obj_fun, lower = lb, upper = ub,
                    heq = function(w) sum(w) - 1, localsolver = c("SLSQP")),
             silent = TRUE)

  if (class(sol) != "try-error" && sol$convergence > 0 && !all(sol$par == w_ini)) {
    w_f <- round(sol$par*100, 3)
    names(w_f) <- colnames(factors_rets)
  }
  return (w_f[w_f>0])
}
