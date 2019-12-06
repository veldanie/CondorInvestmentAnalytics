
#' Assets exposure to factors.
#'
#' Estimates losses distribution, VaR amd CVaR.
#' @param assets_rets Assets xts series.
#' @param factors_rets Factors xts series.
#' @param lambdas Penalization coefs.
#' @return Matrix n_assets x n_factors of exposures.
#' @export

asset_to_factor_exp <- function(assets_rets, factors_rets, lambdas=NULL){
  n_assets <- ncol(assets_rets)
  A = matrix(0, n_assets, ncol(factors_rets))
  for (i in 1:n_assets){
    asset_rets = assets_rets[,i]
    dim(factors_rets)
    dim(asset_rets)
    model_i = cv.glmnet(x=coredata(factors_rets), y=as.vector(asset_rets), lambda = lambdas)
    model_i$glmnet.fit
    fit <- glmnet(x = coredata(factors_rets), y=as.vector(asset_rets), family='gaussian', alpha = 0,
                  lambda = model_i$lambda.min)
    coef <- t(round(as.matrix(fit$beta),5))
    A[i,] = coef
  }
  rownames(A) <- colnames(assets_rets)
  colnames(A) <- colnames(factors_rets)
  return (A)
}
