
#' Assets exposure to factors.
#'
#' Estimates losses distribution, VaR amd CVaR.
#' @param assets_rets Assets xts series.
#' @param factors_rets Factors xts series.
#' @param lambdas Penalization coefs.
#' @return Matrix n_assets x n_factors of exposures.
#' @export

asset_to_factor_exp <- function(assets_rets, factors_rets, lambdas=NULL, nfolds=5, seed = 123){
  n_assets <- ncol(assets_rets)
  A = matrix(0, n_assets, ncol(factors_rets))
  set.seed(seed)
  for (i in 1:n_assets){
    asset_rets = assets_rets[,i]
    foldid = sample(c(1:nfolds), nrow(factors_rets), replace = TRUE)
    model_i = cv.glmnet(x=coredata(factors_rets), y=as.vector(asset_rets), lambda = lambdas, foldid = foldid)
    fit <- glmnet(x = coredata(factors_rets), y=as.vector(asset_rets), family='gaussian', alpha = 0,
                  lambda = model_i$lambda.min)
    coef <- t(round(as.matrix(fit$beta),5))
    A[i,] = coef
  }
  rownames(A) <- colnames(assets_rets)
  colnames(A) <- colnames(factors_rets)
  return (A)
}
