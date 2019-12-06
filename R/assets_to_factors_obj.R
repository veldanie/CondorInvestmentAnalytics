
#' Factors portfolio objective function.
#'
#' Generates loss function.
#' @param w_a Assets weights.
#' @param Sigma Factors cov matrix.
#' @param Q Assets cov matrix.
#' @param A Assets exp to factors.
#' @param gamma Active risk penalty param.
#' @return Loss function.
#' @export

assets_to_factors_obj <- function(w_a, Sigma, Q, A, gamma=0.5){
  return(function(x){
    exp_diff = as.vector(t(w_a) %*% A - t(x))
    loss = (1-gamma)*as.numeric(t(exp_diff) %*% exp_diff) + gamma * as.numeric(t(exp_diff) %*% Sigma %*% exp_diff) + gamma * as.numeric(t(w_a) %*% Q %*% w_a)
    return(loss)
  })
}
