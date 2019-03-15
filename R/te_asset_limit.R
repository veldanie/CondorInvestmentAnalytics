
#' Asset deviation limit given a TE target.
#'
#' Asset deviation limit given a TE target.
#' @param w_bench Portfolio weights,
#' @param Sigma Annualized covariance matrix.
#' @param te_target Traking error target.
#' @return Asset id.
#' @export

te_asset_limit <- function(w_bench, Sigma, te_target=0) {
  w_act_limit <- rep(0, length(w_bench))
  names(w_act_limit) <- names(w_bench)
  if(te_target!=0){
    te_obj <- function(asset_dev, w_bench, Sigma, asset_pos, te_target){
      assets_pos <- setdiff(1:length(w_bench), asset_pos)
      w_act <- rep(0, length(w_bench))
      w_act[asset_pos] <- asset_dev
      w_act[assets_pos] <-  -asset_dev*w_bench[-asset_pos]/sum(w_bench[-asset_pos])
      return((as.numeric(sqrt(t(w_act) %*% Sigma %*% w_act)) - te_target)^2)
    }

    w_act_limit <- w_bench
    for(i in 1:length(w_bench)){
      w_act_limit[i] <- nlminb(0, te_obj, gradient = NULL, hessian = NULL, w_bench, Sigma, i, te_target, upper = 1-w_bench[i], lower = 0)$par
    }
  }
  return(w_act_limit)
}

