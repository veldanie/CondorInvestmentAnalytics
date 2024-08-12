
#' Exposure limits functional
#'
#' Exposure limits functional.
#' @param f_const Matrix of 0 1 to build weight sums to limit exposure.
#' @param ind_rel Indicator of relative restriction of an asset wit respect to a group.
#' @param pos_asset Matrix of assets positions for relative restriction.
#' @param ind_dur_rest Indicator of duration restriction.
#' @param w_adj Adjustment to w. E.g funds.
#' @return Exposure limit function.
#' @export

exp_limit_fun <- function(f_const, ind_rel, pos_asset, ind_dur_rest=FALSE, w_adj=rep(1, ncol(f_const)), w_bench=0) {
  if(any(ind_rel) && !any(ind_dur_rest)){
    function(w){
      w_rest <- as.numeric(f_const %*% (w+w_bench))
      w_rest[ind_rel] <- (pos_asset %*% (w+w_bench)) /w_rest[ind_rel]
      return(w_rest)
    }
  }else if(any(ind_rel) && any(ind_dur_rest)){
    function(w){
      w_rest <- as.numeric(f_const %*% (w+w_bench))
      w_rest[ind_rel] <- (pos_asset %*% (w+w_bench)) /w_rest[ind_rel]
      w_rest[ind_dur_rest] <- as.numeric(f_const %*% ((w+w_bench)*w_adj))[ind_dur_rest]/((f_const[ind_dur_rest,]>0) %*% ((w+w_bench)*w_adj))
      return(w_rest)
    }
  }else if(!any(ind_rel) && any(ind_dur_rest)){
    function(w){
      w_rest <- as.numeric(f_const %*% ((w+w_bench)*w_adj))
      w_rest[ind_dur_rest] <- w_rest[ind_dur_rest]/((f_const[ind_dur_rest,]>0) %*% ((w+w_bench)*w_adj))
      return(w_rest)
    }
  }else{
    function(w) as.numeric(f_const %*% (w+w_bench))
  }
}
