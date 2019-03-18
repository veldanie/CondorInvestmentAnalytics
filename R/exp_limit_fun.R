
#' Exposure limits functional
#'
#' Exposure limits functional.
#' @param f_const Matrix of 0 1 to build weight sums to limit exposure.
#' @param ind_rel Indicator of relative restriction of an asset wit respect to a group.
#' @param pos_asset Matrix of assets positions for relative restriction.
#' @param ind_dur_rest Indicator of duration restriction.
#' @return Exposure limit function.
#' @export

exp_limit_fun <- function(f_const, ind_rel, pos_asset, ind_dur_rest=FALSE) {
  if(any(ind_rel) && !any(ind_dur_rest)){
    function(w){
      w_rest <- as.numeric(f_const %*% w)
      w_rest[ind_rel] <- (pos_asset %*% w) /w_rest[ind_rel]
      return(w_rest)
    }
  }else if(any(ind_rel) && any(ind_dur_rest)){
    function(w){
      w_rest <- as.numeric(f_const %*% w)
      w_rest[ind_rel] <- (pos_asset %*% w) /w_rest[ind_rel]
      w_rest[ind_dur_rest] <- w_rest[ind_dur_rest]/((f_const[ind_dur_rest,]>0) %*% w)
      return(w_rest)
    }
  }else if(!any(ind_rel) && any(ind_dur_rest)){
    function(w){
      w_rest <- as.numeric(f_const %*% w)
      w_rest[ind_dur_rest] <- w_rest[ind_dur_rest]/((f_const[ind_dur_rest,]>0) %*% w)
      return(w_rest)
    }
  }else{
    function(w) as.numeric(f_const %*% w)
  }
}
