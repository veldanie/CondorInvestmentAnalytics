
#' Exposure limits functional
#'
#' Exposure limits functional.
#' @param f_const Matrix of 0 1 to build weight sums to limit exposure.
#' @param ind_rel Indicator of relative restriction of an asset wit respect to a group.
#' @param ind_asset Indicator of relative asset restriction.
#' @return Exposure limit function.
#' @export

exp_limit_fun <- function(f_const, ind_rest, ind_asset) {
  if(any(ind_rest)){
    function(w){
      w_rest <- as.numeric(f_const %*% w)
      w_rest[ind_rest] <- w[ind_asset]/w_rest[ind_rest]
      return(w_rest)
    }
  }else{
    function(w) as.numeric(f_const %*% w)
  }
}
