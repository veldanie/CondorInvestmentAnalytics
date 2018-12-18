
#' Exposure limits functional
#'
#' Exposure limits functional.
#' @param f_const Matrix of 0 1 to build weight sums to limit exposure.
#' @return Exposure limit function.
#' @export

exp_limit_fun <- function(f_const) {
  function(w) f_const %*% w
}
