
#' Maximize deviation.
#'
#' Maximize deviation from benchmark given risk restriction.
#' @param w Portfolio weights.
#' @param series return series.
#' @param risk_target risk target
#' @param ow_assets Overweight assets.
#' @param uw_assets Underweight assets.
#' @param per returns period expressed as number of times per year..
#' @param type weight deviation type.
#' @param risk Risk measure: vol, te, var, cvar.
#' @param quant Quantile.
#' @return Risk objective function.
#' @export


optim_dev <- function(w, series, risk_target, ow_assets, uw_assets, per = 12, type = "uniform", risk = "vol", quant = 0.95) {
  obj_fun <- obj_dev_fun(w, series, risk_target, ow_assets, uw_assets, per, substr(tolower(type), 1,4), risk, quant)
  opt_dev <- try(nlminb(start = 0, obj_fun), silent = TRUE)
  if(class(opt_dev)=="try-error"){
    return(0)
  }else{
    return(opt_dev$par)
  }
}


