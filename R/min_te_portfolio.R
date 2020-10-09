#' Min tracking error portfolio
#'
#' Min tracking error portfolio
#' @param assets Assets vector,
#' @param benchmark_series Benchmark xts series,
#' @param series_list Series list,
#' @param asset_data Assets dataframe,
#' @param ref_curr reference currency,
#' @param lb Lower bounds,
#' @param ub Upper bounds,
#' @param dates Reference dates,
#' @param period Returns period,
#' @param min_w Minimun weight,
#' @return weights.
#' @export

min_te_portfolio <- function(assets, benchmark_series, series_list, asset_data, ref_curr, lb=NULL, ub=NULL, dates=NULL, period="monthly", min_w=0){
  if(is.null(lb)){lb <- rep(0, length(assets))}
  if(is.null(ub)){ub <- rep(1, length(assets))}
  if(is.null(dates)){dates <- dmy(c("01012000","01012050"))}

  series <- series_merge(series_list, asset_data, dates = dates, ref_curr, assets = assets, convert_to_ref = TRUE)
  series_sync <- merge.xts(series, benchmark_series,join = "inner")
  colnames(series_sync) <- c(colnames(series), "Benchmark")
  Sigma <- covar(returns(series_sync, period=period))$cov_matrix_ann
  te_obj_fun <- function(x){
    tvar <- as.numeric(c(x,-1) %*% Sigma %*% c(x,-1))
    return(tvar)
  }

  w_ini <- lb + (1 - sum(lb)) * (ub - lb)/sum(ub - lb)
  sol <- auglag(x0 = w_ini, fn = te_obj_fun, lower = lb, upper = ub,
                heq = function(w) sum(w) - 1, localsolver = c("SLSQP"))

  w <- sol$par
  names(w) <- assets
  w <- w[w > min_w]/sum(w[w > min_w])
  return(w)
}
