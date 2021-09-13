#' Min tracking error portfolio
#'
#' Min tracking error portfolio
#' @param assets Assets vector,
#' @param series_bench Benchmark xts series,
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

min_te_portfolio <- function(assets, series_list, asset_data, ref_curr, series_bench=NULL, w_bench=NULL, lb=NULL, ub=NULL, lb_act = NULL, ub_act = NULL, dates=NULL, period="monthly", min_w=0){
  if(is.null(lb)){lb <- rep(0, length(assets))}
  if(is.null(ub)){ub <- rep(1, length(assets))}
  if(is.null(dates)){dates <- dmy(c("01012000","01012050"))}
  
  if (is.null(series_bench) & is.null(w_bench)){
    stop('Both series_bench and w_bench cannot be NULL!')
  }  
  
  if (!is.null(series_bench)){
    series <- series_merge(series_list, asset_data, dates = dates, ref_curr, assets = assets, convert_to_ref = TRUE)
    series_sync <- merge.xts(series, series_bench,join = "inner")
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
    te <- sqrt(as.numeric(c(w,-1) %*% Sigma[c(names(w),'Benchmark'),c(names(w),'Benchmark')] %*% c(w,-1)))
    
  }else{
    if(all(names(w_bench) %in% assets) & length(assets)==length(w_bench)){
      series <- series_merge(series_list, asset_data, dates = dates, ref_curr, assets = assets, convert_to_ref = TRUE)
      Sigma <- covar(returns(series, period=period))$cov_matrix_ann
      te_obj_fun <- function(x){
        tvar <- as.numeric(x %*% Sigma %*% x)
        return(tvar)
      }
      w_ini <- rep(0, length(assets))
      sol <- auglag(x0 = w_ini, fn = te_obj_fun, lower = lb_act, upper = ub_act, heq = function(w) sum(w), localsolver = c("SLSQP"))
      w <- sol$par
      names(w) <- assets
      te <- sqrt(as.numeric(w %*% Sigma %*% w))
      
    }else{
      assets_temp <- c(assets, names(w_bench))
      series <- series_merge(series_list, asset_data, dates = dates, ref_curr, assets = assets_temp, convert_to_ref = TRUE)
      Sigma <- covar(returns(series, period=period))$cov_matrix_ann
      te_obj_fun <- function(x){
        tvar <- as.numeric(c(x,-w_bench) %*% Sigma %*% c(x,-w_bench))
        return(tvar)
      }
      w_ini <- lb + (1 - sum(lb)) * (ub - lb)/sum(ub - lb)
      sol <- auglag(x0 = w_ini, fn = te_obj_fun, lower = lb, upper = ub, heq = function(w) sum(w) - 1, localsolver = c("SLSQP"))
      w <- sol$par
      names(w) <- assets
      w <- w[w > min_w]/sum(w[w > min_w])
      te <- sqrt(as.numeric(c(w,-w_bench) %*% Sigma[c(names(w),names(w_bench)),c(names(w),names(w_bench))] %*% c(w,-w_bench)))
    }

  }


  return(list(w=w, te=te))
}
