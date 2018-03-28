#' Conditional Drawdown
#'
#' Estimates de covariance using weighted averages of products of past returns.
#' @param series data frame series.
#' @param w Portfolio weights.
#' @param pers_end_ind Periods endpoint index.
#' @param pers_matrix Periods matrix. Used to speed up computations and parallel computing.
#' @param quant Quantile.
#' @param type Type of returns: arithmetic (discrete) or log (continuous)
#' @return Conditional Drawdown.
#' @export

cond_drawdown <- function(series, w, pers_end_ind, pers_matrix, quant = 0.9, type = 'log') {
  if(is.null(series)){ cond_dd <- 0
  }else{
    w_mat <- tcrossprod(rep(1, nrow(series)),w)
    series_ret <- w_mat * log(series/tcrossprod(rep(1, nrow(series)), as.numeric(series[1,])))
    port_ret <- xts(as.numeric(rowSums(series_ret)), order.by = index(series_ret))
    #dd_obs <- do.call(pmax,data.frame(t(pers_matrix * (port_ret %*% t(rep(1, ncol(pers_matrix))))))) - as.vector(port_ret[findInterval(ymd(substr(pers, 12,21)),index(port_ret))]),
    #dd_obs <- colMaxRcpp((pers_matrix * (port_ret %*% t(rep(1, ncol(pers_matrix)))))) - as.vector(port_ret[pers_end_ind]),
    dd_obs <- colMaxs((pers_matrix * tcrossprod(port_ret,rep(1, ncol(pers_matrix))))) - as.vector(port_ret[pers_end_ind])
    max_dd <- quantile(dd_obs, probs = quant)
    cond_dd <- mean(dd_obs[dd_obs >= max_dd])
  }
    return(cond_dd)
}


