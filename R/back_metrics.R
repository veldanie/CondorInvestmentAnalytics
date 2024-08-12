
#' Relative and absolute portfolio and benchmark risk and return metrics
#'
#' Relative and absolute portfolio and benchmark risk and return metrics
#' @param bench_series xts benchmark series.
#' @param port_series xts Portfolio series
#' @param period daily, monthly or quarterly returns
#' @param port_names Header
#' @param factor_val Constant thar multiplies output metrics
#' @return Absolute and relative metrics dataframes.
#' @export

back_metrics <- function(bench_series, port_series, dates=NULL, period="monthly", port_names=c("Benchmark", "Portafolio"), factor_val=100){

  if(is.null(bench_series)){
    series <- port_series
  }else if(is.null(port_series)){
    series <- bench_series
  }else{
    series <- merge.xts(as.xts(bench_series), as.xts(port_series), join = "inner")
  }
  colnames(series) <- port_names[c(!is.null(bench_series), !is.null(port_series))]
  if(!is.null(dates)){
    series <- series[paste0(dates, collapse = "/")]
  }
  freq <- switch(period, 'daily' = 252, 'monthly' = 12, 'quarterly' = 4, 'semiannualy' = 2)
  
  rets <- returns(series, period = period)
  
  n_y <- as.numeric(tail(index(series),1) - index(series)[1])/365
  avg_ret <- colMeans(rets)
  vols <- apply(rets, 2, sd)
  
  total_ret <- as.numeric(tail(series, 1))/as.numeric(series[1,]) - 1
  ann_avg_ret <- avg_ret*freq
  ann_per_ret <- (1 + total_ret)**(1/n_y) - 1
  ann_vol <- vols*sqrt(freq)
  sharpe <- avg_ret/vols
  
  mets_abs <- data.frame(cbind(c('Retorno Promedio', "Retorno Prom. Anual.", "Retorno Total", "Retorno Total Anual.", "Volatilidad", "Sharpe"), round(rbind(factor_val*avg_ret, factor_val*ann_avg_ret, factor_val*total_ret, factor_val*ann_per_ret, factor_val*ann_vol, sharpe), 3)))
  colnames(mets_abs) <- c("Variable", colnames(rets))
  
  mets_rel <- NULL
  if(!is.null(bench_series) && !is.null(bench_series)){
    alpha_dist <- rets[,2] - rets[,1] 
    te <- sd(alpha_dist)
    active_ret <- avg_ret[2] - avg_ret[1]
    ann_active_ret <- active_ret*freq
    ann_te <- te*sqrt(freq)
    info_ratio <- round(ann_active_ret/ann_te,3)
    
    mets_abs <- data.frame(cbind(c('Retorno Promedio', "Retorno Prom. Anual.", "Retorno Total", "Retorno Total Anual.", "Volatilidad", "Sharpe"), round(rbind(factor_val*avg_ret, factor_val*ann_avg_ret, factor_val*total_ret, factor_val*ann_per_ret, factor_val*ann_vol, sharpe), 3)))
    colnames(mets_abs) <- c("Variable", port_names)
    
    mets_rel <- data.frame(Variable=c("Alpha", "Tracking Error Anual.", "IR"), Valor=c(factor_val*ann_active_ret, factor_val*ann_te, info_ratio))
  }

  return(list(mets_abs=mets_abs, mets_rel=mets_rel))
}
