
#' mult_port_metrics
#'
#' Calculate absolute metrics to multiple benchmarks and portfolios, and calculate relative metrics between benchmarks and portfolios series.
#' The benchmark and portfolio series must have the same shape, the relative metrics are calculated one to one (columns)
#'
#' @param bench_series xts serie of multiples benchmarks
#' @param port_series xts serie of multiples portfolios
#' @param period Period
#' @param factor_val Valuation factor
#' @return List of returns, information, absolute metrics to benchmark and portfolio series and relative metrics.
#' @export

mult_port_metrics <- function (bench_series, port_series, period = "monthly", factor_val = 100)
{
  if(length(bench_series)!=length(port_series)){
    warning(paste0('Benchmark and port tables must have same columns and rows lenghts'))
  }

  ## Absolute Metrics
  freq <- switch(period, daily = 252, monthly = 12, quarterly = 4,
                 semiannualy = 2)
  rets_bench <- returns(bench_series)
  rets_port <- returns(port_series)
  n_y <- as.numeric(tail(index(port_series), 1) - index(port_series)[1])/365
  avg_ret_bench <- colMeans(rets_bench)
  avg_ret_port <- colMeans(rets_port)
  vols_bench <- apply(rets_bench, 2, sd)
  vols_port <- apply(rets_port, 2, sd)
  total_ret_bench <- as.numeric(tail(bench_series, 1))/as.numeric(bench_series[1,]) - 1
  total_ret_port <- as.numeric(tail(port_series, 1))/as.numeric(port_series[1,]) - 1

  ann_avg_ret_bench <- avg_ret_bench * freq
  ann_avg_ret_port <- avg_ret_port * freq
  ann_per_ret_bench <- (1 + total_ret_bench)^(1/n_y) - 1
  ann_per_ret_port <- (1 + total_ret_port)^(1/n_y) - 1
  ann_vol_bench <- vols_bench * sqrt(freq)
  ann_vol_port <- vols_port * sqrt(freq)
  sharpe_bench <- avg_ret_bench/vols_bench
  sharpe_port <- avg_ret_port/vols_port

  mets_abs_bench <- data.frame(cbind(c("Retorno Promedio Benchmark", "Retorno Prom. Anual. Benchmark",
                                 "Retorno Total Benchmark", "Retorno Total Anual. Benchmark", "Volatilidad Benchmark",
                                 "Sharpe Benchmark"), round(rbind(factor_val * avg_ret_bench, factor_val *
                                                          ann_avg_ret_bench, factor_val * total_ret_bench, factor_val * ann_per_ret_bench,
                                                        factor_val * ann_vol_bench, sharpe_bench), 3)))
  mets_abs_port <- data.frame(cbind(c("Retorno Promedio Port.", "Retorno Prom. Anual. Port.",
                                       "Retorno Total Port.", "Retorno Total Anual. Port.", "Volatilidad Port.",
                                       "Sharpe Port."), round(rbind(factor_val * avg_ret_port, factor_val *
                                                                          ann_avg_ret_port, factor_val * total_ret_port, factor_val * ann_per_ret_port,
                                                                        factor_val * ann_vol_port, sharpe_port), 3)))
  colnames(mets_abs_bench) <- c("Variable", colnames(rets_bench))
  colnames(mets_abs_port) <- c("Variable", colnames(rets_port))

  # Returns by Horizont
  curr_date <- tail(index(port_series),1)
  ini_date <- index(port_series)[1]

  ref_dates_all <- c(input$custom_ini_date, max(dmy(paste("0101", year(curr_date))),ini_date), curr_date - 30, curr_date - 90, curr_date - 180, curr_date - 365*c(1,3,5,7,10))
  ref_dates_valid <- ref_dates_all >= ini_date
  ref_dates <- ref_dates_all[ref_dates_valid]
  if(length(ref_dates)>1){
    ref_dates[2] <- toString(as.Date(ref_dates[2]) - 1)
  }

  pos_dates_b <- sapply(findInterval(ref_dates, index(bench_series)), max, 1)
  pos_dates_p <- sapply(findInterval(ref_dates, index(port_series)), max, 1)

  if (dim(bench_series)[2]==1){
    ref_rets_bench <- apply(coredata(bench_series)[pos_dates_b,,drop=FALSE],1, function(x) as.vector(tail(bench_series,1))/x)-1
    dim(ref_rets_bench) <- c(length(ref_rets_bench),1)
    colnames(ref_rets_bench) <- colnames(bench_series)
  }else{
    ref_rets_bench <- t(apply(coredata(bench_series)[pos_dates_b,,drop=FALSE],1, function(x) as.vector(tail(bench_series,1))/x))-1
  }

  if (dim(port_series)[2]==1){
    ref_rets_port <- apply(coredata(port_series)[pos_dates_p,,drop=FALSE],1, function(x) as.vector(tail(port_series,1))/x)-1
    dim(ref_rets_port) <- c(length(ref_rets_port),1)
    colnames(ref_rets_port) <- colnames(port_series)
  }else{
    ref_rets_port <- t(apply(coredata(port_series)[pos_dates_p,,drop=FALSE],1, function(x) as.vector(tail(port_series,1))/x))-1
  }

  per_valid <- c("Fecha inicio", "YTD", "1M", "3M", "6M", "1Y", "3Y", "5Y", "7Y", "10Y")[ref_dates_valid]
  lab_dates <- paste(per_valid, paste0("(", ref_dates,"/", curr_date, ")"))
  #Benchmark summary:
  bench_summary <- round(100*t(ref_rets_bench), 2)
  colnames(bench_summary) <- lab_dates
  rownames(bench_summary) <- colnames(bench_series)
  #Port summary:
  port_summary <- round(100*t(ref_rets_port), 2)
  colnames(port_summary) <- lab_dates
  rownames(port_summary) <- colnames(port_series)

  # Exceded return
  exc_rets <- port_summary - bench_summary

  ## Relative Metrics
  alpha_dist <- rets_port - rets_bench
  te <- apply(alpha_dist, 2, sd)
  active_ret <- avg_ret_port - avg_ret_bench
  ann_active_ret <- active_ret * freq
  ann_te <- te * sqrt(freq)
  info_ratio <- round(ann_active_ret/ann_te, 3)
  port_names <- names(avg_ret_port)
  mets_rel <- data.frame(Portafolio = rep(port_names,3),Variable =c(rep("Alpha",length(port_names)),
                        rep("Tracking Error Anual.",length(port_names)), rep("IR",length(port_names))),
                        Valor = c(factor_val * ann_active_ret, factor_val * ann_te, info_ratio))

  return(list(rets_bench = rets_bench, rets_port = rets_port, total_per = n_y, mets_abs_bench = mets_abs_bench,
              mets_abs_port = mets_abs_port, horz_per = per_valid, bench_summary = bench_summary, port_summary = port_summary,
              bench_summ_names = colnames(bench_series), port_summ_names = colnames(port_series), mets_rel = mets_rel))
}
