

calc_risk_ret_metrics <- function(bench_series, port_series, per = 'monthly', years_horizon = c('1Y', '3Y', '5Y')){
  freq <- switch(per, 'daily' = 252, 'monthly' = 12, 'quarterly' = 4, 'semiannualy' = 2)
  rets_port1 <- returns(bench_series, period = per, leading=FALSE)
  rets_port2 <- returns(port_series, period = per, leading=FALSE)

  output <- vector(mode = "list", length = length(years_horizon))
  names(output) <- years_horizon

  for(i in 1:length(output)){
    date_vector <- str_split(as.Date(tail(index(rets_port1), 1)), '-')[[1]]
    date_vector[1] <- as.character(as.numeric(format(as.Date(tail(index(rets_port1), 1)), "%Y")) - as.numeric(gsub("Y", "", years_horizon[i])))
    slice_since <- paste(date_vector, sep = '-', collapse = '-')
    rets_port1_temp <- rets_port1[paste0(slice_since, '/')]
    rets_port2_temp <- rets_port2[paste0(slice_since, '/')]

    v_cuota_bseries <- xts(x = 1000 * cumprod(1 + c(0, returns(bench_series)[paste0(slice_since, '/')])), order.by = index(bench_series[paste0(slice_since, '/')]))
    v_cuota_pseries <- xts(x = 1000 * cumprod(1 + c(0, returns(port_series)[paste0(slice_since, '/')])), order.by = index(port_series[paste0(slice_since, '/')]))
    v_cuota <- merge.xts(v_cuota_bseries, v_cuota_pseries)
    colnames(v_cuota) <- c("Bench series", "Port series")
    output[[i]]['Valor cuota'] <- list(v_cuota)

    avg_port1 <- mean(rets_port1_temp)
    avg_port2 <- mean(rets_port2_temp)
    vol_port1 <- sd(rets_port1_temp)
    vol_port2 <- sd(rets_port2_temp)
    output[[i]]['Retorno promedio anualizado benchmark'] <- round(avg_port1*freq*100,3)
    output[[i]]['Retorno promedio anualizado portafolio'] <- round(avg_port2*freq*100,3)
    output[[i]]['Volatilidad anualizada benchmark'] <- round(vol_port1*sqrt(freq)*100,3)
    output[[i]]['Volatilidad anualizada portafolio'] <- round(vol_port2*sqrt(freq)*100,3)
    output[[i]]['Sharpe ratio benchmark'] <- round(avg_port1/vol_port1,3)
    output[[i]]['Sharpe ratio portafolio'] <- round(avg_port2/vol_port2,3)
    te <- sd(rets_port2_temp - rets_port1_temp)
    active_ret <- avg_port2 - avg_port1
    output[[i]]['Alpha anualizado'] <- round(100*active_ret*freq, 3)
    output[[i]]['Tracking error anualizado'] <- round(te*sqrt(freq)*100,3)
    output[[i]]['Information ratio anualizado'] <- round(output[[i]][['Alpha anualizado']]/output[[i]][['Tracking error anualizado']],3)
    output[[i]]['Retorno maximo benchmark'] <- round(max(rets_port1_temp) * 100, 3)
    output[[i]]['Retorno minimo benchmark'] <- round(min(rets_port1_temp) * 100, 3)
    output[[i]]['Retorno maximo portafolio'] <- round(max(rets_port2_temp) * 100, 3)
    output[[i]]['Retorno minimo portafolio'] <- round(min(rets_port2_temp) * 100, 3)
  }
  return(output)
}
