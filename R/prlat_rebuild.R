
prlat_rebuild <- function(ticker, series_list, reported_returns, since_date){
  
  series <- series_list[[ticker]][paste0(since_date, '/')]
  reported_returns <- xts(x = na.omit(reported_returns[, which(colnames(reported_returns) == ticker) + 1]), 
                          order.by = na.omit(as.Date(reported_returns[, which(colnames(reported_returns) == ticker)], format = "%d/%m/%Y")))
  series_returns <- returns(series)
  
  for(i in 2:length(index(reported_returns))){
    n_values <- length(series_returns[paste(toString(index(reported_returns[i - 1])), toString(index(reported_returns[i])), sep = '/')])
    adjust_factor <- ((as.vector(series[index(reported_returns)[i - 1]]) * (1 + coredata(reported_returns)[i]) / as.vector(series[index(reported_returns)[i]]))) ^ (1 / n_values) - 1
    series_returns[paste(toString(index(reported_returns[i - 1])), toString(index(reported_returns[i])), sep = '/')] <- series_returns[paste(toString(index(reported_returns[i - 1])), toString(index(reported_returns[i])), sep = '/')]  + adjust_factor
  }
  
  rebuilt_series <- xts(x = as.vector(series[1]) * cumprod(1 + c(0, coredata(series_returns))),
                     order.by = index(series))
  output <- rbind(series_list[[ticker]][paste0('/', since_date)], rebuilt_series)
  return(output)
  
}
