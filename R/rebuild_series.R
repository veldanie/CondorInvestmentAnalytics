
#' Rebuild series
#'
#' Builds history for a series given forecasted returns (for the past). 
#' @param series_rebuild forecasted historical returns. Csv file in shinyApp/data/
#' @param series_list SuraInvestmentAnalytics' object
#' @param ticker ticker
#' @return rebuilt series

rebuild_series <- function(series_rebuild, series_list, ticker){
  real_series <- series_list[[ticker]]
  forecasted_series <- xts(x = series_rebuild[, which(colnames(series_rebuild) == ticker) + 1],
                           order.by = as.Date(series_rebuild[, which(colnames(series_rebuild) == ticker)], 
                                              format = '%d/%m/%Y'))
  real_series <- returns(real_series, type = 'arithmetic')[toString(paste0(tail(index(forecasted_series), 1), '/')), ]
  real_series <- real_series[!(index(real_series) %in% index(forecasted_series)), ]
  joined_series <- rbind(forecasted_series, real_series)
  output_series <- xts(x = tail(cumprod(c(100, 1 + joined_series)), -1), order.by = index(joined_series))
  return(output_series)
}
