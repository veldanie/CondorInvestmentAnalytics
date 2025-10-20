
#' Rebuild series
#'
#' Builds history for a series given forecasted returns (for the past).
#' @param series_rebuild forecasted historical returns. Csv file in shinyApp/data/
#' @param series_list SuraInvestmentAnalytics' object
#' @param ticker ticker
#' @return rebuilt series

rebuild_series <- function(series_rebuild, real_series, ticker = NULL, search_in_rebuild = FALSE, search_in_real = FALSE){
  if(search_in_rebuild){
    forecasted_series <- xts(x = as.numeric(series_rebuild[, which(colnames(series_rebuild) == ticker) + 1])[!(is.na(as.numeric(series_rebuild[, which(colnames(series_rebuild) == ticker) + 1])))],
                             order.by = as.Date(series_rebuild[, which(colnames(series_rebuild) == ticker)][!(is.na(as.Date(series_rebuild[, which(colnames(series_rebuild) == ticker)])))],
                                                format = '%d/%m/%Y'))
  }
  else{
    forecasted_series <- series_rebuild
  }
  if(search_in_real){
    real_series <- xts(x = as.numeric(real_series[, which(colnames(real_series) == ticker) + 1])[!(is.na(as.numeric(real_series[, which(colnames(real_series) == ticker) + 1])))],
                       order.by = as.Date(real_series[, which(colnames(real_series) == ticker)][!(is.na(as.Date(real_series[, which(colnames(real_series) == ticker)])))],
                                          format = '%d/%m/%Y'))
  }
  real_series <- returns(real_series, type = 'arithmetic')[toString(paste0(tail(index(forecasted_series), 1), '/')), ]
  real_series <- real_series[!(index(real_series) %in% index(forecasted_series)), ]
  joined_series <- rbind(forecasted_series, real_series)
  output_series <- xts(x = tail(cumprod(c(100, 1 + joined_series)), -1), order.by = index(joined_series))
  return(output_series)
}
