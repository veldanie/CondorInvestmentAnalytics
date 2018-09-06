#' Returns
#'
#' Estimates returns with a given period.
#' @param series object of state prices, or an OHLC type object.
#' @param dates Initial and end date.
#' @param period returns period. c(‘daily’, ‘weekly’, ‘monthly’, ‘quarterly’, ‘yearly’)
#' @param type type of returns: arithmetic (discrete) or log (continuous)
#' @return Returns
#' @export

returns <- function(series, dates = NULL, period = 'daily', type = 'arithmetic') {
  if(!is.null(dates)){
    series <- series[paste(dates, collapse = '/')]
  }
  nvar <- ncol(series)
  rets <- periodReturn(series[,1], period = period, type = type, leading  = FALSE)
  if(nvar > 1){
    for (i in 2:nvar){
      rets  <- merge.xts(rets, periodReturn(series[,i], period = period, type = type, leading = FALSE))
    }
  }
  rets <- rets[!apply(is.na(rets), 1, any)]
  names(rets) <- names(series)
  return(rets)
}
