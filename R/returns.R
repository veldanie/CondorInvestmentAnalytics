#' Returns
#'
#' Estimates returns with a given period.
#' @param series object of state prices, or an OHLC type object.
#' @param dates Initial and end date.
#' @param period returns period. c(‘daily’, ‘weekly’, ‘monthly’, ‘quarterly’, ‘yearly’)
#' @param type type of returns: arithmetic (discrete) or log (continuous)
#' @param leading Leading return
#' @return Returns
#' @export

returns <- function(series, dates = NULL, period = 'daily', type = 'arithmetic', leading = FALSE) {
  if(!is.null(dates)){
    series <- series[paste(dates, collapse = '/')]
  }
  nvar <- ncol(series)
  rets <- periodReturn(series[,1], period = period, type = type, leading  = leading)
  if(nvar > 1){
    for (i in 2:nvar){
      rets_i <- periodReturn(series[,i], period = period, type = type, leading = leading)
      rets_i[is.na(rets_i) | is.infinite(rets_i)] = 0
      rets  <- merge.xts(rets, rets_i)
    }
  }
  rets <- na.omit(rets)#rets[-1]
  names(rets) <- names(series)
  return(rets)
}
