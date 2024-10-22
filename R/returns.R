
#' Returns
#'
#' Estimates returns with a given period.
#' @param series object of state prices, or an OHLC type object.
#' @param dates Initial and end date.
#' @param period returns period. c(‘daily’, ‘weekly’, ‘monthly’, ‘quarterly’, 'semiannualy', ‘yearly’)
#' @param type type of returns: arithmetic (discrete) or log (continuous)
#' @param leading Leading return
#' @param ref_day Reference day to establish if the first month return is calculated
#' @return Returns
#' @export

returns <- function(series, dates = NULL, period = 'daily', type = 'arithmetic', leading = FALSE, ref_day=5) {
  if(!is.null(dates)){
    series <- series[paste(dates, collapse = '/')]
  }
  if(period != "daily" && day(index(series)[1]) <= ref_day){
    leading <- TRUE
  }
  period_semi <- FALSE
  if(substr(period,1,4)=='semi'){
    period_semi <- TRUE
    leading <- TRUE
    period <- 'quarterly'
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

  rets <- na.omit(rets)
  names(rets) <- names(series)

  if(period_semi){
    rets <- quarterly_to_semiannualy(rets)
  }


  return(rets)
}
