#' Max Drawdown Return
#'
#' Max Drawdown.
#' @param series xts series.
#' @export

max_drawdown <- function(series){
  series_cummax <- cummax(series)
  dd_rets <- series/series_cummax - 1
  dd <- min(dd_rets)
  pos_end <- which.min(dd_rets)
  pos_ini <- which.max(series_cummax[1:pos_end])
  date_ini=index(series)[pos_ini]
  date_last=index(series)[pos_end]
  dd_t <- round(as.numeric(date_last - date_ini)/30, 2)
  return(list(dd=abs(dd), date_ini=date_ini, date_last=date_last, dd_t=dd_t))
}
