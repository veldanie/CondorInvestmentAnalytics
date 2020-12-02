
#' complete_series_forward

#' Checks if series is complete and if not, output will be the completed series with another one
#' @param complete_series Series that has complete data
#' @param series_to_complete Series to be completed
#' @return xts object
#' @export

complete_series_forward <- function(complete_series, series_to_complete){
  if(index(complete_series)[length(index(complete_series))] > index(series_to_complete)[length(series_to_complete)]){
    returns_complete_series <- returns(complete_series, period = 'daily')
    part_missing <- returns_complete_series[!(index(returns_complete_series) %in% index(series_to_complete))]
    rebuild <- coredata(series_to_complete)[length(coredata(series_to_complete))] * cumprod(1 + c(0 , part_missing))
    to_concat <- xts(x = rebuild[2:length(rebuild)], order.by = index(part_missing))
    output <- rbind(series_to_complete, to_concat)
    return(output)
  }else{
    return(series_to_complete)
  }
}
