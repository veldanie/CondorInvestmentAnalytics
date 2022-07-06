
#' complete_series_forward

#' Checks if series is complete and if not, output will be the completed series with another one
#' @param complete_series Series that has complete data
#' @param series_to_complete Series to be completed
#' @param estimate_coef Estimata regression coef. to predict
#' @param constant Forward fill with last value
#' @return xts object
#' @export


complete_series_forward <- function(complete_series, series_to_complete, estimate_coef=FALSE, constant=FALSE){
  if(constant){
    ref_dates <- index(complete_series)[index(complete_series)>tail(index(series_to_complete),1)]
    if(length(ref_dates)>5){
      series_to_complete <- rbind(series_to_complete, xts(rep(as.numeric(tail(series_to_complete,1)), length(ref_dates)), order.by = ref_dates))
    }
    return(series_to_complete)
  }else{
    if(index(complete_series)[length(index(complete_series))] > index(series_to_complete)[length(series_to_complete)]){
      returns_complete_series <- returns(complete_series, period = 'daily')
      l_ser <- length(series_to_complete)
      part_missing <- returns_complete_series[!(index(returns_complete_series) %in% index(series_to_complete)) & (index(returns_complete_series)>index(series_to_complete)[l_ser])]
      if(estimate_coef){
        returns_series_to_complete <- returns(series_to_complete, period = 'daily')
        reg_model <- lm(returns_series_to_complete~returns_complete_series - 1, data=merge.xts(returns_series_to_complete, returns_complete_series, join='inner'))
        new_vals <- predict(reg_model, data.frame(returns_complete_series=coredata(part_missing)))
        rebuild <- coredata(series_to_complete)[l_ser] * cumprod(1 + c(0 , new_vals))
      }else{
        rebuild <- coredata(series_to_complete)[l_ser] * cumprod(1 + c(0 , part_missing))
      }
      to_concat <- xts(x = rebuild[2:length(rebuild)], order.by = index(part_missing))
      output <- rbind(series_to_complete, to_concat)
      return(output)
    }else{
      return(series_to_complete)
    }
  }
}
