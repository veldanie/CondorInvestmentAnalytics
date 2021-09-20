

freglob_rebuild <- function(ticker, asset_cf, fund_data, shift_values = NULL, as_div_yield = FALSE,
                                 since_date = NULL, series_list = FALSE){
  fund_cf <- xts(x = na.omit(as.numeric(asset_cf[, which(colnames(asset_cf) == ticker) + 1])),
                 order.by = na.omit(as.Date(asset_cf[, which(colnames(asset_cf) == ticker)], format = "%d/%m/%Y")))

  if(!(series_list)){
    fund_data <- xts(x = na.omit(as.numeric(fund_data[, which(colnames(fund_data) == ticker) + 1])),
                     order.by = na.omit(as.Date(fund_data[, which(colnames(fund_data) == ticker)], format = "%d/%m/%Y")))
  }

  if(!(is.null(since_date))){
    fund_data <- fund_data[paste(toString(as.Date(since_date)), '/')]
  }

  if(!(is.null(shift_values))){
    fund_returns <- xts::lag.xts(returns(fund_data), k = -shift_values)
  }else{
    fund_returns <- returns(fund_data)
  }

  if(as_div_yield){
    div_yield <- fund_cf
  }else{
    div_yield <- fund_cf / fund_data[index(fund_cf), ]
  }

  delta_div_payments <- as.vector(mean(diff(index(div_yield))))
  div_yield <- rbind(div_yield, xts(x = mean(coredata(div_yield)[(length(coredata(div_yield)) - 4):length(coredata(div_yield))]),
                                    order.by = Sys.Date()))

  for(i in 2:length(index(div_yield))){
    if(i == length(index(div_yield))){
      n_values <- index(fund_data)[nrow(fund_data)] - index(div_yield)[i-1]
      adjust_div <- n_values / (index(div_yield)[i] - index(div_yield)[i-1])
      div_yield_nom <- convert_rates(div_yield[i] * adjust_div, n_values - 1, 'Efectiva', 'Nominal') ^ (1/n_values) - 1
    }else{
      n_values <- length(index(fund_data)[index(div_yield[i - 1]):index(div_yield[i])])
      div_yield_nom <- convert_rates(div_yield[i], n_values - 1, 'Efectiva', 'Nominal') ^ (1/n_values) - 1
    }
    fund_returns[paste(toString(index(div_yield[i - 1])), toString(index(div_yield[i])), sep = '/')] <- fund_returns[paste(toString(index(div_yield[i - 1])), toString(index(div_yield[i])), sep = '/')] + as.vector(div_yield_nom)
  }

  fund_series <- xts(x = as.vector(fund_data[1]) * cumprod(1 + c(0, coredata(fund_returns))),
                     order.by = index(fund_data))

  if(!(is.null(shift_values))){
    fund_series <- na.locf(fund_series)
  }
  return(fund_series)
}

