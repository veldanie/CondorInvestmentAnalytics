
library(xts)
library(zoo)
library(quantmod)
library(SuraInvestmentAnalytics)

index_with_div_yield <- function(ticker, asset_cf, fund_data, shift_values = NULL, as_div_yield = FALSE,
                                 since_date = NULL, series_list = FALSE){
  fund_cf <- xts(x = na.omit(asset_cf[, which(colnames(asset_cf) == ticker) + 1]), 
                 order.by = na.omit(as.Date(asset_cf[, which(colnames(asset_cf) == ticker)], format = "%d/%m/%Y")))
  
  if(!(series_list)){
    fund_data <- xts(x = na.omit(fund_data[, which(colnames(fund_data) == ticker) + 1]), 
                     order.by = na.omit(as.Date(fund_data[, which(colnames(fund_data) == ticker)], format = "%d/%m/%Y")))
  }

  if(!(is.null(since_date))){
    fund_data <- fund_data[paste(toString(as.Date(since_date)), '/')]
  }
  
  if(!(is.null(shift_values))){
    fund_returns <- lag(returns(fund_data), k = -shift_values)
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
      n_values <- delta_div_payments
    }else{
      n_values <- length(index(fund_data)[index(div_yield[i - 1]):index(div_yield[i])])
    }
    div_yield_nom <- convert_rates(div_yield[i], n_values - 1, 'Efectiva', 'Nominal') / n_values
    fund_returns[paste(toString(index(div_yield[i - 1])), toString(index(div_yield[i])), sep = '/')] <- fund_returns[paste(toString(index(div_yield[i - 1])), toString(index(div_yield[i])), sep = '/')] + as.vector(div_yield_nom)
  }
  
  fund_series <- xts(x = as.vector(fund_data[1]) * cumprod(1 + c(0, coredata(fund_returns))),
                     order.by = index(fund_data))
  
  if(!(is.null(shift_values))){
    fund_series <- na.locf(fund_series)
  }
  return(fund_series)
}

asset_cf <- read.csv('D:/Inversiones Internacionales Grupo Sura S.A/Daniel Velasquez Vergara - Proyecto Inception/shinyApp/data/asset_series_cf.csv', check.names = FALSE)
asset_series_fix <- read.csv('D:/Inversiones Internacionales Grupo Sura S.A/Daniel Velasquez Vergara - Proyecto Inception/shinyApp/data/asset_series_fix.csv', check.names = FALSE)

prlat <- index_with_div_yield("FPRLAT PE Equity", asset_cf, series_list[['FPRLAT PE Equity']], as_div_yield = TRUE,
                               since_date = '2018-12-31', series_list = TRUE)
freglob <- index_with_div_yield('FREGLOB PE Equity', asset_cf, asset_series_fix, as_div_yield = FALSE, 
                                series_list = FALSE, shift_values = 9)

freglob2 <- index_series_with_jumps(freglob, series_list[["NPNCRE Index"]], threshold=50, ref_years='2005/2020', complete_method_rets = FALSE)

coredata(freglob2['2020-03-31']) / coredata(freglob2['2019-09-30']) - 1

