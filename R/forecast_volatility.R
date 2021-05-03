
forecast_volatility <- function(return_series, n_ahead=12, penalty = 'Akaike', ljung_box_lag = 12){
  library(rugarch)
  # Define models
  garchspec1 <- ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                           variance.model = list(model = "sGARCH"),
                           distribution.model = "sstd")
  garchspec2 <- ugarchspec(mean.model = list(armaOrder = c(0, 1)),
                           variance.model = list(model = "sGARCH"),
                           distribution.model = "sstd")
  garchspec3 <- ugarchspec(mean.model = list(armaOrder = c(1, 0)),
                           variance.model = list(model = "sGARCH"),
                           distribution.model = "sstd")
  garchspec4 <- ugarchspec(mean.model = list(armaOrder = c(1, 1)),
                           variance.model = list(model = "sGARCH"),
                           distribution.model = "sstd")
  garchspec5 <- ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                           variance.model = list(model = "gjrGARCH"),
                           distribution.model = "sstd")
  garchspec6 <- ugarchspec(mean.model = list(armaOrder = c(0, 1)),
                           variance.model = list(model = "gjrGARCH"),
                           distribution.model = "sstd")
  garchspec7 <- ugarchspec(mean.model = list(armaOrder = c(1, 0)),
                           variance.model = list(model = "gjrGARCH"),
                           distribution.model = "sstd")
  garchspec8 <- ugarchspec(mean.model = list(armaOrder = c(1, 1)),
                           variance.model = list(model = "gjrGARCH"),
                           distribution.model = "sstd")

  # Fit models
  garchfit1 <- ugarchfit(data=return_series, spec=garchspec1)
  garchfit2 <- ugarchfit(data=return_series, spec=garchspec2)
  garchfit3 <- ugarchfit(data=return_series, spec=garchspec3)
  garchfit4 <- ugarchfit(data=return_series, spec=garchspec4)
  garchfit5 <- ugarchfit(data=return_series, spec=garchspec5)
  garchfit6 <- ugarchfit(data=return_series, spec=garchspec6)
  garchfit7 <- ugarchfit(data=return_series, spec=garchspec7)
  garchfit8 <- ugarchfit(data=return_series, spec=garchspec8)

  # Residuals
  res1 <- residuals(garchfit1, standardize = TRUE)
  res2 <- residuals(garchfit2, standardize = TRUE)
  res3 <- residuals(garchfit3, standardize = TRUE)
  res4 <- residuals(garchfit4, standardize = TRUE)
  res5 <- residuals(garchfit5, standardize = TRUE)
  res6 <- residuals(garchfit6, standardize = TRUE)
  res7 <- residuals(garchfit7, standardize = TRUE)
  res8 <- residuals(garchfit8, standardize = TRUE)

  # Ljung-Box test
  box1 <- Box.test(abs(res1), ljung_box_lag, type = 'Ljung-Box')[3]
  box2 <- Box.test(abs(res2), ljung_box_lag, type = 'Ljung-Box')[3]
  box3 <- Box.test(abs(res3), ljung_box_lag, type = 'Ljung-Box')[3]
  box4 <- Box.test(abs(res4), ljung_box_lag, type = 'Ljung-Box')[3]
  box5 <- Box.test(abs(res5), ljung_box_lag, type = 'Ljung-Box')[3]
  box6 <- Box.test(abs(res6), ljung_box_lag, type = 'Ljung-Box')[3]
  box7 <- Box.test(abs(res7), ljung_box_lag, type = 'Ljung-Box')[3]
  box8 <- Box.test(abs(res8), ljung_box_lag, type = 'Ljung-Box')[3]
  box_results <- c(box1, box2, box3, box4, box5, box6, box7, box8)
  passed_test <- box_results > 0.05
  # Information criteria
  information_criteria <- c(infocriteria(garchfit1)[penalty, ], infocriteria(garchfit2)[penalty, ], infocriteria(garchfit3)[penalty, ],
                            infocriteria(garchfit4)[penalty, ], infocriteria(garchfit5)[penalty, ], infocriteria(garchfit6)[penalty, ],
                            infocriteria(garchfit7)[penalty, ], infocriteria(garchfit8)[penalty, ])
  names(information_criteria) <- c(1:8)
  information_criteria <- information_criteria[passed_test]
  min_pos <- match(min(information_criteria), information_criteria)

  if(min_pos == "1"){
    garchforecast <- ugarchforecast(fitORspec = garchfit1, n.ahead = n_ahead)
  }else if(min_pos == "2"){
    garchforecast <- ugarchforecast(fitORspec = garchfit2, n.ahead = n_ahead)
  }else if(min_pos == "3"){
    garchforecast <- ugarchforecast(fitORspec = garchfit3, n.ahead = n_ahead)
  }else if(min_pos == "4"){
    garchforecast <- ugarchforecast(fitORspec = garchfit4, n.ahead = n_ahead)
  }else if(min_pos == "5"){
    garchforecast <- ugarchforecast(fitORspec = garchfit5, n.ahead = n_ahead)
  }else if(min_pos == "6"){
    garchforecast <- ugarchforecast(fitORspec = garchfit6, n.ahead = n_ahead)
  }else if(min_pos == "7"){
    garchforecast <- ugarchforecast(fitORspec = garchfit7, n.ahead = n_ahead)
  }else if(min_pos == "8"){
    garchforecast <- ugarchforecast(fitORspec = garchfit8, n.ahead = n_ahead)
  }

  sigma_forecasts <- sigma(garchforecast)
  mean_forecasts <- fitted(garchforecast)
  return(list(volatility = mean(sigma_forecasts), mean = mean(mean_forecasts)))
}

# results <- rep(NA, length(colnames(df)) -1)
# observations <- rep(NA, length(colnames(df)) - 1)
# mean_forecasts <- rep(NA, length(colnames(df)) - 1)
# for(i in 2:length(colnames(df))){
#   temp_df <- xts(x = df[, i], order.by = as.Date(df[, 1], tryFormats = c("%d/%m/%Y")))
#   temp_rets <- returns(temp_df, period = 'monthly')
#   func_result <- forecast_volatility(temp_rets['/2018'])
#   results[i-1] <- func_result$volatility
#   mean_forecasts[i-1] <- func_result$mean
#   observations[i-1] <- sd(temp_rets['2019'])
#   print(i)
# }
#
# (results * 100) * sqrt(12)
# (observations * 100) * sqrt(12)

