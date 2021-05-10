
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
  garchspec9 <- ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                           variance.model = list(model = "eGARCH"),
                           distribution.model = "sstd")
  garchspec10 <- ugarchspec(mean.model = list(armaOrder = c(0, 1)),
                           variance.model = list(model = "eGARCH"),
                           distribution.model = "sstd")
  garchspec11 <- ugarchspec(mean.model = list(armaOrder = c(1, 0)),
                           variance.model = list(model = "eGARCH"),
                           distribution.model = "sstd")
  garchspec12 <- ugarchspec(mean.model = list(armaOrder = c(1, 1)),
                           variance.model = list(model = "eGARCH"),
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
  garchfit9 <- ugarchfit(data=return_series, spec=garchspec9)
  garchfit10 <- ugarchfit(data=return_series, spec=garchspec10)
  garchfit11 <- ugarchfit(data=return_series, spec=garchspec11)
  garchfit12 <- ugarchfit(data=return_series, spec=garchspec12)

  models <- c(garchfit1, garchfit2, garchfit3, garchfit4, garchfit5, garchfit6, garchfit7,
              garchfit8, garchfit9, garchfit10, garchfit11, garchfit12)
  converged <- sapply(models, convergence)
  converged_models <- models[converged == 0]

  if(sum(converged) == length(converged)){
    print("No hubo convergencia")
  }else{
    # Calculate information criteria to select best model
    information_criteria <- lapply(converged_models, infocriteria)
    criteria_pos <- which(names(information_criteria[[1]][, 1]) == penalty)
    values <- sapply(information_criteria, '[', criteria_pos)
    best_model <- converged_models[match(min(values), values)]

    # Instantiate forecast class
    garchforecast <- ugarchforecast(fitORspec = best_model[[1]], n.ahead = n_ahead)

    # Residuals and Ljung-Box test
    res <- residuals(best_model[[1]], standardize = TRUE)
    box_result <- Box.test(abs(res), n_ahead, type = 'Ljung-Box')
    print(box_result)

    # Calculate conditional variance and rolling standard deviation of returns
    conditional_variance <- sigma(best_model[[1]])
    colnames(conditional_variance) <- c('Conditional variance')
    conditional_variance$std_dev <- apply.rolling(return_series, n_ahead, trim = FALSE, FUN = 'sd')
    return(list(forecast = garchforecast, conditional_variance = na.omit(conditional_variance)))
  }
}
#
# library(SuraInvestmentAnalytics)
# library(xts)
# library(quantmod)
# library(lubridate)
#
# indices <- c('SPX Index', 'LG38TRUU Index', 'GCOMMSURA Index', 'IRTCOMP Index', 'XAU')
# for(index in indices){
#   print(index)
#   test_rets <- returns(series_list[[index]], period = 'monthly')
#   vol_results <- forecast_volatility(test_rets)
#   x <- coredata(vol_results$conditional_variance$Conditional.variance)
#   y <- coredata(vol_results$conditional_variance$std_dev)
#   plot(x, y, main = index,
#        xlab = "Conditional variance", ylab = "Standard deviation",
#        pch = 19, frame = FALSE)
#   abline(lm(y ~ x, data = mtcars), col = "blue")
# }


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

