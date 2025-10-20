
forecast_volatility <- function(return_series, n_ahead=12, per = 'monthly', quant = 0.95, penalty = 'Akaike'){

  freq <- switch(per, "daily" = 252, "monthly" = 12, "quarterly" = 4, "semianualy" = 2)

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

  models <- c(garchfit1, garchfit2, garchfit3, garchfit4, garchfit5, garchfit6, garchfit7,
              garchfit8)
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

    # Calculate conditional variance and rolling standard deviation of returns
    # conditional_variance <- sigma(best_model[[1]])
    # colnames(conditional_variance) <- c('Conditional variance')
    # conditional_variance$std_dev <- apply.rolling(return_series, n_ahead, trim = FALSE, FUN = 'sd')
    historical_vol <- apply.rolling(return_series, n_ahead, trim = TRUE, FUN = 'sd')
    ub_historical_vol <- round(quantile(na.omit(historical_vol), probs = (quant + ((1 - quant) / 2))) * sqrt(freq) * 100, 2)
    lb_historical_vol <- round(quantile(na.omit(historical_vol), probs = ((1 - quant) / 2)) * sqrt(freq) * 100, 2)
    vol_forecast <- round(mean(sigma(garchforecast)) * sqrt(freq) * 100, 2)
    return(list(forecast = vol_forecast, ub = ub_historical_vol, lb = lb_historical_vol,
                model = best_model[[1]]))
  }
}
