

complete_all_dates <- function(ticker, series_list){
    first_date <- index(series_list[[ticker]][1])
    last_date <- index(series_list[[ticker]][length(index(series_list[[ticker]]))])
    date_range <- seq(as.Date(first_date), by = "day", length.out = as.numeric(as.Date(last_date) - as.Date(first_date)))
    temp_xts <- xts(x = rep(NA, length(date_range)), order.by = date_range)
    series_out <- na.locf(merge.xts(temp_xts, series_list[[ticker]]))[, 2]
    colnames(series_out) <- NULL
    return(series_out)
}
