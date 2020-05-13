
#' Outlier detection

#' Estimates the upper/lower bounds of the differences of a series 
#' @param series Asset series
#' @param selected_bound Bound that the function will return
#' @return selected_limit value
#' @method method used for the calculation
#' @export

detect_outliers_limit <- function(series, selected_limit, method = 'outlier_detection', multiplier_tukey = 1.5) {
  library(OutlierDetection)
  diff_series <- as.vector(diff(series))
  diff_series <- diff_series[!(is.na(diff_series))]
  if(method == 'tukey') {
    quantiles_diff <- quantile(diff_series)
    iqr <- as.vector(quantiles_diff["75%"]) - as.vector(quantiles_diff["25%"])
    lower_limit <- quantiles_diff['25%'] - multiplier_tukey * iqr
    upper_limit <- quantiles_diff['75%'] + multiplier_tukey * iqr
    if (selected_limit == 'upper') {
      return(as.vector(upper_limit))
    } else {
      return(as.vector(lower_limit))
    }
  }
  else if(method == 'outlier_detection') {
    outlier_vector <- OutlierDetection(data.frame(diff_series))$'Outlier Observations'
    if(selected_limit == 'upper') {
      outlier_vector <- outlier_vector[outlier_vector >= 0]
      return(as.vector(min(outlier_vector)))
    } else {
      outlier_vector <- outlier_vector[outlier_vector < 0]
      return(as.vector(max(outlier_vector)))
    }
  }
  else if(method == 'univariate_outlier_detection') {
    outlier_vector <- UnivariateOutlierDetection(diff_series)$'Outlier Observations'
    if(selected_limit == 'upper') {
      outlier_vector <- outlier_vector[outlier_vector >= 0]
      return(as.vector(min(outlier_vector)))
    } else {
      outlier_vector <- outlier_vector[outlier_vector < 0]
      return(as.vector(max(outlier_vector)))
    }
  }
}