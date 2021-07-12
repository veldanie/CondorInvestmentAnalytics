
#' port_weight_variation
#'
#' Returns real series of portfolio weights
#' @param port_weights portfolio weights
#' @param asset_series asset series
#' @param ini_date initial date
#' @param end_date final date
#' @return xts weights_xts
#' @export

port_weight_variation <- function(port_weights, asset_series, ini_date, end_date){
  output_df <- xts(x = matrix(nrow = length(index(asset_series[paste0(c(ini_date, end_date), collapse = '/')])),
                  ncol = ncol(asset_series)), order.by = index(asset_series[paste0(c(ini_date, end_date), collapse = '/')]))
  colnames(output_df) <- colnames(asset_series)
  output_df[1, ] <- 1000 * port_weights[colnames(asset_series)]
  asset_rets <- returns(asset_series)[paste0(c(ini_date, end_date), collapse = '/')]
  for(i in 1:length(colnames(output_df))){
    output_df[2:nrow(output_df), i] <- as.vector(output_df[1, i]) * cumprod(1 + as.vector(asset_rets[2:nrow(asset_rets), i]))
  }
  output_df / rowSums(output_df)
}

