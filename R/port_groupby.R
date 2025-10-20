
#' Portfolio vector groupby
#'
#' Aggregated portfolio
#' @param port Portfolio vector with names.
#' @param asset_data Assets DF.
#' @param factor_id Group variable.
#' @return Grouped portfolio vector.
#' @export


port_groupby <- function(port, asset_data, factor_id='AssetClass'){
  port_df_temp <- data.frame(port)
  port_agg <- t(rowsum(port_df_temp, asset_data[[factor_id]][match(names(port), asset_data$Asset)]))[1,]
  return(port_agg)
}
