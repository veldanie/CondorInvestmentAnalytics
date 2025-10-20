
#' DF vector groupby
#'
#' Aggregated DataFrame
#' @param port_df Profolio DF
#' @param asset_data Assets DF.
#' @param factor_id Group variable.
#' @return Grouped portfolio vector.
#' @export

port_df_groupby <- function(port_df, asset_data, factor_id='AssetClass'){
  asset_names <- rownames(port_df)
  factor_data <- asset_data[match(asset_names, asset_data$Asset),] %>% dplyr::select(Asset, factor_id)
  factor_names <- unique(factor_data %>% pull(factor_id))
  n_factors <- length(factor_names)
  summ_factor_df <- matrix(0, nrow = n_factors, ncol = ncol(port_df))
  for (i in 1:n_factors) {
    ind_assets <- factor_data[, 2] == factor_names[i]
    summ_factor_df[i, ] <- colSums(port_df[ind_assets,])
  }
  colnames(summ_factor_df) <- colnames(port_df)
  rownames(summ_factor_df) <- factor_names
  return(as.data.frame(summ_factor_df))
}
