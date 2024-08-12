
#' Mean vector and Covariance matrix.
#'
#' Takes the risk aversion coefficient, the covariance matrix and the market cap weights.
#' @param pr_simul_orig Simulated prices in original currency.
#' @param asset_df Assets data frame with characteristics.
#' @param ref_curr Reference currency.
#' @param names_series Series names.
#' @param normalize Normalize series.
#' @return Simulated prices in ref currency.
#' @export

pr_simul_trans <- function(pr_simul_orig, asset_df, ref_curr = "COP", names_series, assets, normalize = TRUE){
  n_assets <- length(assets)
  dim_orig <- dim(pr_simul_orig)
  N <- dim_orig[1]
  M <- dim_orig[2]
  pr_simul_curr <- array(0, dim = c(N,M,n_assets))
  for (i in 1:n_assets){
    asset <- assets[i]
    asset_curr <- as.character(asset_df$MONEDA[asset_df$ACTIVO == asset])
    pos_asset <- which(asset == names_series)

    if(asset_curr == ref_curr){
      pr_simul_curr[,,i] <- pr_simul_orig[,,pos_asset]
    }else{
      pos_curr <- which(ref_curr == names_series)
      pr_simul_curr[,,i] <- pr_simul_orig[,,pos_asset] * pr_simul_orig[,,pos_curr]
    }
    if(normalize){
      pr_temp <-  rbind(100, 1+apply(pr_simul_curr[,,i], 2, diff)/pr_simul_curr[-N,,i])
      pr_simul_curr[,,i] <- apply(pr_temp,2,cumprod)
    }
  }
  return(pr_simul_curr)
}
