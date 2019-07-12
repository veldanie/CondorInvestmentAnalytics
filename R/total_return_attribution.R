
#' Performance attribution df.
#'
#' Performance attribution df.
#' @param w_port Portfolio weights.
#' @param w_bench Benchmark weights.
#' @param efec_ret_assets_port Portfolio assets realized returns.
#' @param efec_ret_assets_bench Benchmark assets realized returns.
#' @param efec_ret_port Portfolio realized return.
#' @param efec_ret_bench Benchmark realized return.
#' @return Performance attribution df.
#' @export

total_return_attribution <- function(w_port, w_bench, efec_ret_assets_port, efec_ret_assets_bench, efec_ret_port, efec_ret_bench, cash_assets_port, cash_assets_bench, weights_port, weights_bench, dec_dates_port = NA, dec_dates_bench = NA, header_df = c("Portafolio", "Benchmark", "AA", "SS", "INT", "TOTAL")) {
  asset_names <- unique(c(names(w_bench), names(w_port)))
  w1 <- w2 <- ra1 <- ra2 <- rep(0, length(asset_names))
  names(w1) <- names(w2) <- names(ra1) <- names(ra2) <- asset_names
  w1[names(w_bench)] <- w_bench
  w2[names(w_port)] <- w_port
  ra1[names(efec_ret_assets_bench)] <- efec_ret_assets_bench
  ra2[names(efec_ret_assets_port)] <- efec_ret_assets_port

  diff_assets1 <- setdiff(names(w_port), names(w_bench))
  diff_assets2 <- setdiff(names(w_bench), names(w_port))

  ref_dates <- sort(unique(c(dec_dates_bench, dec_dates_port)))
  cash_assets_bench <- cash_assets_bench[ref_dates]
  cash_assets_port <- cash_assets_port[ref_dates]
  rets_assets1 <- returns(cash_assets_bench)
  rets_assets2 <- returns(cash_assets_port)
  weights_bench <- weights_bench[index(rets_assets1)]
  weights_port <- weights_port[index(rets_assets2)]

  if(length(diff_assets1)>0){
    asset_names_temp <- colnames(rets_assets1)
    weights_bench <- merge.xts(weights_bench[,asset_names_temp], xts(matrix(0, ncol = length(diff_assets1), nrow = nrow(weights_bench)), order.by = index(weights_bench)))
    rets_assets1 <- merge.xts(rets_assets1, rets_assets2[, diff_assets1], join = "inner")
    colnames(rets_assets1) <- colnames(weights_bench) <- c(asset_names_temp, diff_assets1)
  }
  if(length(diff_assets2)>0){
    asset_names_temp <- colnames(rets_assets2)
    weights_port <- merge.xts(weights_port[,asset_names_temp], xts(matrix(0, ncol = length(diff_assets2), nrow = nrow(weights_port)), order.by = index(weights_port)))
    rets_assets2 <- merge.xts(rets_assets2, rets_assets1[, diff_assets2], join = "inner")
    colnames(rets_assets2) <- colnames(weights_port) <- c(asset_names_temp, diff_assets2)
  }

  rets1 <- returns(xts(rowSums(cash_assets_bench), order.by = index(cash_assets_bench)))
  aa <- apply((weights_port[, asset_names] - weights_bench[, asset_names]) * (rets_assets1[, asset_names] - rets1 %*% t(rep(1,length(asset_names)))),2, function(x) prod(1+x)) - 1
  ss <- apply(weights_bench[, asset_names] * (rets_assets2[, asset_names] - rets_assets1[, asset_names]), 2, function(x) prod(1+x)) - 1
  #inter <- apply((weights_port[, asset_names] - weights_bench[, asset_names]) * (rets_assets2[, asset_names] - rets_assets1[, asset_names]), 2, function(x) prod(1+x)) - 1
  total_asset_ret <- ra2[asset_names] - ra1[asset_names]
  inter <- total_asset_ret - aa - ss

  total <- aa + ss + inter
  summ_df <- data.frame(round(100*cbind(w2, w1, aa, ss, inter, total), 3))
  colnames(summ_df) <- header_df
  rownames(summ_df) <- asset_names
  return(summ_df)
}
