
#' Performance attribution df.
#'
#' Performance attribution df.
#' @param w_port Portfolio weights.
#' @param w_bench Benchmark weights.
#' @param efec_ret_port Portfolio realized returns.
#' @param efec_ret_bench Benchmark realized returns.
#' @param cash_port Portfolio realized return.
#' @param cash_bench Benchmark realized return.
#' @param diff_cash_assets_port Diff cash assets port.
#' @param diff_cash_assets_bench Diff cash assets bench.
#' @param weights_port Weights port.
#' @param weights_bench Weights bench.
#' @param dec_dates_port Dec. dates port.
#' @param dec_dates_bench Dec. dates bench.
#' @param header_df Header.

#' @return Performance attribution df.
#' @export

total_return_attribution <- function(w_port, w_bench, efec_ret_port, efec_ret_bench, cash_port, cash_bench, diff_cash_assets_port, diff_cash_assets_bench, weights_port, weights_bench, dec_dates_port = NA, dec_dates_bench = NA, header_df = c("Portafolio", "Benchmark", "AA", "SS", "INT", "TOTAL")) {
  asset_names <- unique(c(names(w_bench), names(w_port)))
  w1 <- w2 <- rep(0, length(asset_names))
  names(w1) <- names(w2) <- asset_names
  w1[names(w_bench)] <- w_bench
  w2[names(w_port)] <- w_port
  #ra1 <- ra2 <- rep(0, length(asset_names))
  #names(ra1) <- names(ra2) <- asset_names
  #ra1[names(efec_ret_assets_bench)] <- efec_ret_assets_bench
  #ra2[names(efec_ret_assets_port)] <- efec_ret_assets_port

  diff_assets1 <- setdiff(names(w_port), names(w_bench))
  diff_assets2 <- setdiff(names(w_bench), names(w_port))

  port_date_ini <- index(cash_bench)[1]
  port_date_last <- tail(index(cash_bench),1)
  ref_dates <- sort(unique(c(port_date_ini, port_date_last, dec_dates_bench, dec_dates_port)))
  ref_dates <- ref_dates[ref_dates >= port_date_ini & ref_dates <= port_date_last]
  n_dates <- length(ref_dates)

  weights_bench <- weights_bench[ref_dates[-n_dates]]
  weights_port <- weights_port[ref_dates[-n_dates]]


  #cash_assets_bench <- cash_assets_bench[ref_dates]
  #cash_assets_port <- cash_assets_port[ref_dates]
  #cash_bench <- cash_bench[ref_dates]
  rets1 <- returns(cash_bench[ref_dates])

  rets_assets1 <- xts(matrix(0, nrow = n_dates-1, ncol = ncol(diff_cash_assets_bench)), order.by = ref_dates[-1])
  rets_assets2 <- xts(matrix(0, nrow = n_dates-1, ncol = ncol(diff_cash_assets_port)), order.by = ref_dates[-1])
  colnames(rets_assets1) <- colnames(diff_cash_assets_bench)
  colnames(rets_assets2) <- colnames(diff_cash_assets_port)

  for(i in 1:(n_dates-1)){
    valid_dates1 <- index(diff_cash_assets_bench) > ref_dates[i] & index(diff_cash_assets_bench) <= ref_dates[i + 1]
    valid_dates2 <- index(diff_cash_assets_port) > ref_dates[i] & index(diff_cash_assets_port) <= ref_dates[i + 1]
    rets_assets1[i,] <- colSums(diff_cash_assets_bench[valid_dates1])/(as.numeric(cash_bench[ref_dates[i]]) * weights_bench[ref_dates[i]])
    rets_assets2[i,] <- colSums(diff_cash_assets_port[valid_dates2])/(as.numeric(cash_port[ref_dates[i]]) * weights_port[ref_dates[i]])
  }
  rets_assets1[is.nan(rets_assets1)] <- 0
  rets_assets2[is.nan(rets_assets2)] <- 0

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

  aa <- apply(coredata(weights_port[, asset_names] - weights_bench[, asset_names]) * coredata(rets_assets1[, asset_names] - rets1 %*% t(rep(1,length(asset_names)))),2, function(x) prod(1+x)) - 1
  ss <- apply(coredata(weights_bench[, asset_names]) * coredata(rets_assets2[, asset_names] - rets_assets1[, asset_names]), 2, function(x) prod(1+x)) - 1
  inter <- apply(coredata(weights_port[, asset_names] - weights_bench[, asset_names]) * coredata(rets_assets2[, asset_names] - rets_assets1[, asset_names]), 2, function(x) prod(1+x)) - 1
  #total_asset_ret <- ra2[asset_names] - ra1[asset_names]
  #inter <- total_asset_ret - aa - ss

  total <- aa + ss + inter
  summ_df <- data.frame(round(100*cbind(w2, w1, aa, ss, inter, total), 3))
  colnames(summ_df) <- header_df
  rownames(summ_df) <- asset_names
  return(summ_df)
}
