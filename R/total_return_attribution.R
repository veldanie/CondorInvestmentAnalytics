
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

total_return_attribution <- function(w_port, w_bench, efec_ret_assets_port, efec_ret_assets_bench, efec_ret_port, efec_ret_bench, header_df = c("Portafolio", "Benchmark", "AA", "SS", "INT", "TOTAL")) {
  asset_names <- unique(c(names(w_bench), names(w_port)))
  w1 <- w_bench[asset_names]
  w2 <- w_port[asset_names]
  ra1 <- efec_ret_assets_bench[asset_names]
  ra2 <- efec_ret_assets_port[asset_names]

  ret1 <- ra1/w1
  ret2 <- ra2/w2
  names(ret1) <- names(ret2) <- asset_names
  ret1[is.na(ret1)] <- ret2[names(ret1)[is.na(ret1)]]
  ret2[is.na(ret2)] <- ret1[names(ret2)[is.na(ret2)]]
  w1[is.na(w1)] <- 0
  w2[is.na(w2)] <- 0
  aa <- (w2 - w1) * (ret1 - efec_ret_bench)
  ss <- w1 * (ret2 - ret1)
  inter <- (w2 - w1) * (ret2 - ret1)

  aa[is.na(aa)] <- 0
  ss[is.na(ss)] <- 0
  inter[is.na(inter)] <- 0
  total <- aa + ss + inter
  summ_df <- data.frame(round(100*cbind(w2, w1, aa, ss, inter, total), 3))
  colnames(summ_df) <- header_df
  rownames(summ_df) <- asset_names
  return(summ_df)
}
