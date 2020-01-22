
#' Dissagregate portfolio
#'
#' Dissagregates portfolio with funds into assets.
#' @param w weights vector.
#' @param benchmarks DF benchmarks.
#' @return Vector of assets weights.
#' @export

disagg_portfolio <- function(w, benchmarks) {
  fund_ind <- names(w) %in% benchmarks$Benchmark
  if(any(fund_ind)){
    funds <- names(w)[fund_ind]
    w_funds <- c()
    for(fi in funds){
      w_fund_df <- benchmarks %>% filter(Benchmark==fi) %>% dplyr::select(Asset, Weight)
      w_fund <- w[fi] * as.numeric(w_fund_df$Weight)
      names(w_fund) <- w_fund_df$Asset
      w_funds <- c(w_funds, w_fund)
    }
    w_assets <- tapply(c(w[!fund_ind], w_funds), names(c(w[!fund_ind], w_funds)), sum)

  }else{
    w_assets <- w
  }
  return(w_assets)
}
