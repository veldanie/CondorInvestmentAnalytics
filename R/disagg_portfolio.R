
#' Dissagregate portfolio
#'
#' Dissagregates portfolio with funds into assets.
#' @param w_i weights vector.
#' @param benchmarks DF benchmarks.
#' @param conn database connection
#' @return Vector of assets weights.
#' @export

disagg_portfolio <- function(w_i, benchmarks, conn){
  ind_fund_db <- names(w_i) %in% as.data.frame(conn %>% tbl("Weights"))$PortId
  ind_fund <- names(w_i) %in% benchmarks$Benchmark
  w_funds <- c()
  if(any(ind_fund_db)){
    for(fi in names(w_i[ind_fund_db])){
      w_df_db <- as.data.frame(conn %>% tbl("Weights") %>% filter(PortId == fi) %>% dplyr::select(Asset, Weight, DatePort))
      w_df_db <- w_df_db[w_df_db$DatePort == max(w_df_db$DatePort), c("Asset", "Weight")]
      w_fund_db <- w_i[fi] * as.numeric(w_df_db$Weight)
      names(w_fund_db) <- w_df_db$Asset
      w_funds <- c(w_funds, w_fund_db)
    }
  }
  if(any(ind_fund)){
    for (fi in names(w_i[ind_fund])){
      w_df <- benchmarks %>% filter(Benchmark==fi) %>% dplyr::select(Asset, Weight)
      w_fund <- w_i[fi] * as.numeric(w_df$Weight)
      names(w_fund) <- w_df$Asset
      w_funds <- c(w_funds, w_fund)
    }
  }
  w_i <- tapply(c(w_i[!ind_fund], w_funds), names(c(w_i[!ind_fund], w_funds)), sum)
}
