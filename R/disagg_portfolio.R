
#' Dissagregate portfolio
#'
#' Dissagregates portfolio with funds into assets.
#' @param w_i weights vector.
#' @param benchmarks DF benchmarks.
#' @param url_database url request to query_database lambda
#' @param url_token url request token
#' @param username_req Username who needs token. 
#' @param password_req Password
#' @param filter_char Only disaggregate assets that contain the this characters
#' @return Vector of assets weights.
#' @export

disagg_portfolio <- function(w_i, benchmarks, url_database,
                             url_token, username_req, password_req, filter_char=NULL){
  #conn <- poolCheckout(db)
  #DBI::dbBegin(conn)
  ind_fund_db <- names(w_i) %in% as.data.frame(query_database(url_database, "SELECT DISTINCT PortId FROM Weights", url_token, user_condor, pass_condor))$PortId 
  ind_fund <- names(w_i) %in% benchmarks$Benchmark
  
  if (!is.null(filter_char)){
    ind_fund_db <- ind_fund_db & grepl(filter_char, names(w_i))
    ind_fund <- ind_fund & grepl(filter_char, names(w_i))
  }
  w_funds <- c()
  if(any(ind_fund_db)){
    for(fi in names(w_i[ind_fund_db])){
      w_df_db <- as.data.frame(query_database(url_database, sprintf("SELECT * FROM Weights WHERE PortId ='%s'", fi), url_token, user_condor, pass_condor) %>% dplyr::select(Asset, Weight, DatePort))
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
  #DBI::dbCommit(conn)
  #poolReturn(conn)
  w_i <- tapply(c(w_i[!ind_fund & !ind_fund_db], w_funds), names(c(w_i[!ind_fund & !ind_fund_db], w_funds)), sum)
  return(w_i)
}
