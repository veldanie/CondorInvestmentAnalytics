
#' Map assest to funds by reading InvestTickers and UserIndex tables.
#'
#' Map assest to funds by reading InvestTickers and UserIndex tables.
#' @param w named weights vector.
#' @param asset_data Assets DF.
#' @param invest_tickers Invest Tickers Table.
#' @param custom_index User Index table.
#' @return invest_assets Invest Assets indicator.
#' @export

portfolio_assets_to_funds <- function(w, asset_data, invest_tickers = NULL, custom_index = NULL, invest_assets = NULL, keep_w=FALSE){

  asset_names <- names(w)
  asset_in_db <- asset_names[asset_names %in% invest_tickers$Asset]
  asset_not_in_db <- setdiff(asset_names, asset_in_db)
  if (length(asset_in_db)>0){
    ref_tk <- invest_tickers$Ticker[match(asset_in_db, invest_tickers$Asset)]
    names(ref_tk) <- asset_in_db
    
    asset_in_ci <- asset_in_db[ref_tk %in% custom_index$Ticker]
    asset_not_in_ci <- setdiff(asset_in_db, asset_in_ci)
    if(length(asset_not_in_ci)>0){
      tk_name_pos <- match(ref_tk[asset_not_in_ci], asset_data$TickerBenchmark)
      names(w)[match(asset_not_in_ci[!is.na(tk_name_pos)], names(w))] <- asset_data$Asset[tk_name_pos[!is.na(tk_name_pos)]]
    }
    if(length(asset_in_ci)>0){
      for (asset_i in asset_in_ci){
        index_df <- custom_index %>% filter(Ticker==ref_tk[asset_i]) %>% dplyr::select(Asset, Weight)    
        if (keep_w){
          new_w <- rep(1, length(index_df$Weight))
        }else{
          new_w <- index_df$Weight
        }
        w_add <- w[asset_i] * new_w
        names(w_add) <- index_df$Asset
        w <- c(w, w_add)
        w <- w[-match(asset_i, names(w))]
      }
    }
  }
  
  if (length(asset_not_in_db)>0){
    asset_pos <- match(asset_not_in_db, asset_data$Asset)
    if (invest_assets=='ETF'){
      ref_tk <- asset_data$TickerETF[asset_pos]
      tk_name_pos <- match(ref_tk, asset_data$TickerBenchmark)
      names(w)[match(asset_not_in_db[!is.na(tk_name_pos)], names(w))] <- asset_data$Asset[tk_name_pos[!is.na(tk_name_pos)]]
    }else if(invest_assets=='IA'){
      ref_tk <- asset_data$TickerInvestAsset[asset_pos]
      tk_name_pos <- match(ref_tk, asset_data$TickerBenchmark)
      names(w)[match(asset_not_in_db[!is.na(tk_name_pos)], names(w))] <- asset_data$Asset[tk_name_pos[!is.na(tk_name_pos)]]
    }
  }
  return(w)
}
