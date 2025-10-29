#' all_tickers_extraction
#'
#' Extract all tickers from assets or portfolios, including currencies and tactical portfolios
#' @param url_database url request to query_database lambda
#' @param url_token url request token
#' @param username_req Username who needs token. 
#' @param password_req Password
#' @param asset_data Dataframe with info regarding each asset. 
#' @param assets Id of assets of interest. Vector
#' @param ports Id of portfolios of interest. Vector
#' @param fixed_curr list of currencies of interest. Vector
#' @param tactical_port Add tactical portfolios tickers, must have ports
#' @param w_bench data from portfolio 'Mercado', from w_bench()
#' @param w_optim data from portfolio 'Óptimo', from reactw$w_optim
#' @param w_optim_mv data from portfolio 'Mínima Varianza', from w_optim_mv()
#' @param w_optim_pm data from portfolio 'Mínima Varianza', from reactw$w_optim_pm
#' @param port_factor_disagg portfolio fund disaggregation. 
#' @param ETF_IA_tickers get assets ETF and Funds tickers, from asset_data 
#' @param get_fund get fund (IA) series data, and dependecies
#' @return ticker_list, assets_list
#' @export
all_tickers_extraction <- function(url_database, url_token, username_req, password_req,asset_data, benchmarks, assets = NULL, ports = NULL, 
                                   fixed_curr = NULL, tactical_port = FALSE, w_bench = NULL,
                                   w_optim = NULL, w_optim_mv = NULL, w_optim_pm = NULL, 
                                   port_factor_disagg = FALSE, ETF_IA_tickers = TRUE, get_fund = FALSE) {
  ticker_list<- NULL; assets_names<- NULL; assets_list <- NULL; port_assets <- NULL
  if(!is.null(assets)){
    assets_names <- unique(c(assets_names,assets))
  }
  if(!is.null(ports)){
    #conn <- poolCheckout(db)
    #DBI::dbBegin(conn)
    for (port_i in ports) {
      if(port_i == 'Mercado'){
        if (!is.null(w_bench)) {
          assets_names <- unique(c(assets_names,names(w_bench)))
        }else{
          warning(paste0('w_bench() is needed for portfolio Mercado in all_tickers_extraction'))
        }
      }else if(port_i == 'Óptimo'){
        if (!is.null(w_optim)) {
          assets_names <- unique(c(assets_names,names(w_optim)))
        }else{
          warning(paste0('reactw$w_optim is needed for portfolio Óptimo in all_tickers_extraction'))
        }
      }else if(port_i == 'Mínima Varianza'){
        if (!is.null(w_optim_mv)) {
          assets_names <- unique(c(assets_names,names(w_optim_mv)))
        }else{
          warning(paste0('w_optim_mv() is needed for portfolio Óptimo in all_tickers_extraction'))
        }
      }else if(port_i %in%  unique(benchmarks$Benchmark)){
        pos_bench <- which(benchmarks$Benchmark == port_i)
        assets_names <- unique(c(assets_names,benchmarks$Asset[pos_bench]))
      }else if(length(w_optim_pm) > 0  && port_i %in% names(w_optim_pm)){
        assets_names <- unique(c(assets_names,names(w_optim_pm[[port_i]])))
      }else{
        port_db_i <- as.data.frame(query_database(url_database, sprintf("SELECT * FROM Weights WHERE PortId ='%s'", port_i), url_token, user_condor, pass_condor))
        port_assets <- unique(port_db_i$Asset)
        assets_names <- unique(c(assets_names,port_assets))
      }
      
      if(port_factor_disagg){
        ind_fund_bench <- port_assets %in% benchmarks$Benchmark
        fund_names_bench <- port_assets[ind_fund_bench]
        ind_fund_db <- port_assets %in% (query_database(url_database, "SELECT DISTINCT Id FROM Portfolios", url_token, user_condor, pass_condor) %>% pull(Id))
        fund_names_db <- port_assets[ind_fund_db]
        fund_names <- unique(c(fund_names_bench, fund_names_db))
        ind_fund <- port_assets %in% fund_names
        
        # ind_fund <- assets_names %in% benchmarks$Benchmark
        if(any(ind_fund)){
          w_funds <- c()
          for (fi in fund_names){
            if(fi %in% fund_names_bench){
              fund_asset <- benchmarks %>% filter(Benchmark==fi) %>% dplyr::select(Asset)
              assets_names <- unique(c(assets_names,fund_asset$Asset))
            }else if(fi %in% fund_names_db){
              fund_asset <- query_database(url_database, sprintf("SELECT * FROM Weights WHERE PortId ='%s'", fi), url_token, user_condor, pass_condor)
              if(length(fund_asset)){
                fund_asset <- fund_asset %>% dplyr::select(Asset)
              } else {
                fund_asset <- data.frame(Asset = character(), stringsAsFactors = FALSE)
              }
              assets_names <- unique(c(assets_names,fund_asset %>% dplyr::pull(Asset)))
            }
          }
        }
      }
      
      port_tickers <- as.data.frame(query_database(url_database, sprintf("SELECT * FROM InvestTickers WHERE PortId ='%s'", port_i), url_token, user_condor, pass_condor))
      if(length(port_tickers) > 0){
        ticker_list <- unique(c(ticker_list, port_tickers$Ticker)) #Listado de fondos para descargar series
      }
      if(length(ticker_list)>0){
        index_df <- query_database(url_database, paste0("SELECT * FROM UserIndex WHERE Ticker IN (",paste(shQuote(ticker_list, type = "sh"), collapse = ","),")"), url_token, user_condor, pass_condor)
        if(length(index_df)>0){
          index_df <- index_df %>% dplyr::select(IndexId, Asset, Weight, Ticker)
        } else {
        index_df <- data.frame(NULL)
      }
      
      if(length(index_df)>0){
        ct_ind <- ticker_list %in% index_df$Ticker
        ticker_list <- unique(c(ticker_list[!ct_ind], get_ticker(index_df$Asset,asset_data)))
      }
      
    }
    #DBI::dbCommit(conn)
    #poolReturn(conn)
    if (tactical_port){
      #conn <- poolCheckout(db)
      #DBI::dbBegin(conn)
      for (port_i in ports){
        tac_ind <- grepl("Tactico", port_i)
        if(tac_ind){
          port_db <- gsub(" - Tactico", "", port_i)
        }else{
          port_db <- paste0(port_i, " - Tactico")
        }
        port_df <- as.data.frame(query_database(url_database, sprintf("SELECT * FROM Weights WHERE PortId ='%s'", port_db), url_token, user_condor, pass_condor))
        
        if(nrow(port_df)>0){
          port_assets <- unique(port_df$Asset)
          assets_names <- unique(c(assets_names,port_assets))
        }else{
          next
        }
        port_tickers <- as.data.frame(query_database(url_database, sprintf("SELECT * FROM InvestTickers WHERE PortId ='%s'", port_db), url_token, user_condor, pass_condor))
        if(length(port_tickers) > 0){
          ticker_list <- unique(c(ticker_list, port_tickers$Ticker)) #Listado de fondos para descargar series
        }
        if (length(ticker_list)>0){
          index_df <- as.data.frame(query_database(url_database, paste0("SELECT * FROM UserIndex WHERE Ticker IN (",paste(shQuote(ticker_list, type = "sh"), collapse = ","),")"), url_token, user_condor, pass_condor) %>% dplyr::select(IndexId, Asset, Weight, Ticker))  
          if(length(index_df)>0){
            index_df <- index_df %>% dplyr::select(IndexId, Asset, Weight, Ticker)
          }
        } else {
          index_df <- data.frame(NULL)
        }
        
        if(nrow(index_df)!=0){
          ct_ind <- ticker_list %in% index_df$Ticker
          ticker_list <- unique(c(ticker_list[!ct_ind], get_ticker(index_df$Asset,asset_data)))
        }
      }
      #DBI::dbCommit(conn)
      #poolReturn(conn)
    }
  }
  if(get_fund){
    funds_names <- asset_data$Asset[match(get_ticker(assets_names,asset_data, invest_assets = 'IA'),asset_data$TickerBenchmark)]
    funds_names <- funds_names[!is.na(funds_names)]
    assets_names <- unique(c(assets_names,funds_names))
  }
  # FX
  ticker_list <- unique(c(ticker_list, na.omit(asset_data$Currency[match(ticker_list, asset_data$TickerBenchmark)])))
  
  if(!is.null(assets_names)){
    pos_assets <- unique(match(assets_names, asset_data$Asset))
    ticker_list <- unique(c(ticker_list,asset_data$Currency[pos_assets]))
    ticker_list <- unique(c(ticker_list,get_ticker(assets_names,asset_data)))
    if(ETF_IA_tickers){
      ticker_list <- unique(c(ticker_list,asset_data$CurrencyETF[pos_assets]))
      ticker_list <- unique(c(ticker_list,asset_data$CurrencyIA[pos_assets]))
      ticker_list <- unique(c(ticker_list,get_ticker(assets_names,asset_data, invest_assets = 'ETF')))
      ticker_list <- unique(c(ticker_list,get_ticker(assets_names,asset_data, invest_assets = 'IA')))
      ticker_list <- ticker_list[ticker_list!='USD']
    }
    
  }
  if(length(fixed_curr)!=0){
    ticker_list <- unique(c(ticker_list,fixed_curr))
    ticker_list <- ticker_list[!is.na(ticker_list)]
    ticker_list <- ticker_list[ticker_list!='USD']
  }
  pos_tickers <- unique(match(ticker_list, asset_data$TickerBenchmark))
  pos_tickers <- na.omit(pos_tickers)
  assets_list <- unique(c(assets_list,asset_data$Asset[pos_tickers]))
  assets_list <- assets_list[!is.na(assets_list)]
  
  return(list(ticker_list = ticker_list, assets_list = assets_list))
}
