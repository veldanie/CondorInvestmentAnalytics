#' port_duration
#'
#' Calculate the normalized duration of multiple portfolios bond market component
#'
#' @param asset_data 
#' @param assets_w matrix (Must be 2-dim), columns are the portfolios, rows assets names, vales are the weights. Colnames and rownames.
#' @return List of portfolios duration, assets normalized duration, assets normalized weight, and assets original duration.
#' @export

port_duration <- function (asset_data, assets_w, dur_field='Duration')
{
  
  if(length(assets_w)==0){
    return(list(ports_final_dur = NULL, ports_dur_norm = NULL, assets_w_norm = NULL, ports_dur = NULL))
  }
  
  assets_names <- row.names(assets_w)
  assets_dur <-  as.double(asset_data[[dur_field]][match(assets_names,asset_data$Asset)])
  pos_na <- is.na(assets_dur)
  assets_dur[pos_na] <- 0
  ports_dur <- (t(assets_dur) %*% as.matrix(assets_w))[1,]
  return (ports_dur) 
}
