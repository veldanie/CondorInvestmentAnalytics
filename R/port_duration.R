#' port_duration
#'
#' Calculate the normalized duration of multiple portfolios bond market component
#'
#' @param asset_data 
#' @param assets_names matrix, columns are the portfolios, values are the asset names. Colnames.
#' @param assets_w matrix (Must be 2-dim), columns are the portfolios, rows assets names, vales are the weights. Colnames and rownames.
#' @return List of portfolios duration, assets normalized duration, assets normalized weight, and assets original duration.
#' @export

port_duration <- function (asset_data, assets_names, assets_w)
{
  
  if (any(dim(assets_names)!=dim(as.data.frame(assets_w)))){
    warning(paste0('asset_names matrix and asset_w weights matrix must have dimensions'))
  }
  if((length(assets_names)==0)|(length(assets_w)==0)){
    return(list(ports_final_dur = NULL, ports_dur_norm = NULL, assets_w_norm = NULL, ports_dur = NULL))
  }
  
  ports_dim <- dim(assets_names)
  ports_dur <-  as.double(asset_data$DurMod[match(assets_names,asset_data$Asset)])
  dim(ports_dur) <-ports_dim
  pos_na <- is.na(ports_dur)
  ports_dur[pos_na] <- 0
  assets_w[pos_na] <- 0
  sum_dur <- apply(assets_w,2,sum)
  assets_w_norm <- assets_w/sum_dur
  ports_dur_norm <- ports_dur*assets_w_norm
  ports_final_dur <- apply(ports_dur_norm,2,sum)
  
  colnames(ports_dur) <- colnames(assets_w)
  row.names(ports_dur) <- row.names(assets_w)
  
  return(list(ports_final_dur = ports_final_dur, ports_dur_norm = ports_dur_norm, assets_w_norm = assets_w_norm, ports_dur = ports_dur))
}
