
#' Calculates tactical deviation per asset given restrictions.
#'
#' Tactical deviation per asset
#' @param w Portfolio weights.
#' @param val_dev Deviation value.
#' @param min_dev_asset Min. deviation per asset.
#' @param lower Lower weights bound.
#' @param type uniform or proportional.
#' @return Tactical dev
#' @export

tac_dev <- function(w, ow_assets, uw_assets, val_dev = 0,  min_dev_asset = 0.01, lower = 0, type = "uniform") {
  w_dev_ow <- w_dev_uw <- 0
  w_new <- w
  w_devs <- w_new - w
  pos_0_ow <- rep(FALSE, length(ow_assets))

  if(val_dev>0){
    if(substr(tolower(type), 1,4)=="unif" || any(w_new[ow_assets]==0)){
      w_dev_ow <- rep(val_dev/length(ow_assets), length(ow_assets))
      w_dev_uw <- rep(val_dev/length(uw_assets), length(uw_assets))
    }else{
      w_dev_ow <- val_dev*w_new[ow_assets]/sum(w_new[ow_assets])
      w_dev_uw <- val_dev*w_new[uw_assets]/sum(w_new[uw_assets])
    }
    if(any(w_dev_ow < min_dev_asset) || any(w_dev_uw < min_dev_asset)){
      pos_0_ow <- w_dev_ow < min_dev_asset
      pos_0_uw <- w_dev_uw < min_dev_asset
      if(all(pos_0_ow) || all(pos_0_ow)){
        return(list(w_new = w_new, w_devs = w_devs))
      }else{
        dev_adj_ow <- sum(w_dev_ow[pos_0_ow])
        dev_adj_uw <- sum(w_dev_uw[pos_0_uw])
        dev_adj_net <- dev_adj_ow - dev_adj_uw
        w_dev_ow[pos_0_ow] <- 0
        w_dev_uw[pos_0_uw] <- 0
        if(dev_adj_net > 0){
          w_dev_uw[!pos_0_uw] <- w_dev_uw[!pos_0_uw] - dev_adj_net/sum(!pos_0_uw)
        }else if(dev_adj_net < 0){
          w_dev_ow[!pos_0_ow] <- w_dev_ow[!pos_0_ow] - abs(dev_adj_net)/sum(!pos_0_ow)
        }
      }
    }
    w_new[ow_assets] <- w_new[ow_assets] + w_dev_ow
    w_new[uw_assets] <- w_new[uw_assets] - w_dev_uw
    if(any(w_new < lower)){
      pos_0 <- w_new < lower
      dev_adj <- abs(sum(w_new[pos_0]))
      w_new[pos_0] <- 0
      w_new[ow_assets[!pos_0_ow]] <- w_new[ow_assets[!pos_0_ow]] - dev_adj/sum(!pos_0_ow)
    }
  }
  w_devs <- w_new - w

  return(list(w_new = w_new, w_devs = w_devs))
}
