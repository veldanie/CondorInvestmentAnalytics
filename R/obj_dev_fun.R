
#' Objective deviation function.
#'
#' Objective deviation function.
#' @param w Portfolio weights.
#' @param series return series.
#' @param risk_target risk target
#' @param ow_assets Overweight assets.
#' @param uw_assets Underweight assets.
#' @param per returns period expressed as number of times per year..
#' @param type weight deviation type.
#' @param risk Risk measure: vol, te, var, cvar.
#' @param quant Quantile.
#' @return Deviation objective function.
#' @export


obj_dev_fun <- function(w, series, risk_target, ow_assets, uw_assets, per = 12, type = "unif", risk = "vol", quant = 0.95) {
  covar_mat <- cov(series) * per
  vol_ini <- as.numeric(sqrt(t(w) %*% covar_mat %*% w))

  w_new<- w
  neg_rets_ini <- -as.vector(series %*% w)
  var_ini <- quantile(neg_rets_ini, probs = quant)
  cvar_ini <- mean(neg_rets_ini[neg_rets_ini > var_ini])

  n_ow_assets <- length(ow_assets)
  n_uw_assets <- length(uw_assets)
  w_ow <- w[ow_assets]
  w_uw <- w[uw_assets]
  w_ow_norm <- w_ow/sum(w_ow)
  w_uw_norm <- w_uw/sum(w_uw)

  if(substr(tolower(risk),1,3) == "vol"){
    if(type=="unif"){
      return(function(x){
        w_new[ow_assets] <- w_ow + x/n_ow_assets
        w_new[uw_assets] <- w_uw - x/n_uw_assets
        return((abs(as.numeric(sqrt(t(w_new) %*% covar_mat %*% w_new)) - vol_ini) - risk_target)**2)
      }
      )
    }else{
      return(function(x){
        w_new[ow_assets] <- w_ow + x*w_ow_norm
        w_new[uw_assets] <- w_uw - x*w_uw_norm
        return((abs(as.numeric(sqrt(t(w_new) %*% covar_mat %*% w_new) - vol_ini)) - risk_target)**2)
      }
      )
    }
  }else if(substr(tolower(risk),1,4) == "var" || substr(tolower(risk),1,4) == "ver"){
    if(type=="unif"){
      return(function(x){
        w_new[ow_assets] <- w_ow + x/n_ow_assets
        w_new[uw_assets] <- w_uw - x/n_uw_assets
        neg_rets_new <- -as.vector(series %*% w_new)
        return((abs(quantile(neg_rets_new, probs = quant)  - var_ini) - risk_target)**2)
      }
      )
    }else{
      return(function(x){
        w_new[ow_assets] <- w_ow + x*w_ow_norm
        w_new[uw_assets] <- w_uw - x*w_uw_norm
        neg_rets_new <- -as.vector(series %*% w_new)
        return((abs(quantile(neg_rets_new, probs = quant)  - var_ini) - risk_target)**2)
      }
      )
    }
  }else if(substr(tolower(risk),1,4) == "cvar" || substr(tolower(risk),1,4) == "verc"){
    if(type=="unif"){
      return(function(x){
        w_new[ow_assets] <- w_ow + x/n_ow_assets
        w_new[uw_assets] <- w_uw - x/n_uw_assets
        neg_rets_new <- -as.vector(series %*% w_new)
        return((abs(mean(neg_rets_new[neg_rets_new > quantile(neg_rets_new, probs = quant)])  - cvar_ini) - risk_target)**2)
      }
      )
    }else{
      return(function(x){
        w_new[ow_assets] <- w_ow + x*w_ow_norm
        w_new[uw_assets] <- w_uw - x*w_uw_norm
        neg_rets_new <- -as.vector(series %*% w_new)
        return((abs(mean(neg_rets_new[neg_rets_new > quantile(neg_rets_new, probs = quant)])  - cvar_ini) - risk_target)**2)
      }
      )
    }
  }else{
    if(type=="unif"){
      return(function(x){
        w_new[ow_assets] <- w_ow + x/n_ow_assets
        w_new[uw_assets] <- w_uw - x/n_uw_assets
        return((sqrt(t(w_new-w) %*% covar_mat %*% (w_new-w)) - risk_target)**2)
      }
      )
    }else{
      return(function(x){
        w_new[ow_assets] <- w_ow + x*w_ow_norm
        w_new[uw_assets] <- w_uw - x*w_uw_norm
        return((sqrt(as.numeric(t(w_new-w) %*% covar_mat %*% (w_new-w))) - risk_target)**2)
      }
      )
    }
  }
}


