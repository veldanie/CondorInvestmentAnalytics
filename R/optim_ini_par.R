
#' Optimization Starting parameters
#'
#' Inital parameters
#' @param lb Lower bound,
#' @param ub Upper bound,
#' @param f_const Matrix of factor restrictions,
#' @param ind_rel Indicator of relative restriction of an asset wit respect to a group,
#' @param pos_asset Position of relative asset restriction.
#' @param f_const_lb Factor constraint lower bound,
#' @param f_const_ub Factor constraint upper bound,
#' @param risk_fun Risk function,
#' @param risk_obj Risk objective
#' @return Initial guess
#' @export

optim_ini_par <- function(lb, ub, f_const=NULL, ind_rel=NULL, pos_asset=NULL, f_const_lb=0, f_const_ub=Inf, risk_fun=NULL, risk_obj=100) {
  n_par <- length(lb)

  E <- t(rep(1, n_par))
  eq_const <- 1

  if(all(f_const==1) || is.null(f_const)){
    G <- rbind(diag(n_par),-1*diag(n_par))
    h <- c(lb, -1*ub)
  }else{
    G <- rbind(diag(n_par),-1*diag(n_par), f_const[!ind_rel,], -1*f_const[!ind_rel,])
    h <- c(lb, -1*ub, f_const_lb[!ind_rel], -1*f_const_ub[!ind_rel])
  }

  ws <- try(xsample(E=E, F=eq_const, G=G, H=h, iter = 10000)$X, silent = TRUE)
  if(class(ws)=='try-error'){
    w0 <- lb + (1-sum(lb))*(ub - lb)/sum(ub - lb)
  }else{
    #Transform output to incorporate relative constraints:
    if(!is.null(ind_rel) && any(ind_rel)){
      pos_rest <- which(ind_rel)
      for(j in 1:length(pos_rest)){
        i <- pos_rest[j]
        ind_group <- f_const[i,]!=0
        ind_rel_w <- pos_asset[j,]!=0
        group_sums <- rowSums(ws[, ind_group, drop = FALSE])
        rel_w <- rowSums(ws[, ind_rel_w, drop = FALSE])/group_sums
        ind_valid_w <- rel_w <= f_const_ub[i] & rel_w >= f_const_lb[i]
        if(sum(ind_valid_w)<100){
          ind_adjust_down <- rel_w > f_const_ub[i]
          ind_adjust_up <- rel_w < f_const_lb[i]
          if(any(ind_adjust_down)){
            adjust_val <- rowSums(ws[ind_adjust_down, ind_rel_w, drop = FALSE]) - f_const_ub[i]*group_sums[ind_adjust_down]
            ws_ini_ex <- ws[ind_adjust_down, setdiff(which(ind_group), which(ind_rel_w)), drop=FALSE]
            ws[ind_adjust_down, ind_rel_w] <- f_const_ub[i]*group_sums[ind_adjust_down]*(ws[ind_adjust_down, ind_rel_w]/(rowSums(ws[ind_adjust_down, ind_rel_w, drop = FALSE] %*% rep(1, sum(ind_rel_w)))))
            ws[ind_adjust_down, setdiff(which(ind_group), which(ind_rel_w))] <- ws_ini_ex+adjust_val*ws_ini_ex/rowSums(ws_ini_ex)
          }
          if(any(ind_adjust_up)){
            adjust_val <- f_const_ub[i]*group_sums[ind_adjust_down] - rowSums(ws[ind_adjust_up, ind_rel_w, drop = FALSE])
            ws_ini_ex <- ws[ind_adjust_up, setdiff(which(ind_group), which(ind_rel_w)), drop=FALSE]
            ws[ind_adjust_up, ind_rel_w] <- f_const_lb[i]*group_sums[ind_adjust_down]*(ws[ind_adjust_up, ind_rel_w]/(rowSums(ws[ind_adjust_up, ind_rel_w, drop = FALSE] %*% rep(1, sum(ind_rel_w)))))
            ws[ind_adjust_up, setdiff(which(ind_group), which(ind_rel_w))] <- ws_ini_ex-adjust_val*ws_ini_ex/rowSums(ws_ini_ex)
          }
          rel_w <- ws[, which(ind_rel_w)]/group_sums
          ind_valid_w <- rel_w <= (f_const_ub[i] + 1e-5) & rel_w >= f_const_lb[i]
        }
        ws <- ws[ind_valid_w,]
      }
    }
    # Select initial point that complies the risk target and the other restrictions.
    ws_valid_pre <- apply(ws, 1, function(x) all(x>lb) & all(x<ub))
    if(!is.null(f_const) && !all(f_const==1)){
      ws_valid_exrisk <- ws_valid_pre & apply(ws, 1, function(x) all(f_const[!ind_rel,]%*%x < f_const_ub[!ind_rel]) & all(f_const[!ind_rel,]%*%x > f_const_lb[!ind_rel]))
    }
    if(!is.null(risk_fun)){
      risk_val <- apply(ws, 1, risk_fun)
      ws_valid <- ws_valid_exrisk & apply(ws, 1, function(x) risk_fun(x)<risk_obj)
    }

    if(any(ws_valid)){
      w0 <- ws[base::sample(which(ws_valid), 1),]
    }else if(any(ws_valid_exrisk)){
      w0 <- ws[which.min(risk_val),] # Min risk sample
    }else{
      w0 <- ws[base::sample(which(ws_valid_pre), 1),]
    }
  }
  names(w0) <- names(lb)
  return(w0)
}
