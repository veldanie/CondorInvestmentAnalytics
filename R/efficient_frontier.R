#' Efficient frontier.
#'
#' Efficient Frontier.
#' @param mu Expected return,
#' @param Sigma Covariance matrix,
#' @param lb Lower bound,
#' @param ub Upper bound,
#' @param lambda Vector of risk aversion coefficients,
#' @param add_sigma Additional volatility points,
#' @param add_mu Additional mean points,
#' @param n_nodes Number of nodes,
#' @param gen_vols Generate Vols,
#' @param fit_spline Fit smooth spline,
#' @param port_id_pref Portfolios id preffix,
#' @return Vectors or portfolio means and volatilities for different levels of risk aversion, and smooth spline object,
#' @export

efficient_frontier <- function(mu, Sigma, lb = rep(0, ncol(Sigma)), ub = rep(1, ncol(Sigma)), lambda = NULL, vols = NULL, add_sigma = NULL, add_mu = NULL, n_nodes = 10, gen_vols = FALSE, fit_spline = FALSE, port_id_pref = "") {
  n_assets <- length(mu)
  w_ini <- lb + (1 - sum(lb)) * (ub - lb)/sum(ub - lb)


  if(!is.null(lambda)){
    w_mat <- matrix(0, nrow = n_assets, ncol = length(lambda))
    mean_vec <- sigma_vec <- rep(0, length(lambda))
    for (i in 1:length(lambda)){

      obj_fun <- function(w){
        util <- -(t(w)%*%mu - 0.5 * lambda[i] * t(w)%*%Sigma%*%w)
        as.numeric(util)
      }
      weights <- try(optim_portfolio(w_ini = w_ini, fn = obj_fun, lb = lb, ub = ub,
                                    eqfun = sum_weigths, eqB = 1, method = "GD"),
                     silent = TRUE)
      if(class(weights)!="try-error" && !all(weights==w_ini)){
        port_res <- portfolio_return(weights, mu, Sigma)
        mean_vec[i] <- port_res$port_mean_ret
        sigma_vec[i] <- port_res$port_vol
      }
    }
  }else{
    if(!is.null(vols) || gen_vols){
      obj_fun <- utility_fun(type = 'absolute', mu = mu, Sigma = Sigma,lambda = 0)
      if(gen_vols){
        asset_vols <- sqrt(diag(Sigma))
        vols <- seq(min(asset_vols), max(asset_vols), length = n_nodes)
      }
      risk_function <- risk_fun(Sigma = Sigma)
      w_mat <- matrix(0, nrow = n_assets, ncol = length(vols))
      mean_vec <- sigma_vec <- rep(0, length(vols))
      for (i in 1:length(vols)){
        weights <- try(optim_portfolio(w_ini = w_ini, fn = obj_fun, lb = lb, ub = ub,
                                       eqfun = sum_weigths, eqB = 1, ineqfun = risk_function,
                                       ineqLB = 0, ineqUB = vols[i], method = "GD"),
                       silent = TRUE)
        if(class(weights)!="try-error" && !all(weights==w_ini)){
          port_res <- portfolio_return(weights, mu, Sigma)
          mean_vec[i] <- port_res$port_mean_ret
          sigma_vec[i] <- port_res$port_vol
          w_mat[, i] <- weights
        }
      }
    }else{
      sigma_vec <- 0
    }
  }
  valid_pos <- sigma_vec > 0
  if(sum(valid_pos)>0){
    mean_vec_front <- unique(mean_vec[valid_pos])
    sigma_vec_front <- unique(sigma_vec[valid_pos])
    w_mat <- w_mat[, valid_pos, drop = FALSE]
    ports_id <- 1:length(mean_vec_front)
    colnames(w_mat) <- paste0(port_id_pref, ports_id)
    rr_df <- data.frame(Port=ports_id, Ret=round(100*mean_vec_front, 2), Vol=round(100*sigma_vec_front, 2))
    w_df <- data.frame(Asset=names(mu), round(100*w_mat, 2), check.names = FALSE)
  }else{
    mean_vec_front <- NULL
    sigma_vec_front <- NULL
    w_mat <- NULL
    rr_df <- NULL
    w_df <- NULL
  }
  sigma_points <- c(sigma_vec, add_sigma)
  if(!is.null(sigma_points)){
    n_points <- length(unique(round(c(sigma_vec, add_sigma), 4)))
  }else{
    n_points <- 0
  }
  if(fit_spline){
    ef_ss <- smooth.spline(x = c(sigma_vec, add_sigma), y = c(mean_vec, add_mu)) #Efficient frontier smooth spline
  }else{
    ef_ss <- NULL
  }
  return(list(mean_vec = c(mean_vec_front, add_mu), sigma_vec = c(sigma_vec_front, add_sigma), mean_vec_front=mean_vec_front, sigma_vec_front=sigma_vec_front, ef_ss = ef_ss, n_points = n_points, w_df = w_df, rr_df = rr_df))
}
