#' Efficient frontier.
#'
#' Efficient Frontier.
#' @param mu Expected return
#' @param Sigma Covariance matrix
#' @param lambda Vector of risk aversion coefficients
#' @return Vectors or portfolio means and volatilities for different levels of risk aversion, and smooth spline object.
#' @export

efficient_frontier <- function(mu, Sigma, lambda = seq(0.5, 3, 0.25)){

  n_assets <- length(mu)
  mean_vec <- sigma_vec <- rep(0, length(lambda))
  for (i in 1:length(lambda)){

    obj_fun <- function(w){
      util <- -(t(w)%*%mu - 0.5 * lambda[i] * t(w)%*%Sigma%*%w)
      as.numeric(util)
    }
    weights <- optim_portfolio(w_ini = rep(1/n_assets, n_assets), fn = obj_fun, lb = rep(0,n_assets), ub = rep(1, n_assets),
                                  eqfun = sum_weigths, eqB = 1, method = "GD")

    port_res <- portfolio_return(weights, mu, Sigma)
    mean_vec[i] <- port_res$port_mean_ret
    sigma_vec[i] <- port_res$port_vol
  }
  ef_ss <- smooth.spline(x = sigma_vec, y = mean_vec) #Efficient frontier smooth spline
  return(list(mean_vec = mean_vec, sigma_vec = sigma_vec, ef_ss = ef_ss))
}
