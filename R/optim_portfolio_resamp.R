#' Optimal portfolio.
#'
#' Optimal Portfolio. Assuming returns
#' @param mu Vector of mean returns.
#' @param Sigma Covariance matrix.
#' @param lb Lower bound.
#' @param ub Upper bound.
#' @param lambda Risk aversion coefficient.
#' @N Number of observations.
#' @M Number of portfolios.
#' @plot_ef Indicator to plot efficient frontier.
#' @spar Smoothing parameter
#' @method Default: GD
#' @n.restarts Number of solver restarts.
#' @n.sim Random parameters for every restart of the solver.
#' @return Optimal weights, mean resampled optimal weights, matrix of sampled weights.
#' @export

optim_portfolio_resamp <- function(mu, Sigma, lb, ub, w_ini, lambda = 1, N = 2e2, M = 1e3, plot_ef = FALSE, spar = 0, ineqfun = NULL, ineqLB = NULL, ineqUB = NULL, method = 'GD', n.restarts = 10, n.sim = 20000, conf_int = 0.9){

  n_assets <- length(mu)

  w_optim_mat <- matrix(0, nrow = M, ncol = n_assets)
  colnames(w_optim_mat) <- names(mu)

  obj_fun <- utility_fun(type = 'absolute', mu = mu, Sigma = Sigma, lambda = lambda)
  w_optim <- optim_portfolio(w_ini = w_ini, fn = obj_fun, lb = rep(0, n_assets), ub = rep(1, n_assets),
                             eqfun = sum_weigths, eqB = 1, ineqfun = NULL, ineqLB = NULL, ineqUB = NULL, method = method)
  port_means <- port_vols <- rep(0, M)

  for (i in 1:M){
    sample_i <- mvrnorm(n = N , mu, Sigma)
    mu_i <- apply(sample_i, 2, mean)
    Sigma_i <- covar(sample_i)$cov_matrix
    if(!is.null(ineqUB)){lambda <- 0}
    obj_fun <- utility_fun(type = 'absolute', mu = mu_i, Sigma = Sigma_i, lambda = lambda)
    w_optim_mat[i,] <- optim_portfolio(w_ini = w_ini, fn = obj_fun, lb = lb, ub = ub,
                                       eqfun = sum_weigths, eqB = 1, ineqfun = ineqfun, ineqLB = ineqLB, ineqUB = ineqUB, method = method, n.restarts = n.restarts, n.sim = n.sim)
    port_ret <- unlist(portfolio_return(w_optim_mat[i,], mu, Sigma)[c('port_mean_ret', 'port_vol')])
    port_means[i] <- port_ret[1]
    port_vols[i] <- port_ret[2]
  }
  w_optim_resamp <- apply(w_optim_mat, 2, mean)
  w_optim_resamp_sd <- apply(w_optim_mat, 2, sd)
  z <- qnorm(conf_int + 0.5*(1 - conf_int))
  w_optim_lower <- sapply(w_optim_resamp - z * w_optim_resamp_sd, max, 0)
  w_optim_upper <- w_optim_resamp + z * w_optim_resamp_sd
  if(plot_ef){
    ef_ss <- efficient_frontier(mu, Sigma)$ef_ss
    x <- seq(min(port_vols),max(port_vols),0.001)
    plot(100*x , y = 100*predict(ef_ss, x)$y, type = 'h', main = 'Efficient Frontier', xlab = 'Volatility', ylab = 'Returns', col = 'grey')
    port_ret <- 100*unlist(portfolio_return(w_optim, mu, Sigma)[c('port_mean_ret', 'port_vol')])
    points(x = port_ret[2], y = port_ret[1], col = 'red', pch = 3)

    port_ret <- 100*unlist(portfolio_return(w_optim_resamp, mu, Sigma)[c('port_mean_ret', 'port_vol')])
    points(x = port_ret[2], y = port_ret[1], col = 'blue', pch = 3)
    legend('topleft', legend = c('Optimum Portfolio', 'Optimum Resampled Portfolio'), lty =1, col = c('red', 'blue'), bty = 'n')
  }

  return(list(w_optim = w_optim, w_optim_resamp = w_optim_resamp, w_optim_resamp_sd = w_optim_resamp_sd, w_optim_lower = w_optim_lower, w_optim_upper = w_optim_upper, w_optim_matrix = w_optim_mat))
}
