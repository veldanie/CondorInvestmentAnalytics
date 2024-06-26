#' Optimal portfolio.
#'
#' Optimal Portfolio. Assuming returns
#' @param rets Returns series.
#' @param per Returns period
#' @param mu_ann Annualized mean returns used to generate samples
#' @param Sigma_ann Annualized cov matrix used to generate samples
#' @param lb Lower bound.
#' @param ub Upper bound.
#' @param lambda Risk aversion coefficient.
#' @param N Number of observations.
#' @param M Number of portfolios.
#' @param plot_ef Indicator to plot efficient frontier.
#' @param spar Smoothing parameter
#' @param method Default: GD
#' @param n.restarts Number of solver restarts.
#' @param n.sim Random parameters for every restart of the solver.
#' @param mom Mean of means.
#' @param k Number of groups.
#' @param sample_window Sample window of returns.
#' @param len_window Length of the window in months.
#' @param dyn_mu Dynamic means
#' @param q_sel Quantile to select portfolio to reach risk target. Vola or diversification criterion.
#' @return Optimal weights, mean resampled optimal weights, matrix of sampled weights.
#' @export

optim_portfolio_resamp <- function(rets, per = 12, mu_ann=NULL, Sigma_ann=NULL, util_type="absolute", lb=rep(0, ncol(rets)), ub=rep(1, ncol(rets)), w_ini=NULL, lambda = 1, N = 2e2, M = 1e3, plot_ef = FALSE, spar = 0, ineqfun = NULL, ineqLB = 0, ineqUB = NULL, method = 'GD', n.restarts = 10, n.sim = 20000, conf_int = 0.9, shrink_cov = FALSE, mom = FALSE, k = NULL, sample_window = FALSE, len_window = 36, dyn_mu = FALSE, q_sel = 0.2, same_assets_bench = TRUE, w_bench=NULL){
  options('nloptr.show.inequality.warning'=FALSE)
  options(warn=-1)
  if(is.null(w_ini) & util_type=="relative"){
    stop("Benchmark not available for relative utility function.")
  }
  if(util_type=="relative" && same_assets_bench){
    sum_weights <- function(w){return(sum(w)+1)}
  }
  if(util_type=="relative" && !same_assets_bench && is.null(w_ini)){
    stop("Relative resampling: Please provide w_ini!")
  }else{
    asset_names <- names(w_ini)
  }
  if(is.null(w_ini)){
    w_ini <- lb + (1-sum(lb))*(ub - lb)/sum(ub - lb)
    asset_names <- colnames(rets)
    names(w_ini) <- asset_names
  }else{
    asset_names <- names(w_ini)
  }
  Sigma_all <- covar(rets, per=per, shrink = shrink_cov)
  mu_all <- apply(rets, 2, mean)
  if(is.null(mu_ann) || is.null(Sigma_ann)){
    Sigma <- Sigma_all$cov_matrix
    date_ini <- index(rets)[1]
    date_last <- tail(index(rets), 1)

    if(mom | dyn_mu){
      if(length(k)==0){
        k <- round(as.numeric((date_last - date_ini)/365)/(len_window/12))
      }
      n_rows <- nrow(rets)
      size <- ceiling(n_rows/k)
      sample_means <- matrix(0, nrow=k, ncol=length(w_ini))
      for (i in 1:k){
        if(i==k){
          sample_means[i,] <- colMeans(rets[(1+size*(i-1)):nrow(rets)])
        }else{
          sample_means[i,] <- colMeans(rets[(1+size*(i-1)):(size*i)])
        }
      }
      if(dyn_mu){
        mu <- rbind(sample_means, mu_all)
        colnames(mu) <- names(w_ini)
      }else{
        mu <- apply(sample_means,2,median)
        names(mu) <- asset_names
      }
    }else{
      mu <- mu_all
    }
  }else{
    dyn_mu <- sample_window <- FALSE
    mu <- mu_ann/per
    Sigma <- Sigma_ann/per
  }

  if(is.null(ineqfun) && (!is.null(ineqUB) | any(ineqLB>0))){
    ineqfun <- risk_fun(Sigma = Sigma_all$cov_matrix_ann, type = 'vol') # Assumes volatility restriction
  }

  mu_mat <- matrix(0, ncol = length(w_ini), nrow = M)
  n_assets <- length(w_ini)

  w_optim_mat <- matrix(0, nrow = M, ncol = n_assets)
  colnames(w_optim_mat) <- asset_names

  port_means <- port_vols <- rep(0, M)

  if(sample_window){
    months_seq <- seq(date_ini, date_last %m+% -months(len_window), by = "months")
    for (i in 1:M){
      m_ini <- sample(months_seq, size = 1, replace = TRUE)
      m_last <- m_ini %m+% months(len_window)
      sample_i <- rets[paste(c(m_ini, m_last), collapse = '/')]
      mu_i <- apply(sample_i, 2, mean)
      mu_mat[i,] <- mu_i
      Sigma_i <- covar(sample_i)$cov_matrix
      if(!is.null(ineqUB)){lambda <- 0}
      obj_fun <- utility_fun(type = util_type, mu = mu_i, Sigma = Sigma_i, lambda = lambda, w_bench=w_bench, same_assets_bench = same_assets_bench)
      w_optim_mat[i,] <- optim_portfolio(w_ini = w_ini, fn = obj_fun, lb = lb, ub = ub,
                                         eqfun = sum_weights, eqB = 1, ineqfun = ineqfun, ineqLB = ineqLB, ineqUB = ineqUB, method = method, n.restarts = n.restarts, n.sim = n.sim,
                                         outer.iter = 10, inner.iter = 10)
      port_ret <- unlist(portfolio_return(w_optim_mat[i,], mu_all, Sigma_all$cov_matrix)[c('port_mean_ret', 'port_vol')])
      port_means[i] <- port_ret[1]*per
      port_vols[i] <- port_ret[2]*sqrt(per)
      if((i %% 100)==0){
        if(i==100){
          if(all(colSums(w_optim_mat[1:i,])==0)){break}
        }
        cat("\n iter:", i)
      }
    }
  }else{
    mu_regime <- mu
    for (i in 1:M){
      if(dyn_mu){mu_regime <- mu[sample(1:k, 1, replace = TRUE),]}
      sample_i <- mvrnorm(n = N , mu_regime, Sigma)
      mu_i <- apply(sample_i, 2, mean)
      mu_mat[i,] <- mu_i
      Sigma_i <- covar(sample_i)$cov_matrix
      if(!is.null(ineqUB)){lambda <- 0}
      obj_fun <- utility_fun(type = util_type, mu = mu_i, Sigma = Sigma_i, lambda = lambda, w_bench=w_bench, same_assets_bench = same_assets_bench)
      w_optim_mat[i,] <- optim_portfolio(w_ini = w_ini, fn = obj_fun, lb = lb, ub = ub,
                                         eqfun = sum_weights, eqB = 1, ineqfun = ineqfun, ineqLB = ineqLB, ineqUB = ineqUB,
                                         method = method, n.restarts = n.restarts, n.sim = n.sim,
                                         outer.iter = 10, inner.iter = 10)
      port_ret <- unlist(portfolio_return(w_optim_mat[i,], mu_all, Sigma_all$cov_matrix)[c('port_mean_ret', 'port_vol')])
      port_means[i] <- port_ret[1]*per
      port_vols[i] <- port_ret[2]*sqrt(per)
      if((i %% 100)==0){
        if(i==100){
          if(all(colSums(w_optim_mat[1:i,])==0)){break}
        }
        cat("\n iter:", i)
      }
    }
  }
  ind_sol <- apply(w_optim_mat, 1, sum) != 0
  if(!any(ind_sol)){
    if(is.null(w_ini)){
      w_sol <- rep(0, n_assets)
      names(w_sol) <- colnames(rets)
    }else{
      w_sol <- w_ini
    }
    w_optim_resamp <- w_optim_resamp_sd <- w_optim_lower <- w_optim_upper <- w_best_avg_ret <- w_avg_risk <- w_sol
  }else{
    port_means <- port_means[ind_sol]
    port_vols <- port_vols[ind_sol]
    mu_mat <- mu_mat[ind_sol,]
    w_optim_mat <- w_optim_mat[ind_sol,]
    w_optim_resamp <- apply(w_optim_mat, 2, mean)
    w_best_avg_ret <- w_optim_mat[which.max(as.numeric(apply(w_optim_mat %*% t(mu_mat),1,mean))),]

    #average solution when risk constraint.
    w_avg_risk <- NULL
    if(!is.null(ineqfun)){
      q_vol <- quantile(port_vols, q_sel)
      sel_port_vol <- port_vols >= q_vol

      #Option1: Average solutiom but selecting portafolios with higher volatility, i.e. closer to target.
      w_avg_risk <- apply(w_optim_mat[sel_port_vol,], 2, mean)

      #Option 2: Selecto ports. with higher vol. and more diversified. Adjust weigths such that we enforce solution to be equal to target.
      #port_div <- -apply(w_optim_mat**2, 1, sum)
      #q_diver <- quantile(port_div, q_sel)
      #sel_port_div <- port_div >= q_diver
      #sel_port <- sel_port_vol & sel_port_div

      #pond <- rep(1/sum(sel_port), sum(sel_port))
      #risk_target = function(pond)(as.numeric(sqrt((t(pond/sum(pond)) %*%w_optim_mat[sel_port,]) %*% Sigma %*% (t(w_optim_mat[sel_port,]) %*%(pond/sum(pond))))*sqrt(per)) - mean(port_vols[sel_port]))**2
      #pond_sol = nlminb(pond, risk_target,  lower = 0, upper = 1)$par
      #w_avg_risk <- apply(((pond_sol/sum(pond_sol)) %*% t(rep(1, ncol(w_optim_mat[sel_port,])))) * w_optim_mat[sel_port,], 2, sum)
    }


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
  }
  return(list(w_optim_resamp = w_optim_resamp, w_optim_resamp_sd = w_optim_resamp_sd, w_optim_lower = w_optim_lower, w_optim_upper = w_optim_upper, w_optim_matrix = w_optim_mat, w_best_avg_ret=w_best_avg_ret, w_avg_risk = w_avg_risk))
}
