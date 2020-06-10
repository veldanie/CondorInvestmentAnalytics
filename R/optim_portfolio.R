#' Optimal portfolio.
#'
#' Optimal Portfolio.
#' @param w_ini Initial weights.
#' @param fn Objective function or list of objective function. By default it corresponds to the mean-var utility.
#' @param lb Lower bound.
#' @param ub Upper bound.
#' @param eqfun Equality constraint function returning vector.
#' @param eqB Equality constraints.
#' @param w_bench Benchmark weigths.
#' @param lb_act Lower bound active weight per asset
#' @param ub_act Upper bound active weight per asset
#' @param ineqfun Inequality constraint function returning vector.
#' @param ineqLB Inequality lower bound.
#' @param ineqUB Inequality upper bound.
#' @param method Gradient descent (GD), differential evolution (DE), Genetic Opt. using Derivative (GE), Generalized Simulated Annealing (SA) or Memetic with local search (MALS).
#' @param fixed Numeric index indicating parameters that stay fixed.
#' @param n.restars Number of solver restarts.
#' @param n.sim Random parameters for every restart of the solver.
#' @param type Type of objective function. absolute or relative.
#' @param itermax Maximum iteration (population generation) allowed.
#' @param NP Number of population members.
#' @param max.time Max time in seconds. Applied to SA.
#' @param outer.iter Outer Iter solnp parameter.
#' @param inner.iter Inner Iter solnp parameter.
#' @return Optimal weights.
#' @export

optim_portfolio <- function(w_ini, fn, lb, ub, eqfun, eqB, w_bench = NULL, lb_act = NULL, ub_act = NULL, ineqfun = NULL, ineqLB = NULL, ineqUB = NULL, method = "RI", fixed = NULL, n.restarts = 10, n.sim = 20000, type = 'absolute', itermax = 1000, NP = 100, max.time = 60, outer.iter=400, inner.iter=800){
  #objective function:
  n_fn <- length(fn)
  n_par <- length(w_ini)
  if (method == "GD" && is.null(ineqfun)){
    sol <- try(auglag(x0 = w_ini, fn = fn, lower = lb, upper = ub, heq = function(w) eqfun(w) - 1,
                  localsolver = c("SLSQP")), silent = TRUE)
    if(class(sol)!="try-error" && sol$convergence> 0 && !all(sol$par==w_ini)){
      w <- sol$par
      names(w) <- names(w_ini)
    }else{
      w <- w_ini#rep(0, n_par)
      warning('Convergence not achived. The problem might not have solution. Please modify the parameters and constraints.')
    }
  }else if (method == "GD" && !is.null(ineqfun)){
    sol <- try(auglag(x0 = w_ini, fn = fn, lower = lb, upper = ub,
                  hin = function(w) c(ineqfun(w) - ineqLB, ineqUB - ineqfun(w)), heq = function(w) eqfun(w) - 1,
                  localsolver = c("LBFGS")), silent = TRUE)
    if(class(sol)!="try-error" && sol$convergence>0 && !all(sol$par==w_ini)){
      w <- sol$par
      names(w) <- names(w_ini)
    }else{
      w <- w_ini #rep(0, n_par)
      warning('Convergence not achived. The problem might not have solution. Please modify the parameters and constraints.')
    }
  }else if (method == "RI"){# Random Initialization:
    sol <- solnp(pars = w_ini, fun = fn,
                 eqfun = eqfun, eqB = eqB, ineqfun = ineqfun, ineqLB = ineqLB, ineqUB = ineqUB, LB = lb, UB = ub,
                 control = list(outer.iter=outer.iter, inner.iter=inner.iter))
    if(sol$convergence == 0){
      w <- sol$pars
    }else{
      w <- w_ini #rep(0, n_par)
      warning('Convergence not achived. The problem might not have solution. Please modify the parameters and constraints.')
      }
  }else if (method == 'DE'){
    # Differential Evolution:
    control_list <- list(itermax = itermax, # maximum iteration (population generation) allowed.
                         p = 0.2,
                         NP = NP,       # number of population members
                         F = 0.7,
                         CR = 0.9,
                         strategy = 2,  # DE / rand / 1 / bin (classical strategy)
                         #storepopfrom = 1, # store intermediate populations from 1st gen
                         parallelType = 0, # use all available cores
                         trace = FALSE    # do not print out progress at each iteration
                         #initialpop= # Initial population
                         #packages = c("")), # load package for fn)
                         )

    if (type == 'absolute' | !all(names(w_ini) %in% names(w_bench)) | length(w_ini)!=length(w_bench)) {
      if(any(lb > ub)){w <- rep(0, n_par); names(w) <- names(w_ini); return(w)}
      sol <- DEoptim(fn = fn, lower = lb-1e-7, upper = ub+1e-7, control = control_list)
      if(sol$optim$bestval<1000){
        w <- sol$optim$bestmem-(sum(sol$optim$bestmem)-1)*(sol$optim$bestmem-lb)/sum(sol$optim$bestmem-lb)
        #w <- sol$optim$bestmem/sum(sol$optim$bestmem)
      }else{
        w <- w_ini #rep(0, n_par)
      }
    } else {
      if(is.null(w_bench)){stop('w_bench cannot be NULL. Please add a benchmark portfolio.')}

      lower_act <- -mapply(min, w_bench[names(w_ini)] - lb, abs(lb_act))
      upper_act <- mapply(min, sapply(ub - w_bench[names(w_ini)], max, 0), ub_act)

      if(any(lower_act > upper_act)){w <- rep(0, n_par); names(w) <- names(w_ini); return(w)}
      sol <- DEoptim(fn = fn, lower = lower_act-1e-7, upper = upper_act+1e-7, control = control_list)
      if(sol$optim$bestval<1000){
        w <- sol$optim$bestmem
        w <- w-sum(w)*(w-lower_act)/sum(w-lower_act)
        #w[w > 0] <- abs(w[w > 0] * sum(w[w < 0]) / sum(w[w > 0]))
      }else{w <- rep(0, n_par)}
    }
    names(w) <- names(w_ini)
    if(sol$optim$bestval==1000){
      warning('Convergence not achived. The problem might not have solution. Please modify the parameters and constraints.')
    }
  }else if(method == 'GE'){
    # Genetic Optim. using Derivatives

    if (type == 'absolute' | !all(names(w_ini) %in% names(w_bench)) | length(w_ini)!=length(w_bench)) {
      if(any(lb > ub)){w <- rep(0, n_par); names(w) <- names(w_ini); return(w)}
      sol <- genoud(fn = fn, nvar = length(lb), Domains = cbind(lb-1e-7, ub+1e-7), wait.generations=50)
      if(fn(sol$par)<1000){
        w <- sol$par-(sum(sol$par)-1)*(sol$par-lb)/sum(sol$par-lb)
        #w <- sol$par/sum(sol$par)
      }else{
        w <- w_ini #rep(0, n_par)
      }
    } else {
      if(is.null(w_bench)){stop('w_bench cannot be NULL. Please add a benchmark portfolio.')}
      lower_act <- -mapply(min, w_bench[names(w_ini)] - lb, abs(lb_act))
      upper_act <- mapply(min, sapply(ub - w_bench[names(w_ini)], max, 0), ub_act)

      sol <- genoud(fn = fn, Domains = cbind(lower_act, upper_act), wait.generations=50)
      if(fn(sol$par)<1000){
        w <- sol$par
        w <- w-sum(w)*(w-lower_act)/sum(w-lower_act)
        #w[w > 0] <- abs(w[w > 0] * sum(w[w < 0]) / sum(w[w > 0]))
      }else{w <- rep(0, n_par)}
    }
    names(w) <- names(w_ini)
    if(sol$value==1000){
      warning('Convergence not achived. The problem might not have solution. Please modify the parameters and constraints.')
    }
  }else if(method == 'SA'){
    # Generalized Simulating Annealing

    if (type == 'absolute' | !all(names(w_ini) %in% names(w_bench)) | length(w_ini)!=length(w_bench)) {
      if(any(lb > ub)){w <- rep(0, n_par); names(w) <- names(w_ini); return(w)}
      sol <- GenSA(par = w_ini, fn = fn, lower = lb-1e-7, upper =  ub+1e-7, control = list(max.time = max.time, nb.stop.improvement=100, verbose=TRUE, smooth=FALSE))
      if(sol$value<1000){
        #w <- sol$par/sum(sol$par)
        w <- sol$par-(sum(sol$par)-1)*(sol$par-lb)/sum(sol$par-lb)
      }else{
        if (type == 'relative'){
          w <- rep(0, n_par)
        }else{
          w <- w_ini
        }
      }
    } else {
      lower_act <- -mapply(min, w_bench[names(w_ini)] - lb, abs(lb_act))
      upper_act <- mapply(min, sapply(ub - w_bench[names(w_ini)], max, 0), ub_act)

      w_act_ini <- rep(0, n_par)
      names(w_act_ini) <- names(w_ini)
      sol <- GenSA(par = w_act_ini, fn = fn, lower = lower_act-1e-7, upper = upper_act+1e-7, control = list(max.time = max.time, nb.stop.improvement=10, verbose=TRUE, smooth=FALSE))
      if(sol$value<1000){
        w <- sol$par
        w <- w-sum(w)*(w-lower_act)/sum(w-lower_act)
        #w[w > 0] <- abs(w[w > 0] * sum(w[w < 0]) / sum(w[w > 0]))
      }else{w <- rep(0, n_par)}
    }
    names(w) <- names(w_ini)
    if(sol$value==1000){
      warning('Convergence not achived. The problem might not have solution. Please modify the parameters and constraints.')
    }
  }else if(method == 'MALS'){
    # Memetic with local search

    if (type == 'absolute' | !all(names(w_ini) %in% names(w_bench)) | length(w_ini)!=length(w_bench)) {
      if(any(lb > ub)){w <- rep(0, n_par); names(w) <- names(w_ini); return(w)}
      sol <- malschains(fn = fn, lower = lb-1e-7, upper =  ub+1e-7)
      if(sol$fitness<1000){
        w <- sol$sol-(sum(sol$sol)-1)*(sol$sol-lb)/sum(sol$sol-lb)
        #w <- sol$sol/sum(sol$sol)
      }else{
        w <- w_ini #rep(0, n_par)
      }
    } else {
      if(is.null(w_bench)){stop('w_bench cannot be NULL. Please add a benchmark portfolio.')}

      lower_act <- -mapply(min, w_bench[names(w_ini)] - lb, abs(lb_act))
      upper_act <- mapply(min, sapply(ub - w_bench[names(w_ini)], max, 0), ub_act)

      sol <- malschains(fn = fn, lower = lower_act, upper = upper_act)
      if(sol$fitness<1000){
        w <- sol$sol
        w <- w-sum(w)*(w-lower_act)/sum(w-lower_act)
        #w[w > 0] <- abs(w[w > 0] * sum(w[w < 0]) / sum(w[w > 0]))
      }else{w <- rep(0, n_par)}
    }
    names(w) <- names(w_ini)
    if(sol$fitness==1000){
      warning('Convergence not achived. The problem might not have solution. Please modify the parameters and constraints.')
    }
  }
  return(w)
}
