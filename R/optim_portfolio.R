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
#' @param method Gradient descent (GD), GD with random initialization (RI), differential evolution (DE), Genetic Opt. using Derivative (GE) or Generalized Simulated Annealing (SA).
#' @param fixed Numeric index indicating parameters that stay fixed.
#' @param n.restars Number of solver restarts.
#' @param n.sim Random parameters for every restart of the solver.
#' @param type Type of objective function. absolute or relative.
#' @param itermax Maximum iteration (population generation) allowed.
#' @param NP Number of population members.
#' @param max.time Max time in seconds. Applied to SA.
#' @return Optimal weights.
#' @export

optim_portfolio <- function(w_ini, fn, lb, ub, eqfun, eqB, w_bench = NULL, lb_act = NULL, ub_act = NULL, ineqfun = NULL, ineqLB = NULL, ineqUB = NULL, method = "RI", fixed = NULL, n.restarts = 10, n.sim = 20000, type = 'absolute', itermax = 1000, NP = 100, max.time = 2){
  #objective function:
  n_fn <- length(fn)
  n_par <- length(w_ini)
  if (method == "GD"){n.restarts = n.sim = 1}
  # Random Initialization:
  if (method %in% c("GD", "RI") & n_fn == 1){

    sol <- gosolnp(pars = w_ini, fixed = fixed, fun = fn,
                   eqfun = eqfun, eqB = eqB, ineqfun = ineqfun, ineqLB = ineqLB, ineqUB = ineqUB, LB = lb, UB = ub, n.restarts = n.restarts, n.sim = n.sim)
    if(sol$convergence == 0){
      w <- sol$pars
    }else{
      w <- w_ini
      warning('Convergence not achived. The problem might not have solution. Please modify the parameters and constraints.')}
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

    if (type == 'absolute' | !all(names(w_ini) %in% names(w_bench))) {
      sol <- DEoptim(fn = fn, lower = lb, upper = ub, control = control_list)
      if(is.finite(sol$optim$bestval)){w <- sol$optim$bestmem/sum(sol$optim$bestmem)}else{w <- rep(0, n_par)}
    } else {
      if(is.null(w_bench)){stop('w_bench cannot be NULL. Please add a benchmark portfolio.')}
      lower_act <- -mapply(min, w_bench - lb, abs(lb_act))
      upper_act <- mapply(min, max(0, ub - w_bench), ub_act)

      sol <- DEoptim(fn = fn, lower = lower_act, upper = upper_act, control = control_list)
      if(is.finite(sol$optim$bestval)){
        w <- sol$optim$bestmem
        w[w > 0] <- abs(w[w > 0] * sum(w[w < 0]) / sum(w[w > 0]))
      }else{w <- rep(0, n_par)}
    }
    names(w) <- names(w_ini)
    if(!is.finite(sol$optim$bestval)){
      warning('Convergence not achived. The problem might not have solution. Please modify the parameters and constraints.')
    }
  }else if(method == 'GE'){
    # Genetic Optim. using Derivatives

    if (type == 'absolute' | !all(names(w_ini) %in% names(w_bench))) {
      sol <- genoud(fn = fn, nvar = length(lb), Domains = cbind(lb, ub))
      if(is.finite(sol$value)){w <- sol$par/sum(sol$par)}else{w <- rep(0, n_par)}
    } else {
      if(is.null(w_bench)){stop('w_bench cannot be NULL. Please add a benchmark portfolio.')}
      lower_act <- -mapply(min, w_bench - lb, abs(lb_act))
      upper_act <- mapply(min, max(0, ub - w_bench), ub_act)

      sol <- genoud(fn = fn, Domains = cbind(lower_act, upper_act))
      if(is.finite(sol$value)){
        w <- sol$par
        w[w > 0] <- abs(w[w > 0] * sum(w[w < 0]) / sum(w[w > 0]))
      }else{w <- rep(0, n_par)}
    }
    names(w) <- names(w_ini)
    if(!is.finite(sol$value)){
      warning('Convergence not achived. The problem might not have solution. Please modify the parameters and constraints.')
    }
  }else if(method == 'SA'){
    # Generalized Simulating Annealing

    if (type == 'absolute' | !all(names(w_ini) %in% names(w_bench))) {
      sol <- GenSA(par = w_ini, fn = fn, lower = lb, upper =  ub, control = list(max.time = max.time))
      if(is.finite(sol$value)){w <- sol$par/sum(sol$par)}else{w <- rep(0, n_par)}
    } else {
      if(is.null(w_bench)){stop('w_bench cannot be NULL. Please add a benchmark portfolio.')}
      lower_act <- -mapply(min, w_bench - lb, abs(lb_act))
      upper_act <- mapply(min, max(0, ub - w_bench), ub_act)

      sol <- GenSA(par = w_ini, fn = fn, lower = lower_act, upper = upper_act, control = list(max.time = max.time))
      if(is.finite(sol$value)){
        w <- sol$par
        w[w > 0] <- abs(w[w > 0] * sum(w[w < 0]) / sum(w[w > 0]))
      }else{w <- rep(0, n_par)}
    }
    names(w) <- names(w_ini)
    if(!is.finite(sol$value)){
      warning('Convergence not achived. The problem might not have solution. Please modify the parameters and constraints.')
    }
  }
  return(w)
}
