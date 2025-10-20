#' Black Litterman posterior mean and variance.
#'
#' Black Litterman posterior return incorporating views.
#' @param mu Expected equilibrium return.
#' @param q Views relative and absolute returns.
#' @param tau Factor to reduce equilibrium returns variance.
#' @param Sigma Covariance matrix.
#' @param P Views matrix.
#' @param omega Views variance matrix. Bu default: tau.diag(P.Sigma.t(P))/conf (Meucci (2009)).
#' @param conf Confidence level on the view. Value between 0 and 1. This value is mapped to [0, 10] with 0.5 correponding to conf = 1.
#' @return Returns.
#' @export

posterior_params <- function(mu, q, tau, Sigma, P, Omega = NULL, conf = 0.5, update_mu=FALSE){
  
  if (update_mu && !is.null(P) && all(rowSums(P)==1)){ # All are absolutr views
    bl_ret <- mu
    bl_ret[apply(P, 1, which.max)] <- q
    post_var <- Sigma      
  }else{
    if (sum(P!=0) == 0 | is.null(P)){bl_ret = mu; post_var = Sigma
    }else{
      if(is.null(Omega)){
        conf <- (1-conf)/conf
        Omega <- diag(tau * diag(P%*% Sigma %*% t(P)), nrow = nrow(P), ncol = nrow(P))*conf
      }
      omega_inv <- solve(Omega)
      tSigma_inv <- solve(tau * Sigma)
      post_ret <- solve(tSigma_inv + t(P)%*%omega_inv%*%P)%*%(tSigma_inv %*% mu+t(P) %*% omega_inv %*% q)
      bl_ret <- as.vector(post_ret)
      names(bl_ret) <- rownames(post_ret)
      
      post_var <- (1+tau)*Sigma - tau^2 * Sigma %*% t(P) %*% solve(P %*% (tau*Sigma) %*% t(P) + Omega) %*% P %*% Sigma
    }
  }
  
  return(list(post_ret = bl_ret, post_var = post_var))
}
