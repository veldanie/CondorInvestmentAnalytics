
#' Portfolio values.
#'
#' Simulate portafolios values.
#' @param port_list portfolio list,
#' @param afil_dist Client portolio distribuion.
#' @param afil_cf Client cash flows.
#' @param series_start_per Series start period.
#' @return Portfolio simulation.
#' @export

port_simul <- function(port_list, capital, afil_dist, afil_cf, series_start_per = 12){
  n_port <- ncol(afil_dist)
  port_names <- colnames(afil_dist)
  N <- length(index(afil_dist))
  M <- ncol(port_list[[1]])
  port_val <- matrix(0, nrow = N, ncol = M)
  port_val[1,] <- capital
  #Arreglo de series de unidad de fondo ordenadas iniciando cuando el programa de PE ha avanzado
  port_units <- array(0, dim = c(N, M, n_port))
  for(l in 1:n_port){
    p_name <- port_names[l]
    port_units[,,l] <-   port_list[[p_name]][series_start_per:(series_start_per + N - 1),]
  }

  for (i in 1:(N-1)){
    port_val[i,] <- port_val[i,] + as.numeric(afil_cf[i])
    port_alloc <- (port_val[i,] %*% t(rep(1, n_port))) * (rep(1, M) %*% afil_dist[i])
    units_per_port <- port_alloc/port_units[i,,]
    port_val[i+1, ] <- rowSums(units_per_port * port_units[i+1,,])
  }
  port_dist <- as.vector(tail(port_val,1))

  port_mean <- mean(port_dist)
  port_sd <- sd(port_dist)
  port_rets <- (apply(port_val, 2, diff)-afil_cf[-1] %*% t(rep(1,M)))/port_val[-N,]

  return(list(port_val = port_val, port_rets = port_rets, port_dist = port_dist, port_mean = port_mean, port_sd = port_sd))
}
