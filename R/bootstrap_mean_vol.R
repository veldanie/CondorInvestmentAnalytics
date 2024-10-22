#' Bootstrap means and volatilities.
#'
#' Bootstrap means and volatilities.
#' @param series Assets series.
#' @param len_window Length of the window in months.
#' @param period Returns period.
#' @param M Sample size.
#' @return Annualized volatilies expressed in percentage.
#' @export

bootstrap_mean_vol <- function(series, len_window = 60, period = "monthly", M = 1e3){
  rets <- returns(series, period = period)
  freq <- switch(period, 'daily' = 252, 'monthly' = 12, 'quarterly' = 4)

  date_ini <- index(rets)[1]
  date_last <- tail(index(rets), 1)
  months_seq <- seq(date_ini, date_last %m+% -months(len_window), by = "months")
  mu_df <- sd_df <- matrix(0, nrow = M, ncol = ncol(rets))
  colnames(mu_df) <- colnames(sd_df) <- colnames(series)
  for (i in 1:M){
    m_ini <- sample(months_seq, size = 1, replace = TRUE)
    m_last <- m_ini %m+% months(len_window)
    sample_i <- rets[paste(c(m_ini, m_last), collapse = '/')]
    mu_df[i,] <- apply(sample_i, 2, mean)*100*freq
    sd_df[i,] <- sqrt(diag(covar(sample_i)$cov_matrix))*100*sqrt(freq)
  }

  rets_mean <- apply(rets,2,mean)*100*freq
  bp_means <- boxplot(mu_df, outline = TRUE, y_lab = 'Retornos', main = 'Boxplot', plot = FALSE)$stats
  summ_means <- t(round(rbind(rets_mean, bp_means[3,, drop = FALSE], bp_means[c(2,4),, drop = FALSE]),3))
  colnames(summ_means) <- c('Promedio', 'Mediana', 'Cuartil 1', 'Cuartil 3')

  rets_sd <- apply(rets,2,sd)*100*sqrt(freq)
  bp_sd <- boxplot(sd_df, outline = TRUE, y_lab = 'Retornos', main = 'Boxplot', plot = FALSE)$stats
  summ_sd <- t(round(rbind(rets_sd, bp_sd[3,, drop = FALSE], bp_sd[c(2,4),, drop = FALSE]),3))
  colnames(summ_sd) <- c('Promedio', 'Mediana', 'Cuartil 1', 'Cuartil 3')
  return(list(summ_means=summ_means, summ_sd=summ_sd, mu_dist=mu_df, sd_dist=sd_df))
}
