#' Bootstrap means and volatilities of returns.
#'
#' Bootstrap means and volatilities of returns series.
#' @param rets Returns series.
#' @param len_window Length of the window in months.
#' @param period Returns period.
#' @param M Sample size.
#' @return Annualized means and volatilies expressed in percentage.
#' @export

bootstrap_rets_mean_vol <- function(rets, len_window = 60, period = "monthly", M = 1e3){

  freq <- switch(period, "daily" = 252, "monthly" = 12, "quarterly" = 4)
  mu_vec <- sd_vec <- rep(0, M)
  for (i in 1:M){
    m_ini <- sample(1:(length(rets)-len_window), size = 1, replace = TRUE)
    m_last <- m_ini + len_window - 1
    sample_i <- rets[m_ini:m_last]
    mu_vec[i] <- mean(sample_i)*freq*100
    sd_vec[i] <- mean(sample_i)*sqrt(freq)*100
  }

  rets_mean <- mean(rets)*100*freq
  bp_means <- boxplot(mu_vec, outline = TRUE, y_lab = 'Retornos', main = 'Boxplot', plot = FALSE)$stats
  summ_means <- round(c(rets_mean, bp_means[3,], bp_means[c(2,4),]),3)

  names(summ_means) <- c('Promedio', 'Mediana', 'Cuartil 1', 'Cuartil 3')

  rets_sd <- sd(rets)*100*sqrt(freq)
  bp_sd <- boxplot(sd_df, outline = TRUE, y_lab = 'Retornos', main = 'Boxplot', plot = FALSE)$stats
  summ_sd <- round(c(rets_sd, bp_sd[3,], bp_sd[c(2,4),]),3)

  summ_df <- cbind(summ_means, summ_sd)
  rownames(summ_df) <- c('Promedio', 'Mediana', 'Cuartil 1', 'Cuartil 3')
  colnames(summ_df) <- c('Promedio', 'Desv. Estándar')
  return(list(summ_df=summ_df, mu_vec=mu_vec, sd_vec=sd_vec))
}
