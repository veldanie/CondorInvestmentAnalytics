#' Median of means.
#'
#' Median of means.
#' @param rets Returns series.
#' @param k Number of groups.
#' @return Median of means.
#' @export

median_of_means <- function(rets, k){
  mu <- apply(rets, 2, mean)

  if(length(k)==0){k <- round(as.numeric((tail(index(rets),1) - index(rets)[1])/365)/2)}
  n_rows <- nrow(rets)
  size <- ceiling(n_rows/k)
  sample_means <- matrix(0, nrow=k, ncol=ncol(rets))
  for (i in 1:k){
    if(i==k){
      sample_means[i,] <- apply(rets[(1+size*(i-1)):nrow(rets)],2,mean)
    }else{
      sample_means[i,] <- apply(rets[(1+size*(i-1)):(size*i)],2,mean)
    }
  }
  mom <- apply(sample_means,2,median)
  names(mom) <- colnames(rets)

  return(list(mu=mu, mom=mom))
}
