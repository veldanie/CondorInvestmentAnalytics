#' Median of means.
#'
#' Median of means.
#' @param rets Returns series.
#' @param k Number of groups.
#' @param pers Number of periods per group.
#' @return Median of means.
#' @export

median_of_means <- function(rets, k = NULL, size = NULL){
  mu <- apply(rets, 2, mean)
  n_rows <- nrow(rets)

  if(!is.null(size)){
    k <- ceiling(n_rows/size)
  }else if(is.null(size) && length(k)==0){
    k <- round(as.numeric((tail(index(rets),1) - index(rets)[1])/365)/2)
    size <- ceiling(n_rows/k)
  }else{
    size <- ceiling(n_rows/k)
  }
  sample_means <- matrix(0, nrow=k, ncol=ncol(rets))
  for (i in 1:k){
    if(i==k){
      sample_means[i,] <- apply(rets[(1+size*(i-1)):nrow(rets),,drop = FALSE],2,mean)
    }else{
      sample_means[i,] <- apply(rets[(1+size*(i-1)):(size*i),,drop = FALSE],2,mean)
    }
  }
  mom <- apply(sample_means,2,median)
  names(mom) <- colnames(rets)

  return(list(mu=mu, mom=mom))
}
