#' Semiannual returns.
#'
#' Takes quarterly returnos and turn them into semiannualy.
#' @param rets xts quarterly returns.
#' @param type Return type.
#' @return Semiannual returns.
#' @export

quarterly_to_semiannualy <- function(rets, type='arithmetic'){
  if(substr(type,1,4)=='arit'){
    rets_cumprod <- apply(1+rets,2,cumprod)
  }else{
    rets_cumprod <- exp(apply(rets,2,cumsum))
  }
  nrets <- nrow(rets)
  pos_semi <- unique(c(seq(2,nrets,by = 2),nrets))
  nvar <- ncol(rets)
  rets_semi <- xts(rets_cumprod[pos_semi, 1]/c(1,rets_cumprod[pos_semi, 1][1:(length(pos_semi)-1)])-1, order.by = index(rets)[pos_semi])
  if(nvar > 1){
    for(i in 2:nvar){
      rets_i <- xts(rets_cumprod[pos_semi, i]/c(1,rets_cumprod[pos_semi, i][1:(length(pos_semi)-1)])-1, order.by = index(rets)[pos_semi])
      rets_semi <- merge.xts(rets_semi, rets_i)
    }
    colnames(rets_semi) <- colnames(rets)
  }
  return(rets_semi)
}
