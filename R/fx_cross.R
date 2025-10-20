#' fx_cross.
#'
#' Cross fx rate .

#' @param base_curr Base currency.
#' @param ref_curr Reference currency.
#' @param curr_mkt_base Currency iso quote of base currency.
#' @param fx_base Base currency market fx rate.
#' @param curr_mkt_ref Currency iso quote of ref currency.
#' @param fx_ref Ref currency market fx rate.
#' @return Cross fx rate.
#' @export

fx_cross<-function(base_curr, ref_curr, curr_mkt_base, fx_base, curr_mkt_ref, fx_ref){
  if (any(base_curr== "USD") || any(ref_curr == "USD"))
    stop("Esta función aplica para pares de monedas donde la base y la referencia son diferentes a USD")

  mkt_codes=c(substr(curr_mkt_base,1,3),substr(curr_mkt_base,4,6),substr(curr_mkt_ref,1,3), substr(curr_mkt_ref,4,6))
  base_pos=match(base_curr,mkt_codes)
  ref_pos=match(ref_curr,mkt_codes)

  if (any(base_pos==1) & any(ref_pos==4)){
    cross=fx_base*fx_ref
  }
  if (any(base_pos==2) & any(ref_pos==4)){
    cross=(1/fx_base)*(fx_ref)
  }
  if (any(base_pos==2) & any(ref_pos==3)){
    cross=1/(fx_base*fx_ref)
  }
  return(cross)
}

