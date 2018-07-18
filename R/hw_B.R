ahw_B<-function(t0, tn, a){
  b=(1-exp(-a*(tn-t0)))/a
  return(b)
}
