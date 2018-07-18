hw_logA<-function(t0, tn, a, sigma, cc_curve, rate_type="nominal", base=360){
  cc_days=cc_curve[,1]
  cc_rates=cc_curve[,2]
  d0=round(t0*base,0)
  dn=round(tn*base,0)

  rt0=approx(x=cc_days,y=cc_rates, xout=d0)$y
  rt0=ifelse(is.na(rt0),0,rt0)
  rtn=approx(x=cc_days,y=cc_rates, xout=dn)$y

  if (substr(rate_type, 1, 1) == "n") {
    if(t0!=0){rt0 = log(1 + rt0 * t0)/t0}
    rtn = log(1 + rtn * tn)/tn
  }
  if (substr(rate_type, 1, 1) == "e") {
    rt0 = log(1 + rt0)
    rtn = log(1 + rtn)
  }

  pt0=exp(-rt0*t0)
  ptn=exp(-rtn*tn)
  b=hw_B(t0=t0, tn=tn, a=a)

  a=log(ptn/pt0)-b*(-rt0)-(sigma^2/(4*a^3))*(exp(-a*tn)-exp(-a*t0))^2*(exp(2*a*t0)-1)
  return(a)
}
