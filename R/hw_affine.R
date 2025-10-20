hw_affine <-
function(date_ini, cf_dates, a , sigma, cc_curve, simul_days, base){
  #cf_dates: TODAS las fecha de cashflows de TODOS los contratos.
  #cc_curve: Curva de tasas continuas.
  simul_dates=date_ini+simul_days
  cc_rates=cc_curve[,2]
  cc_days=cc_curve[,1]
  r0=cc_rates[1]

  simul_rates=approx(x=cc_days,y=cc_rates,xout=simul_days)$y
  #simul_rates1=approx(x=cc_days,y=cc_rates,xout=simul_days+1)$y
  simul_times=simul_days/base
  #simul_times1=(simul_days+1)/base
  ls=length(simul_times)

  ##simul_times=simul_days[-length(simul_days)]/base
  ##simul_times1=(simul_days[-1])/base

  ##ft=(simul_rates1*simul_times1-(simul_rates*simul_times))*base;
  ft=simul_rates
  lcf=length(cf_dates)
  df=matrix(0, nrow=ls, ncol=lcf)

  cf_days=t(sapply(simul_dates, diff_dates, cf_dates))
  cf_days[cf_days<0]=NA
  cf_times=cf_days/base

  cf_days_t0=as.numeric(cf_dates-date_ini)

  p0t=exp(-simul_rates*simul_times)
  p0T=exp(-approx(x=cc_days, y=cc_rates, xout=cf_days_t0)$y*cf_days_t0/base)
  p0t_mat=repc(p0t,lcf)
  p0T_mat=repr(p0T,ls)

  B=(1-exp(-a*cf_times))/a
  simul_times_mat=repc(simul_times,lcf)

  ##Gener. de trayectorias de las rt y factores de descuento: Brigo y Mercurio
  ft_mat=repc(ft,lcf)
  A=p0T_mat/p0t_mat*exp(B*ft_mat-(sigma^2/(4*a))*(1-exp(-2*a*simul_times_mat))*B^2)

  fwd_curve=data.frame(c(1/base,simul_times), c(r0,ft))
  return(list(A=A,B=B, fwd_curve=fwd_curve))
}
