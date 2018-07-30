hw_simul_rt <-
function(date_ini, a , sigma, fwd_curve, M=1000){

  ft=fwd_curve[-1,2]#Forwards en los dias de simulacion
  simul_times=fwd_curve[-1,1]
  alpha=ft+(sigma^2/(2*a^2))*(1-exp(-a*c(0,simul_times)))^2
  ls=length(simul_times)
  rt=matrix(rep(0,M*(ls+1)), ncol=M)
  rt[1,]=fwd_curve[1,2]
  diff_simul_times=c(simul_times[1],diff(simul_times))

  for(i in 1:ls){
    mu=(rt[i,]-alpha[i])*exp(-a*diff_simul_times[i])+alpha[i+1]
    var=(sigma^2/(2*a))*(1-exp(-2*a*diff_simul_times[i]))
    rt[i+1,]=rnorm(M,mu,var)
  }
  return(rt[-1,])
}
