hw_obj<-function(par, lambda=0.94, base=360, dates, days, rates_mat, on_rate, rate_type="nominal"){
  #rates_mat: Matriz de tasas. Fila corresponde a tenors, columna a fechas. 
  a=par[1]
  sigma=par[2]
  sigmah=par[3]
  tmin=days[1]/base
  tn=days/360
  rates=rates_mat[,1]
  
  if (substr(rate_type, 1, 1) == "n") {
    rates_mat = log(1 + rates_mat * tn)/tn}
  on_rate=log(1 + on_rate * 1/base)*base/1
  if (substr(rate_type, 1, 1) == "e") {
    rates_mat = log(1 + rates_mat)
    on_rate = log(1 + on_rate)
  }
  
  rates=rates_mat[,1]
  ##Otros inputs:
  H=diag(lambda^(1:length(tn))*sigmah^2)
  H1=solve(H)#Inv de H
  dt=as.numeric(diff(dates)/base)
  times=c(0,cumsum(dt))
  lt=length(times)
  ld=length(days)
  #Inicio proceso:
  xt=c(1, days)/base
  yt=c(on_rate, rates)
  rt=c(0,approx(x=xt, y=yt, xout=times[-1])$y)
  ft=c(0,(rt[-1]*times[-1]-rt[-lt]*times[-lt])/dt)
  B=hw_B(0, tn, a)
  logAk=t(-sigma^2*(exp(-a*(times+repr(tn, lt)))-exp(-a*times))^2*(exp(2*a*times)-1)/(4*a^3))+B%*%t(ft)+repr(times*rt,ld)
  yk=rates_mat
  dk=B/tn
  sk=ft+sigma^2*(1-exp(-a*times))^2/(2*a^2)
  ek=-logAk/repc(tn,lt)+((B/tn)%*%t(times))
  
  #Inicio del ciclo:
  xk=0
  pk=1
  sum1=sum2=0
  for(i in c(1:(lt-1))){
    vk=yk[,i]-dk*xk-ek[,i]
    Fk=H+pk*(dk%*%t(dk))
    F1=H1-as.numeric(pk/(pk*t(dk)%*%H1%*%dk+1))*(H1%*%dk%*%t(dk)%*%H1)
    sum1=sum1+log(det(Fk),10)
    sum2=sum2+t(vk)%*%F1%*%vk
    
    #xk=as.numeric(xk+pk*(t(dk)%*%F1%*%vk))
    #pk=as.numeric(pk-pk*(t(dk)%*%F1%*%dk)*pk)  
    
    xk=exp(-a*dt[i])*xk+sigma*sqrt((1-exp(-2*a*dt[i]))/(2*a))*rnorm(1)
    pk=exp(-2*a*dt[i])*pk+sigma^2*(1-exp(-2*a*dt[i]))/(2*a)
  }
  res=sum(0.5*sum1+0.5*sum2)
  return(res)
}
