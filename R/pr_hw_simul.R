#' Simulated prices within the hull-white model,
#'
#' HW simulation.
#' @param w_ini Initial weights.
#' @param fn Objective function or list of objective function. By default it corresponds to the mean-var utility.
#' @param lb Lower bound.
#' @param ub Upper bound.
#' @return Optimal weights.
#' @export

pr_hw_simul <-function(date_ini, id_swaps, cf_matrix,
                       proy_leg1, proy_leg2,nom_leg1,nom_leg2,
                       pr_leg1,pr_leg2,rate_leg1, rate_leg2,
                       curr_leg1,curr_leg2,
                       disc_leg1,disc_leg2,str_leg1,str_leg2,
                       first_rate_float_leg1, first_rate_float_leg2,
                       prev_cf_dates_leg1, prev_cf_dates_leg2,
                       next_cf_dates_leg1, next_cf_dates_leg2,
                       curves_list, curves_curr,
                       spot_cop_id, spot_cop_array,
                       spot_id, spot_array,
                       rt_simul_list,
                       simul_days, a, sigma, drift_fx,
                       base=360, M=1000, base_currs=c("NZD", "AUD", "EUR", "GBP"), drift_adjust_usdcop=0) {


  ##Supuestos:
  ## Flujos de la pata variable y la pata fija ocurren en la misma fecha.
  ## No amortizaciones.
  ## Conteo de d?as act
  ##cf_dates: Data frame con id, cf_dates, ind_leg
  ##pay_fix es 1 o -1. (-1 implica pago fijo)
  if(any(is.na(id_swaps))){stop("No hay swaps insertados")}

  cf_id=cf_matrix[,2]

  cf_dates=as.Date(cf_matrix[,1],"%d/%m/%Y")
  #names(cf_dates)=cf_dates
  cf_leg=cf_matrix[,3]

  pos_cf_dates=unlist(lapply(id_swaps, function(x) which(x==cf_id)))

  cf_dates=cf_dates[pos_cf_dates];cf_id=cf_id[pos_cf_dates];cf_leg=cf_leg[pos_cf_dates]
  order_cf=order(cf_dates)

  cf_dates=cf_dates[order_cf]
  cf_id=cf_id[order_cf]
  cf_leg=cf_leg[order_cf]
  cf_code=paste0(cf_id,cf_leg)

  cf_fut_dates=cf_dates[cf_dates>date_ini]
  cf_id_fut=cf_id[cf_dates>date_ini]
  cf_leg_fut=cf_leg[cf_dates>date_ini]
  cf_code_fut=paste0(cf_id_fut,cf_leg_fut)

  days_next_cf_leg1=as.numeric(next_cf_dates_leg1-date_ini)
  days_next_cf_leg2=as.numeric(next_cf_dates_leg2-date_ini)

  pos_type_ois_leg1=which(rate_leg1=="IBRON")
  pos_type_ois_leg2=which(rate_leg2=="IBRON")

  if((length(pos_type_ois_leg1)+length(pos_type_ois_leg2))>0){
    cc_curve_leg1=curves_list[[unique(disc_leg1)]]
    cc_curve_leg2=curves_list[[unique(disc_leg2)]]

    first_rate_float_leg1[pos_type_ois_leg1]=((1+first_rate_float_leg1[pos_type_ois_leg1])*(1+approx_extrap(x=cc_curve_leg1[,1],y=cc_curve_leg1[,2],xout=days_next_cf_leg1[pos_type_ois_leg1])$y*days_next_cf_leg1[pos_type_ois_leg1]/base)-1)*360/as.numeric(next_cf_dates_leg1[pos_type_ois_leg1]-prev_cf_dates_leg1[pos_type_ois_leg1])
    first_rate_float_leg2[pos_type_ois_leg2]=((1+first_rate_float_leg2[pos_type_ois_leg2])*(1+approx_extrap(x=cc_curve_leg2[,1],y=cc_curve_leg2[,2],xout=days_next_cf_leg2[pos_type_ois_leg2])$y*days_next_cf_leg2[pos_type_ois_leg2]/base)-1)*360/as.numeric(next_cf_dates_leg2[pos_type_ois_leg2]-prev_cf_dates_leg2[pos_type_ois_leg2])
  }

  curves_id_disc=unique(c(disc_leg1,disc_leg2))
  curves_id_proy=unique(c(proy_leg1,proy_leg2))
  curves_id_proy=curves_id_proy[!is.na(curves_id_proy)]

  curves_id=unique(c(curves_id_disc,curves_id_proy))
  lcurv_disc=length(curves_id_disc)
  lcurv_proy=length(curves_id_proy)

  lcurv=length(curves_id)
  A = B=as.list(rep(NA, lcurv))
  names(A)=names(B)=curves_id

  lcurv_disc=length(curves_id_disc)
  code_names=pr_curv=as.list(rep(NA, lcurv_disc))
  names(pr_curv)=names(code_names)=curves_id_disc

  led=length(simul_days)
  lswaps=length(id_swaps)

  for(curv in curves_id_disc){

    proy_curves=unique(c(proy_leg1[which(disc_leg1==curv)],proy_leg2[which(disc_leg2==curv)]))
    proy_curves=proy_curves[!is.na(proy_curves)]

    pos_curv_leg1=which(disc_leg1==curv)
    pos_curv_leg2=which(disc_leg2==curv)

    cf_code_curv=c(paste0(id_swaps[pos_curv_leg1],1),paste0(id_swaps[pos_curv_leg2],2))
    cf_code_curv=cf_code_curv[cf_code_curv!="1" & cf_code_curv!="2"]
    code_names[[curv]]=cf_code_curv

    pos_cf_code_curv=unlist(sapply(cf_code_curv, function(x) which(x==cf_code_fut)))
    cf_fut_dates_curv=cf_fut_dates[pos_cf_code_curv]
    names(cf_fut_dates_curv)=cf_code_fut[pos_cf_code_curv]
    cf_fut_dates_curv=cf_fut_dates_curv[order(cf_fut_dates_curv)]

    affine=hw_affine(date_ini, cf_fut_dates_curv, a[[curv]] , sigma[[curv]], curves_list[[curv]], simul_days, base)
    A_curv=affine$A;
    B_curv=affine$B;
    A[[curv]]=A_curv
    B[[curv]]=B_curv

    #En caso que existan curvas de proyecci?n diferentes a la curva de descuento:
    if(any(curv!=proy_curves)){
      for(proy_curv in setdiff(proy_curves,curv)){
        proy_affine=hw_affine(date_ini, cf_fut_dates_curv, a[[proy_curv]] , sigma[[proy_curv]], curves_list[[proy_curv]], simul_days, base)
        proy_A=proy_affine$A;
        proy_B=proy_affine$B;
        A[[proy_curv]]=proy_A
        B[[proy_curv]]=proy_B
      }}


    lcf_curv=length(cf_fut_dates_curv)
    lcodes=length(cf_code_curv)
    pr_array=array(0, dim=c(led,M,lcodes))#Arreglos de precios de cada contrato en cada escenario y momento de tiempo

    payoff_fix_curv=matrix(rep(0,lcf_curv*lcodes),ncol=lcodes)

    colnames(payoff_fix_curv)=cf_code_curv##colnames(delta_curv)=cf_code_curv

    pos_fix_leg1=intersect(pos_curv_leg1, which(is.na(proy_leg1)))
    pos_fix_leg2=intersect(pos_curv_leg2, which(is.na(proy_leg2)))
    l_fix_leg1=length(pos_fix_leg1)
    l_fix_leg2=length(pos_fix_leg2)

    if((l_fix_leg1+l_fix_leg2)>0){
      id_fixed=c(id_swaps[pos_fix_leg1],id_swaps[pos_fix_leg2])

      cf_code_fixed=c(paste0(id_swaps[pos_fix_leg1],1),paste0(id_swaps[pos_fix_leg2],2))
      cf_code_fixed=cf_code_fixed[cf_code_fixed!="1" & cf_code_fixed!="2"]

      for(codei in cf_code_fixed){
        pos_codei=which(codei==cf_code_curv)

        legi=substr(codei,nchar(codei),nchar(codei)+1)
        id_swapi=substr(codei,1,nchar(codei)-1)
        pos_id=which(id_swapi==id_swaps)
        pos_cf_fixed=which(codei==names(cf_fut_dates_curv))
        lf=length(pos_cf_fixed)
        deltai=as.numeric(tail(diff(c(cf_dates[cf_code==codei]))/base,lf))

        str_fixed=str_leg2[pos_id]*(legi==2)+str_leg1[pos_id]*(legi==1)
        nom_fixed=nom_leg2[pos_id]*(legi==2)+nom_leg1[pos_id]*(legi==1)
        pr_fixed=pr_leg2[pos_id]*(legi==2)+pr_leg1[pos_id]*(legi==1)

        payoff_fix=str_fixed*deltai*pr_fixed*nom_fixed
        if(capital_exchange==TRUE){payoff_fix[lf]=payoff_fix[lf]+nom_fixed*pr_fixed}
        payoff_fix_curv[pos_cf_fixed, pos_codei]=payoff_fix
      }
    }

    pos_float_leg1=intersect(pos_curv_leg1, which(!is.na(proy_leg1)))
    pos_float_leg2=intersect(pos_curv_leg2, which(!is.na(proy_leg2)))
    l_float_leg1=length(pos_float_leg1)
    l_float_leg2=length(pos_float_leg2)

    rt_curv=rt_simul_list[[curv]]

    if((l_float_leg1+l_float_leg2)>0){
      id_float=c(id_swaps[pos_float_leg1],id_swaps[pos_float_leg2])
      cf_code_float=c(paste0(id_swaps[pos_float_leg1],1),paste0(id_swaps[pos_float_leg2],2))
      cf_code_float=cf_code_float[cf_code_float!="1" & cf_code_float!="2"]
    }

    for(i in 1:M){
      rt_sim=rt_curv[,i]
      affine_df=hw_affine_df(date_ini, A_curv, B_curv, rt_sim)

      ##Fix Leg:
      pr_code_fix=affine_df%*%payoff_fix_curv
      pr_code_float=matrix(rep(0,led*lcodes), nrow=led)

      if((l_float_leg1+l_float_leg2)>0){

        for(codei in cf_code_float){

          pos_codei=which(codei==cf_code_curv)

          legi=substr(codei,nchar(codei),nchar(codei)+1)
          id_swapi=substr(codei,1,nchar(codei)-1)
          pos_id=which(id_swapi==id_swaps)
          pos_cf_float=which(codei==names(cf_fut_dates_curv))
          lcf_fut_codei=length(pos_cf_float)
          deltai=as.numeric(tail(diff(c(cf_dates[cf_code==codei]))/base,lcf_fut_codei))

          str_float=str_leg2[pos_id]*(legi==2)+str_leg1[pos_id]*(legi==1)
          nom_float=nom_leg2[pos_id]*(legi==2)+nom_leg1[pos_id]*(legi==1)
          pr_float=pr_leg2[pos_id]*(legi==2)+pr_leg1[pos_id]*(legi==1)

          #Factores para proyecci?n de forwards:
          proy_curv=c(proy_leg1[pos_id],proy_leg2[pos_id])[as.numeric(legi)]

          if(proy_curv==curv){
            proy_affine_df=affine_df}
          if(proy_curv!=curv){
            proy_rt_curv=rt_simul_list[[proy_curv]]
            proy_rt_sim=proy_rt_curv[,i]
            proy_affine_df=hw_affine_df(date_ini, A[[proy_curv]], B[[proy_curv]], proy_rt_sim)
          }

          float_rates=matrix(rep(0,led*lcf_fut_codei), ncol=lcf_fut_codei)
          ##float_rates[,pos_cf_float[1]]=first_rate_float_leg2[pos_id]*(legi==2)+first_rate_float_leg1[pos_id]*(legi==1)
          float_rates[,1]=first_rate_float_leg2[pos_id]*(legi==2)+first_rate_float_leg1[pos_id]*(legi==1)

          #lcfi=length(pos_cf_fut)
          if(lcf_fut_codei>1){
            df2=proy_affine_df[,pos_cf_float[-1]]
            df1=proy_affine_df[,pos_cf_float[-lcf_fut_codei]]
            fwd=(df1/df2-1)/repr(t(deltai[-1]),led)
            fwd[is.na(fwd)|fwd<0]=0
            for(l in 1:(lcf_fut_codei-1)){fwd[fwd[,l]==0,l]=tail(fwd[fwd[,l]!=0,l],1)}

            float_rates[,-1]=fwd+str_float

          }
          pr_codei=((affine_df[,pos_cf_float]*float_rates)%*%deltai)*pr_float*nom_float
          if(capital_exchange==TRUE){pr_codei=pr_codei+nom_float*pr_float*affine_df[,tail(pos_cf_float,1)]}

          pr_code_float[,pos_codei]=pr_codei
        }
      }
      pr_code=pr_code_fix+pr_code_float
      pr_array[,i,]=pr_code
    }

    pr_curv[[curv]]=pr_array
  }

  s_array=array(0, dim=c(led,M,lswaps))
  if(lcurv_disc==1){
    pr_array_curv=pr_curv[[curves_id_disc]]
    codes=code_names[[curv]]
    id=unlist(sapply(codes, function(x) substr(x, 1,nchar(x)-1)))
    for (l in 1:lswaps){
      pos_id=which(id_swaps[l]==id)
      s_array[,,l]=t(colSums(aperm(pr_array_curv[,,pos_id])))
    }
  }
  if(lcurv_disc==2){
    pr_array_curv1=pr_curv[[curves_id_disc[1]]]
    pr_array_curv2=pr_curv[[curves_id_disc[2]]]

    codes1=code_names[[curves_id_disc[1]]]
    codes2=code_names[[curves_id_disc[2]]]

    id1=unlist(sapply(codes1, function(x) substr(x, 1,nchar(x)-1)))
    id2=unlist(sapply(codes2, function(x) substr(x, 1,nchar(x)-1)))

    curr1=curves_curr[curves_id_disc[1]]
    curr2=curves_curr[curves_id_disc[2]]

    if(any(curr1==c("COP","UVR"))|any(curr2==c("COP","UVR"))){
      spot1=spot_cop_array[,,which(paste0(curr1,"COP")==spot_cop_id)]
      spot2=spot_cop_array[,,which(paste0(curr2,"COP")==spot_cop_id)]
      if(paste0(curr1,"COP")=="USDCOP"){
        spot1=spot1*exp(-drift_fx[,"USDCOP"]*drift_adjust_usdcop)
      }
      if(paste0(curr2,"COP")=="USDCOP"){
        spot2=spot2*exp(-drift_fx[,"USDCOP"]*drift_adjust_usdcop)
      }


    }else{
      if(curr1=="USD"){
        spot1=1
      }else if(any(curr1==base_currs)){
        cross_id1=paste0(curr1,"USD")
        spot1=spot_array[,,which(cross_id1==spot_id)]
      }else{
        cross_id1=paste0("USD",curr1)
        spot1=1/spot_array[,,which(cross_id1==spot_id)]
      }
      if(curr2=="USD"){
        spot2=1
      }else if(any(curr2==base_currs)){
        cross_id2=paste0(curr2,"USD")
        spot2=spot_array[,,which(cross_id1==spot_id)]
      }else{
        cross_id2=paste0("USD",curr2)
        spot2=1/spot_array[,,which(cross_id2==spot_id)]
      }
    }
    for (l in 1:lswaps){
      pos_id1=which(id_swaps[l]==id1)
      pos_id2=which(id_swaps[l]==id2)
      pr_array_curv1_equiv=pr_array_curv1[,,pos_id1]*spot1
      pr_array_curv2_equiv=pr_array_curv2[,,pos_id2]*spot2
      s_array[,,l]=t(colSums(aperm(abind(pr_array_curv1_equiv,pr_array_curv2_equiv, along=3, force.array=TRUE))))
    }
  }

  return(pr_sim=s_array)
}
