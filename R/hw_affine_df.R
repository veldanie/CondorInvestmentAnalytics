
hw_affine_df <- function(date_ini, A, B, rt_sim){

  lcf=dim(A)[2]
  df_mat=A*exp(-B*repc(rt_sim,lcf))
  df_mat[is.na(df_mat)]=0
  return(df_mat)

}
