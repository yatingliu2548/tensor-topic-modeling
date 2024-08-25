

post_process_latent_ntd <-function(x){
  A1_hat_ntd = t(x)
  A1_hat_ntd[which(A1_hat_ntd<0)] = 0
  A1_hat_ntd <- apply(A1_hat_ntd,2,function(x) x/rowSums(A1_hat_ntd))
  return(A1_hat_ntd)
}

post_process_core_ntd <-function(hat_core){
    hat_core = hat_core@data
    K_n = dim(hat_core)[1]
    K_t = dim(hat_core)[2]
    for (i in 1:K_n){
      for(j in 1:K_t){
        hat_core[i,j,which(hat_core[i,j,]<0)] = 0
        hat_core[i,j,] <-  hat_core[i,j,]/sum( hat_core[i,j,])
    }
    }
  return(hat_core)
}

