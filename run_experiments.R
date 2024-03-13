## other methods
library(nnTensor)
## run LDA

run_experiment<- function(data,K1,K2,K3,M,error){
  A1=data$A1
  A2=data$A2
  A3=data$A3
  core=data$G

  Y=data$Y
  Q1=dim(A1)[1]
  Q2=dim(A2)[1]
  R=dim(A3)[1]
  D=data$Y/M
  D3=matrization_tensor(Y,3)
  #if(method == "lda"){
    #D1=matrization_tensor(D,1)
    #D2=matrization_tensor(D,2)
   
    #lda1 <- LDA(t(D1), k = K1, control = list(seed = 1234), method = 'VEM')
    #lda2 <- LDA(D2, k = K2, control = list(seed = 1234), method = 'VEM')
    #lda3 <- LDA(t(D3), k = K3, control = list(seed = 1234), method = 'VEM')
    #ldacore <- LDA(t(as.vector(D1)), k = K1*K2*K3, control = list(seed = 1234), method = 'VEM')
    #ap_topics1 <- tidy(lda1, matrix = "beta")
    #hatA1 = exp(t(lda1@beta))
    #ap_topics2 <- tidy(lda2, matrix = "beta")
    #hatA2 = lda2@gamma
    #ap_topics3 <- tidy(lda3, matrix = "beta")
    
  #   elapsed_timeLDA <- system.time({
  #     hatcore_es=1
  #     lda<- tryCatch(
  #     LDA(t(D3), k = K3, control = list(seed = 1234),  method = "VEM"),
  #     error = function(err) {
  #       # Code to handle the error (e.g., print an error message, log the error, etc.)
  #       cat("Error occurred while running lda:", conditionMessage(err), "\n")
  #       # Return a default value or NULL to continue with the rest of the code
  #       return(NULL)
  #     })
  #     if (is.null(lda)==FALSE){
  #       hatA3 <- exp(t(lda@beta))
  #     }else{
  #       hatA3=NULL
  #       hatcore_es=NULL
  #     }
  #     
  #     lda<- tryCatch(
  #       LDA(matrization_tensor(Y,1), k = K1, control = list(seed = 1234),  method = "VEM"),
  #       error = function(err) {
  #         # Code to handle the error (e.g., print an error message, log the error, etc.)
  #         cat("Error occurred while running lda:", conditionMessage(err), "\n")
  #         # Return a default value or NULL to continue with the rest of the code
  #         return(NULL)
  #       })
  #     if (is.null(lda)==FALSE){
  #         hatA1 <-  lda@gamma
  #     }else{
  #       hatA1 =NULL
  #       hatcore_es=NULL
  #     }
  #     lda<- tryCatch(
  #       LDA(matrization_tensor(Y,2), k = K2, control = list(seed = 1234),  method = "VEM"),
  #       error = function(err) {
  #         # Code to handle the error (e.g., print an error message, log the error, etc.)
  #         cat("Error occurred while running lda:", conditionMessage(err), "\n")
  #         # Return a default value or NULL to continue with the rest of the code
  #         return(NULL)
  #       })
  #     if (is.null(lda)==FALSE){
  #       hatA2 <-  lda@gamma
  #     }else{
  #       hatA2 =NULL
  #       hatcore_es=NULL
  #     }
  #     #ap_topics <- tidy(lda, matrix = "beta")
  #     if (is.null(hatcore_es)==FALSE){
  #     hatA=list(hatA1,hatA2,hatA3)
  #     hatcore <- get_hat_core(Y=Y,A1=hatA1,A2=hatA2,A3=hatA3)
  #     }else{
  #       hatcore=NULL
  #     }
  #   })["elapsed"]
  #   error <- update_error(hatA1=hatA1,hatA2=hatA2,hatcore=hatcore,hatA3=hatA3,time=elapsed_timeLDA,method="LDA",A1=A1,A2=A2,A3=A3,core=core,K1=K1,K2=K2,K3=K3,Q1=Q1,Q2=Q2,R=R,M=M,error=error)
  # 
  # print("finish LDA")
  #   
  #}
  #else if (method =="NTD"){
    elapsed_timeNTD <- system.time({
      ntd_res<-tryCatch(
      NTD(D,rank=c(K1,K2,K3),algorithm="HALS",init="NMF",num.iter=2),
      error = function(err) {
        # Code to handle the error (e.g., print an error message, log the error, etc.)
        paste0("Error occurred while running NTD ", alpha, " :", conditionMessage(err), "\n")
        # Return a default value or NULL to continue with the rest of the code
        return(NULL)
      }
      )      #ap_topics <- tidy(lda, matrix = "beta")
    })["elapsed"]
    if (is.null(ntd_res)==FALSE){
      hatA_ntd=ntd_res$A
      Pi=t(hatA_ntd$A1)
      Pi <- pmax(Pi,matrix(0,dim(Pi)[1],dim(Pi)[2])) ### sets negative entries to 0
      temp <- rowSums(Pi)
      Pi <- apply(Pi,2,function(x) x/temp)
      hatA1=Pi
      Pi=t(hatA_ntd$A2)
      Pi <- pmax(Pi,matrix(0,dim(Pi)[1],dim(Pi)[2])) ### sets negative entries to 0
      temp <- rowSums(Pi)
      Pi <- apply(Pi,2,function(x) x/temp)
      hatA2=Pi
      Pi=t(hatA_ntd$A3)
      Pi <- pmax(Pi,matrix(0,dim(Pi)[1],dim(Pi)[2])) ### sets negative entries to 0
      temp <- rowSums(Pi)
      Pi <- apply(Pi,2,function(x) x/temp)
      hatA3=Pi
      hatcore=ntd_res$S
      G3=matrization_tensor(hatcore,3)
      G3 <- pmax(G3,matrix(0,dim(G3)[1],dim(G3)[2])) ### sets negative entries to 0
      temp <- colSums(G3)
      G3 <- apply(G3,1,function(x) x/temp)
      G3[is.na(G3)] <- 0
      hatcore=tensorization(G3,3,dim(hatcore)[1],dim(hatcore)[2],dim(hatcore)[3])

    error <- update_error(hatA1=hatA1,hatA2=hatA2,hatA3=hatA3,hatcore=hatcore,time=elapsed_timeNTD,method="NTD",A1=A1,A2=A2,A3=A3,core=core,K1=K1,K2=K2,K3=K3,Q1=Q1,Q2=Q2,R=R,M=M,error=error)
    }
    print("finish NTD")
  #}else if (method =="Ours"){
    elapsed_timeOurs <- system.time({
      tmp<-tryCatch(
        score(data$Y/M,normalize="Ours",K1=K1,K2=K2,K3=K3,M=M,as.sparse = FALSE),
        error = function(err) {
          # Code to handle the error (e.g., print an error message, log the error, etc.)
          paste0("Error occurred while running Ours ", alpha, " :", conditionMessage(err), "\n")
          # Return a default value or NULL to continue with the rest of the code
          return(NULL)}
      )
    })["elapsed"]
    if (is.null(tmp)==FALSE){
      hatA1=tmp$hatA1
      hatA2=tmp$hatA2
      hatA3=tmp$hatA3
      hatcore=tmp$hatcore
      error <- update_error(hatA1=hatA1,hatA2=hatA2,hatA3=hatA3,hatcore=hatcore,time=elapsed_timeOurs,method="Ours",A1=A1,A2=A2,A3=A3,core=core,K1=K1,K2=K2,K3=K3,Q1=Q1,Q2=Q2,R=R,M=M,error=error)
    }
    print("finish OURS")
    elapsed_timeHOSVD <- system.time({
      tmp<- tryCatch(
        score(data$Y/M,normalize="HOSVD",K1=K1,K2=K2,K3=K3,M=M,as.sparse = FALSE),
        error = function(err) {
          # Code to handle the error (e.g., print an error message, log the error, etc.)
          paste0("Error occurred while running HOSVD ", alpha, " :", conditionMessage(err), "\n")
          # Return a default value or NULL to continue with the rest of the code
          return(NULL)}
      )
        })["elapsed"]
    if (is.null(tmp)==FALSE){
      hatA1=tmp$hatA1
      hatA2=tmp$hatA2
      hatA3=tmp$hatA3
      hatcore=tmp$hatcore
    error <- update_error(hatA1=hatA1,hatA2=hatA2,hatA3=hatA3,hatcore=hatcore,time=elapsed_timeHOSVD,method="HOSVD",A1=A1,A2=A2,A3=A3,core=core,K1=K1,K2=K2,K3=K3,Q1=Q1,Q2=Q2,R=R,M=M,error=error)
    }
    print("finish HOSVD")
    elapsed_timeHOOI <- system.time({
      tmp<-tryCatch(
        score(data$Y/M,normalize="HOOI",K1=K1,K2=K2,K3=K3,M=M,as.sparse = FALSE),
        error = function(err) {
          # Code to handle the error (e.g., print an error message, log the error, etc.)
          paste0("Error occurred while running HOOI ", alpha, " :", conditionMessage(err), "\n")
          # Return a default value or NULL to continue with the rest of the code
          return(NULL)}
      )
        })["elapsed"]
    if (is.null(tmp)==FALSE){
      hatA1=tmp$hatA1
      hatA2=tmp$hatA2
      hatA3=tmp$hatA3
      hatcore=tmp$hatcore
    error <- update_error(hatA1=hatA1,hatA2=hatA2,hatA3=hatA3,hatcore=hatcore,time=elapsed_timeHOOI,method="HOOI",A1=A1,A2=A2,A3=A3,core=core,K1=K1,K2=K2,K3=K3,Q1=Q1,Q2=Q2,R=R,M=M,error=error)
    }
    print("finish HOOI")
  #}else if(method=="Tracy"){
    elapsed_timeTracy <- system.time({
      tmp<- tryCatch(
        score(data$Y/M,normalize="Tracy",K1=K1,K2=K2,K3=K3,M=M,as.sparse = FALSE),
        error = function(err) {
          # Code to handle the error (e.g., print an error message, log the error, etc.)
          paste0("Error occurred while running Tracy ", alpha, " :", conditionMessage(err), "\n")
          # Return a default value or NULL to continue with the rest of the code
          return(NULL)}
      )
        })["elapsed"]
    if (is.null(tmp)==FALSE){
      hatA1=tmp$hatA1
      hatA2=tmp$hatA2
      hatA3=tmp$hatA3
      hatcore=tmp$hatcore
    error <- update_error(hatA1=hatA1,hatA2=hatA2,hatA3=hatA3,hatcore = hatcore,time=elapsed_timeTracy,method="Tracy",A1=A1,A2=A2,A3=A3,core=core,K1=K1,K2=K2,K3=K3,Q1=Q1,Q2=Q2,R=R,M=M,error=error)
    }
    print("finish Tracy")
    
    # elapsed_timeOurs_iter <- system.time({
    #   Y_iter=D
    #   for(i in 1:10){
    #   tmp=score(Y_iter,normalize="Ours",K1=K1,K2=K2,K3=K3,M=M,as.sparse = FALSE)
    #   hatA1=tmp$hatA1
    #   hatA2=tmp$hatA2
    #   hatA3=tmp$hatA3
    #   hatcore=tmp$hatcore
    #   Y_iter=tensor_create(hatcore,hatA1,hatA2,hatA3)
    #   }
    # })["elapsed"]
    # error <- update_error(hatA1=hatA1,hatA2=hatA2,hatA3=hatA3,hatcore=hatcore,time=elapsed_timeOurs_iter,method="Ours_iter_10",A1=A1,A2=A2,A3=A3,core=core,K1=K1,K2=K2,K3=K3,Q1=Q1,Q2=Q2,R=R,M=M,error=error)
    # print("finish Ours iter")
    
    
  #}
    return(error)
}



update_error<- function(hatA1=NULL,A1,hatA2=NULL,A2,hatA3=NULL,A3,hatcore=NULL,core,K1,K2,K3,Q1,Q2,R,M,time,method,error){
 
  core_est=1
  if (is.null(hatA1)){
  
    errorl1_1=NA
    core_est=NULL
  }else{
    error_res=matrix_lp_distance(hatA1, A1, lp=1)
    errorl1_1=error_res$error
    perm1=error_res$permutation
    if (method=="Tracy"){
      error_temp <- error_update(error=errorl1_1,K=K1,Q1=Q1,R=R,Q2=Q2,M=M,mode="A1",method="Olga",time=time)
      
    }else{
      error_temp <- error_update(error=errorl1_1,K=K1,Q1=Q1,R=R,Q2=Q2,M=M,mode="A1",method=method,time=time)
      
    }
    error=rbind(error,error_temp)
  }
  if (is.null(hatA2)){
    errorl1_2=NA
    core_est=NULL
  }else{
    error_res=matrix_lp_distance(hatA2, A2, lp=1)
    errorl1_2=error_res$error
    perm2=error_res$permutation
    if (method=="Tracy"){
      error_temp <- error_update(error=errorl1_2,K=K2,Q1=Q1,R=R,Q2=Q2,M=M,mode="A2",method="Olga",time=time)
      
    }else{
      error_temp <- error_update(error=errorl1_2,K=K2,Q1=Q1,R=R,Q2=Q2,M=M,mode="A2",method=method,time=time)
      
    }
    error=rbind(error,error_temp)
  }
  if (is.null(hatA3)){
    errorl1_3=NA
    core_est=NULL
  }else{
    error_res=matrix_lp_distance(hatA3, A3, lp=1)
    errorl1_3=error_res$error
    perm3=error_res$permutation
    error_temp <- error_update(error=errorl1_3,K=K3,Q1=Q1,R=R,Q2=Q2,M=M,mode="A3",method=method,time=time)
    error=rbind(error,error_temp)
  }
  if (is.null(hatcore)){
    error_core=NA
  }else{
    hat_G=hatcore
    hatG3=matrization_tensor(hat_G,3)
    G3=matrization_tensor(core,3)
    G3_PER=t(perm3)%*% G3%*% kronecker(perm1,perm2)
    error_core=l1_error(G3_PER,hatG3)
    if (method=="Tracy"){
      error_temp <- error_update(error=error_core,K=K3,Q1=Q1,R=R,Q2=Q2,M=M,mode="core",method="Tracy_Olga",time=time)
      
    }else{
      error_temp <- error_update(error=error_core,K=K3,Q1=Q1,R=R,Q2=Q2,M=M,mode="core",method=method,time=time)
    }
    error=rbind(error,error_temp)
  }
  
 
  #ifelse(is.null(What), NA, matrix_lp_distance(What, W, lp=1)),
  #errorl1_2=ifelse(is.null(hatA2), NA, matrix_lp_distance(hatA2, A2, lp=1)),
  #errorl1_3=ifelse(is.null(hatA3), NA, matrix_lp_distance(hatA3, A3, lp=1)),
  
 return(error)

}



get_hat_core<- function(Y,A1,A2,A3){
  A=list(A1,A2,A3)
  # NA mask
  M_NA <- Y
  M_NA@data[] <- 1
  M_NA@data[which(is.na(Y@data))] <- 0
 
    M <- M_NA
  
  pM <- M
  # Pseudo count
  pseudocount=.Machine$double.eps
  Y@data[which(is.na(Y@data))] <- pseudocount
  Y <- .pseudocount(Y, pseudocount)
  S= recTensor(S=Y, A=A, idx=c(1,2,3),reverse=FALSE)
  X_bar <- recTensor(S=S, A=A, idx=c(1,2,3),reverse=TRUE)

  pM <- .pseudocount(M, pseudocount)
  
  numer <- pM * Y * X_bar^(2 - 1)
  denom <- pM * X_bar^2
  for (n in 1:3) {
    numer <- ttm(numer, t(A[[n]]), m = n)
    denom <- ttm(denom, t(A[[n]]), m = n)
  }
  S <- S * numer/denom
  return(S)
}
.positive <- function(X, thr = .Machine$double.eps){
  if (is(X)[1] == "matrix") {
    X[which(X < thr)] <- thr
  }
  else if (is(X)[1] == "Tensor") {
    X@data[which(X@data < thr)] <- thr
  }
  else if ("numeric" %in% is(X) && length(X) != 1) {
    X[which(X < thr)] <- thr
  }
  else if ("numeric" %in% is(X) && length(X) == 1) {
    X <- max(X, thr)
  }
  X
}
.pseudocount <- function(X, pseudocount = 1e-10){
  X@data[which(X@data == 0)] <- pseudocount
  X
}

