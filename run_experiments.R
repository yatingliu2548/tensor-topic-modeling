## other methods
library(nnTensor)
library(tidyverse)
library(topicmodels)
library(tidytext)

source("algorithm.R")
source("bayesian.R")
source("NTD.R")


run_experiment<- function(data, K1, K2, K3, M, method, threshold=FALSE){
  A1 <- data$A1
  A2 <- data$A2
  A3 <- data$A3
  core <- data$G

  Y <- data$D
  Q1 <- dim(A1)[1]
  Q2 <- dim(A2)[1]
  #R_old = R
  R <- dim(A3)[1]
  
  if (method == "bayesian"){
    elapsed_time_bayes <- system.time({
      bayesian_res <- tryCatch(
        fit_bayesian_model(data$D, K1, K2, K3,
                           use_vb = TRUE),
        error = function(err) {
          # Code to handle the error (e.g., print an error message, log the error, etc.)
          paste0("Error occurred while running the Bayesian method ", R, " :", conditionMessage(err), "\n")
          # Return a default value or NULL to continue with the rest of the code
          return(NULL)
        }
      )     
    })["elapsed"]
    if (is.null(bayesian_res)==FALSE){
      A1_hat_df = bayesian_res$A1
      A2_hat_df = bayesian_res$A2
      A3_hat_df = bayesian_res$A3
      core_hat_df = bayesian_res$core
      time = elapsed_time_bayes
      
      #### convert into matrices
      
      A1_hat = A1_hat_df %>%
        pivot_wider(id_cols = "Id1", names_from = "Cluster1", values_from = "Probability")
      A1_hat = as.matrix(A1_hat[, 2:ncol(A1_hat)])
      A2_hat = A2_hat_df %>%
        pivot_wider(id_cols = "Id2", names_from = "Cluster2", values_from = "Probability")
      A2_hat = as.matrix(A2_hat[, 2:ncol(A2_hat)])
      A3_hat = A3_hat_df %>%
        pivot_wider(id_cols = "word", names_from = "Topic", values_from = "Probability")
      A3_hat = as.matrix(A3_hat[, 2:ncol(A3_hat)])
      core_hat = array(rep(0, K1 * K2 *K3),
                       dim=c(K1,K2,K3))
      for (k1 in 1:K1){
        for (k2 in 1:K2){
          for (k3 in 1:K3){
            core_hat[k1,k2,k3] = core_hat_df$Probability[which((core_hat_df$cluster1 == k1) & (core_hat_df$cluster2 == k2) & (core_hat_df$Topic == paste0("Topic ", k3)))]
          }
        }
      }
      
    }else{
      A1_hat = NULL
      A2_hat = NULL
      A3_hat = NULL
      core_hat = NULL
      time = elapsed_time_bayes
      A1_hat_df = NULL
      A2_hat_df = NULL
      A3_hat_df = NULL
      core_hat_df = NULL
    }
    return(list(A1 = A1_hat,
                A2 = A2_hat,
                A3 = A3_hat,
                core = core_hat,
                A1_df = A1_hat_df,
                A2_df = A2_hat_df,
                A3_df = A3_hat_df,
                core_hat_df = core_hat_df,
                time = time))
  }
  
  if (method == "LDA"){
    elapsed_time_lda <- system.time({
      lda_res <- tryCatch(
        fit_LDA(data$D, K1, K2, K3),
        error = function(err) {
          # Code to handle the error (e.g., print an error message, log the error, etc.)
          paste0("Error occurred while running the LDA method ", R, " :", conditionMessage(err), "\n")
          # Return a default value or NULL to continue with the rest of the code
          return(NULL)
        }
      )     
    })["elapsed"]
    
    if (is.null(lda_res)==FALSE){
      A1_hat_df = data.frame(lda_res$A1$What)
      colnames(A1_hat_df) = 1:K1
      A1_hat_df["Id1"] = 1:Q1
      A1_hat_df = A1_hat_df %>% pivot_longer(cols = -c('Id1'))
      colnames(A1_hat_df) = c("Id1", "Cluster1", "Probability")
      A2_hat_df = data.frame(lda_res$A2$What)
      colnames(A2_hat_df) = 1:K2
      A2_hat_df["Id2"] = 1:Q2
      A2_hat_df = A2_hat_df %>% pivot_longer(cols = -c('Id2'))
      colnames(A2_hat_df) = c("Id2", "Cluster2", "Probability")
      A3_hat_df = lda_res$A3 %>% pivot_longer(cols=-c("word"))
      colnames(A3_hat_df) = c("word", "Topic", "Probability")
      core_hat_df = data.frame(lda_res$core)
      colnames(core_hat_df) = 1:K3
      core_hat_df["cluster1"] = unlist(lapply(1:K1, function(x){rep(x, K2)}))
      core_hat_df["cluster2"] = unlist(lapply(1:K1, function(x){1:K2}))
      core_hat_df = core_hat_df %>% 
        pivot_longer(cols = -c("cluster1", "cluster2"))
      colnames(core_hat_df) = c("cluster1", "cluster2", "Topic", "Probability")
      time = elapsed_time_lda
      
      #### convert into matrices
      
      A1_hat = lda_res$A1$What
      A2_hat = lda_res$A2$What
      A3_hat = as.matrix(lda_res$A3[,2:ncol(lda_res$A3)])
      core_hat = array(rep(0, K1 * K2 *K3),
                       dim=c(K1,K2,K3))
      for (k1 in 1:K1){
        for (k2 in 1:K2){
          for (k3 in 1:K3){
            core_hat[k1,k2,k3] = lda_res$core[(k1 - 1) * K2 + k2, k3]
          }
        }
      }
      
    }else{
      A1_hat = NULL
      A2_hat = NULL
      A3_hat = NULL
      core_hat = NULL
      time = elapsed_time_lda
      A1_hat_df = NULL
      A2_hat_df = NULL
      A3_hat_df = NULL
      core_hat_df = NULL
    }
    return(list(A1 = A1_hat,
                A2 = A2_hat,
                A3 = A3_hat,
                core = core_hat,
                A1_df = A1_hat_df,
                A2_df = A2_hat_df,
                A3_df = A3_hat_df,
                core_hat_df = core_hat_df,
                time = time))
  }
  
  
  if (method == "STM"){
    elapsed_time_stm <- system.time({
      stm_res <- tryCatch(
        fit_stm_model(data$D, K1, K2, K3),
        error = function(err) {
          # Code to handle the error (e.g., print an error message, log the error, etc.)
          paste0("Error occurred while running the LDA method ", R, " :", conditionMessage(err), "\n")
          # Return a default value or NULL to continue with the rest of the code
          return(NULL)
        }
      )     
    })["elapsed"]
    
    if (is.null(stm_res)==FALSE){
      A1_hat_df = data.frame(stm_res$A1$What)
      colnames(A1_hat_df) = 1:K1
      A1_hat_df["Id1"] = 1:Q1
      A1_hat_df = A1_hat_df %>% pivot_longer(cols = -c('Id1'))
      colnames(A1_hat_df) = c("Id1", "Cluster1", "Probability")
      A2_hat_df = data.frame(stm_res$A2$What)
      colnames(A2_hat_df) = 1:K2
      A2_hat_df["Id2"] = 1:Q2
      A2_hat_df = A2_hat_df %>% pivot_longer(cols = -c('Id2'))
      colnames(A2_hat_df) = c("Id2", "Cluster2", "Probability")
      A3_hat_df = stm_res$A3 %>% pivot_longer(cols=-c("word"))
      colnames(A3_hat_df) = c("word", "Topic", "Probability")
      core_hat_df = data.frame(stm_res$core)
      colnames(core_hat_df) = 1:K3
      core_hat_df["cluster1"] = unlist(lapply(1:K1, function(x){rep(x, K2)}))
      core_hat_df["cluster2"] = unlist(lapply(1:K1, function(x){1:K2}))
      core_hat_df = core_hat_df %>% 
        pivot_longer(cols = -c("cluster1", "cluster2"))
      colnames(core_hat_df) = c("cluster1", "cluster2", "Topic", "Probability")
      time = elapsed_time_stm
      
      #### convert into matrices
      
      A1_hat = stm_res$A1$What
      A2_hat = stm_res$A2$What
      A3_hat = as.matrix(stm_res$A3[,2:ncol(stm_res$A3)])
      core_hat = array(rep(0, K1 * K2 *K3),
                       dim=c(K1,K2,K3))
      for (k1 in 1:K1){
        for (k2 in 1:K2){
          for (k3 in 1:K3){
            core_hat[k1,k2,k3] = stm_res$core[(k1 - 1) * K2 + k2, k3]
          }
        }
      }
      
    }else{
      A1_hat = NULL
      A2_hat = NULL
      A3_hat = NULL
      core_hat = NULL
      time = elapsed_time_stm
      A1_hat_df = NULL
      A2_hat_df = NULL
      A3_hat_df = NULL
      core_hat_df = NULL
    }
    return(list(A1 = A1_hat,
                A2 = A2_hat,
                A3 = A3_hat,
                core = core_hat,
                A1_df = A1_hat_df,
                A2_df = A2_hat_df,
                A3_df = A3_hat_df,
                core_hat_df = core_hat_df,
                time = time))
  }
  
  if (method =="NTD"){
    elapsed_timeNTD <- system.time({
      ntd_res <- tryCatch(
        fit_NTD(data$D, num.iter=10),
      error = function(err) {
        # Code to handle the error (e.g., print an error message, log the error, etc.)
        paste0("Error occurred while running NTD ", R, " :", conditionMessage(err), "\n")
        # Return a default value or NULL to continue with the rest of the code
        return(NULL)
      }
      )     
    })["elapsed"]
    if (is.null(ntd_res)==FALSE){
      print("finish NTD")
      A1_hat = ntd_res$A1
      A2_hat = ntd_res$A2
      A3_hat = ntd_res$A3
      
      A1_hat_df = NULL
      A2_hat_df = NULL
      A3_hat_df = NULL
      core_hat_df = NULL
      
      core_hat = ntd_res$core
      time = elapsed_timeNTD
      
    }else{
      A1_hat = NULL
      A2_hat = NULL
      A3_hat = NULL
      core_hat = NULL
      time = elapsed_timeNTD
      A1_hat_df = NULL
      A2_hat_df = NULL
      A3_hat_df = NULL
      core_hat_df = NULL
      
    }
    return(list(A1 = A1_hat,
                A2 = A2_hat,
                A3 = A3_hat,
                core = core_hat,
                A1_df = A1_hat_df,
                A2_df = A2_hat_df,
                A3_df = A3_hat_df,
                core_hat_df = core_hat_df,
                time = time))
    
  } 
    
   if (method =="TTM-HOSVD"){
    elapsed_timeOurs <- system.time({
      tmp<-tryCatch(
        score(data$D/M, normalization="TTM", method = "HOSVD", K1=K1, K2=K2, K3=K3, M=M,
              as.sparse = FALSE),
        error = function(err) {
          # Code to handle the error (e.g., print an error message, log the error, etc.)
          paste0("Error occurred while running Ours ", R, " :", conditionMessage(err), "\n")
          # Return a default value or NULL to continue with the rest of the code
          return(NULL)}
      )
    })["elapsed"]
    if (is.null(tmp)==FALSE){
      A1_hat = tmp$hatA1
      A2_hat = tmp$hatA2
      A3_hat = tmp$hatA3
      core_hat=tmp$hatcore
    }else{
      A1_hat = NULL
      A2_hat = NULL
      core_hat=NULL
    }
    return(list(A1 = A1_hat,
                A2 = A2_hat,
                A3 = A3_hat,
                core = core_hat,
                time = elapsed_timeOurs))
   }
  
  if (method =="TTM-HOOI"){
    elapsed_timeOurs <- system.time({
      tmp<-tryCatch(
        score(data$D/M, normalization="TTM", method = "HOOI", 
              K1=K1, K2=K2, K3=K3, M=M,
              as.sparse = FALSE),
        error = function(err) {
          # Code to handle the error (e.g., print an error message, log the error, etc.)
          paste0("Error occurred while running Ours ", R, " :", conditionMessage(err), "\n")
          # Return a default value or NULL to continue with the rest of the code
          return(NULL)}
      )
    })["elapsed"]
    if (is.null(tmp)==FALSE){
      A1_hat = tmp$hatA1
      A2_hat = tmp$hatA2
      A3_hat = tmp$hatA3
      core_hat=tmp$hatcore
    }else{
      A1_hat = NULL
      A2_hat = NULL
      A3_hat = NULL
      core_hat=NULL
    }
    return(list(A1 = A1_hat,
                A2 = A2_hat,
                A3 = A3_hat,
                core = core_hat,
                time = elapsed_timeOurs))
  }
  
  if (method =="TopicScore-HOSVD"){
    elapsed_timeOurs <- system.time({
      tmp<-tryCatch(
        score(data$D/M, normalization="TopicScore", method = "HOSVD", K1=K1, K2=K2, K3=K3, M=M,
              as.sparse = FALSE),
        error = function(err) {
          # Code to handle the error (e.g., print an error message, log the error, etc.)
          paste0("Error occurred while running Ours ", R, " :", conditionMessage(err), "\n")
          # Return a default value or NULL to continue with the rest of the code
          return(NULL)}
      )
    })["elapsed"]
    if (is.null(tmp)==FALSE){
      A1_hat = tmp$hatA1
      A2_hat = tmp$hatA2
      A3_hat = tmp$hatA3
      core_hat=tmp$hatcore
    }else{
      A1_hat = NULL
      A2_hat = NULL
      A3_hat = NULL
      core_hat=NULL
    }
    return(list(A1 = A1_hat,
                A2 = A2_hat,
                A3 = A3_hat,
                core = core_hat,
                time = elapsed_timeOurs))
  }
  print("Method not implemented")
  return(NULL)
}



update_error<- function(hatA1=NULL,A1,hatA2=NULL,A2,hatA3=NULL,A3,hatcore=NULL,
                        core, K1, K2, K3, Q1, Q2, R, M, time, method, error){
  core_est=1
  if (is.null(hatA1)){
    errorl1_1 =NA
    core_est = NULL
  }else{
    error_res = matrix_lp_distance(hatA1, A1, lp=1)
    errorl1_1 = error_res$error
    perm1 = error_res$permutation
    if (method=="TopicScore"){
      error_temp <- error_update(error=errorl1_1, K=K1, Q1=Q1, R=R, 
                                 Q2=Q2, M=M,
                                 mode="A1",method="SPOC",time=time)
      
    }else{
      error_temp <- error_update(error=errorl1_1,
                                 K=K1, Q1=Q1, R=R, Q2=Q2, M=M, 
                                 mode="A1",method=method,time=time)
      
    }
    error=rbind(error,error_temp)
  }
  
  if (is.null(hatA2)){
    errorl1_2 =NA
    core_est = NULL
  }else{
    error_res = matrix_lp_distance(hatA2, A2, lp=1)
    errorl1_2 = error_res$error
    perm2 = error_res$permutation
    if (method=="TopicScore"){
      error_temp <- error_update(error=errorl1_2, K=K1, Q1=Q1, R=R, Q2=Q2, M=M,
                                 mode="A2",method="SPOC",time=time)
      
    }else{
      error_temp <- error_update(error=errorl1_2,
                                 K=K2, Q1=Q1, R=R, Q2=Q2, M=M, 
                                 mode="A2",method=method,time=time)
      
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
    error_temp <- error_update(error=errorl1_3,
                               K=K3,Q1=Q1,R=R,Q2=Q2,M=M,
                               mode="A3", method=method, time=time)
    error=rbind(error,error_temp)
  }

  
  if (is.null(hatcore)){
    error_core=NA
  }else{
    hat_G=hatcore
    print("here")
    hatG3=matricization(hat_G, 3)
    print("here")
    G3=matricization(core, 3)
    G3_PER=t(perm3)%*% G3%*% kronecker(perm1, perm2)
    error_core = l1_error(G3_PER, hatG3)
    error_temp <- error_update(error=error_core,K=K3, Q1=Q1, R=R, Q2=Q2, M=M,
                               mode="core",
                               method=method,time=time)
    error=rbind(error,error_temp)
  }
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

error_update <- function(error,K,Q1,R,Q2,M=M,mode,method=NULL,time=NULL){
  error_temp <- data.frame(error=error,
                           K = K,
                           Q1=Q1,
                           R=R,
                           Q2=Q2,
                           M=M,
                           mode=mode,method=method,time=time)
  return(error_temp)
}