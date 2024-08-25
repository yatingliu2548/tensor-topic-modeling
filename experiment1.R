##### Synthetic experiments
library(VGAM)
library("nnTensor")
setwd("~/Documents/tensor-topic-modeling/")
source("data_generation.R")
source("compute_error_stats.r")
source("utils_alternative_methods.r")
set.seed(1234)
library(einsum)

K_p = 4 #### nb of topics
p = 50   #### nb of words
t = 10 ### nb of time points
K_t = 2  #### nb of time "topics"
n = 30 ### nb of samples
K_n = 3  #### nb of reviewer "personae"
nb_words_per_doc = 500

test <- synthetic_dataset_creation2(n, t, p, 
                                    K_n, K_t, K_p, 
                                    alpha_dirichlet = 1,
                                    n_anchors = 2,
                                    delta_anchor = 1,
                                    nb_words_per_doc = 500, seed = 1234)

D3_0 <- k_unfold(test$D0,3)
dim(D3_0)
sum(test$D0@data[1,1,])
apply(D3_0@data,2,sum)



test_NTD <- NTD(test$D,rank=c(K_n,K_t,K_p),algorithm="KL",init="NMF",num.iter=2)
hat_A_ntd = test_NTD$A 


A1_hat_ntd <- post_process_latent_ntd(hat_A_ntd$A1)
A2_hat_ntd <- post_process_latent_ntd(hat_A_ntd$A2)
A3_hat_ntd <- t(post_process_latent_ntd(t(hat_A_ntd$A3)))
hat_core = post_process_core_ntd(test_NTD$S)
error <- compute_error_stats(A1_hat_ntd, 
                             test$A1, A2_hat_ntd, 
                             test$A2, A3_hat_ntd, test$A3, 
                             hat_core, test$G, K_n, K_t, 
                             K_p,  n, t, p, 
                             nb_words_per_doc, 0, method="NTD", c())



error <- c()
#### This is probably not an optimal way of doing things'
D3 <- k_unfold(test$D, 3)
lda1 <- LDA(t(D3@data), k = K_t, control = list(seed = 1234), method = 'VEM')

error <- compute_error_stats(A1_hat_ntd, 
                             test$A1, A2_hat_ntd, 
                             test$A2, A3_hat_ntd, test$A3, 
                             hat_core, test$G, K_n, K_t, 
                             K_p,  n, t, p, 
                             nb_words_per_doc, 0, method="NTD", error)





error<- c()
error <- compute_error_stats(A1_hat_ntd, 
                             test$A1, A2_hat_ntd, 
                             test$A2, A3_hat_ntd, test$A3, 
                             hat_core, test$G, 
                             K_n, K_t, 
                             K_p,  n, t, p, 
                             nb_words_per_doc, 
                             0, method="NTD", error)


library(combinat)
library(clue)
l2_error <- function(A, B) {
  sqrt(sum((A - B)^2))
}

l1_error <- function(A, B) {
  sum(abs(A - B))
}

matrix_lp_distance <- function(A, B, 
                               lp=2){
  K <- dim(A)[2]
  if (lp ==2){
    error_matrix <- outer(seq_len(ncol(A)), seq_len(ncol(B)), 
                          Vectorize(function(i, j) l2_error(A[, i], B[, j])))
  }else{
    error_matrix <- outer(seq_len(ncol(A)), seq_len(ncol(B)), 
                          Vectorize(function(i, j) l1_error(A[, i], B[, j])))
  }
  # Find the optimal column permutation using the Hungarian algorithm
  permutation <- solve_LSAP(error_matrix) 
  B_permuted <- B[, permutation]
  
  if (lp ==2){
    error <- l2_error(A, B_permuted)
  }else{
    error <- l1_error(A, B_permuted)
  }
  
  return(list(error=error, permutation=permutation))
}


compute_error_stats<- function(hatA1=NULL,
                               A1, 
                               hatA2=NULL,
                               A2,
                               hatA3=NULL,
                               A3,
                               hatcore=NULL, core, K1, K2, K3, 
                               Q1, Q2, R, M, time, method, 
                               error){
  
  core_est=1
  if (is.null(hatA1)){
    errorl1_1=NA
    core_est=NULL
  }else{
    error_res=matrix_lp_distance(hatA1, A1, lp=1)
    errorl1_1=error_res$error
    perm1=error_res$permutation
    error_temp <- error_update(error=errorl1_1,K=K1,Q1=Q1,
                               R=R,Q2=Q2,M=M,
                               mode="A1",method=method,time=time)
    error=rbind(error,error_temp)
  }
  if (is.null(hatA2)){
    errorl1_2=NA
    core_est=NULL
  }else{
    error_res=matrix_lp_distance(hatA2, A2, lp=1)
    errorl1_2=error_res$error
    perm2=error_res$permutation
    error_temp <- error_update(error=errorl1_2,
                               K=K2, Q1=Q1, 
                               R=R, Q2=Q2, M=M,
                               mode="A2",method=method,
                               time=time)
    error=rbind(error,error_temp)
  }
  if (is.null(hatA3)){
    errorl1_3=NA
    core_est=NULL
  }else{
    error_res=matrix_lp_distance(hatA3, A3, lp=1)
    errorl1_3=error_res$error
    perm3=error_res$permutation
    error_temp <- error_update(error=errorl1_3, K=K3, Q1=Q1,
                               R=R, Q2=Q2, M=M,
                               mode="A3", method=method, time=time)
    error=rbind(error,error_temp)
  }
  if (is.null(hatcore)){
    error_core=NA
  }else{
    G = core[error_res1$permutation, 
             error_res2$permutation, 
             error_res3$permutation]
    error_core = sum(abs( G - hatcore))  
    error_temp <- error_update(error=error_core,
                               K=K3, Q1=Q1, 
                               R=R, Q2=Q2, M=M, 
                               mode="core", method=method, time=time)
    error=rbind(error,error_temp)
  }
  return(error)
}







