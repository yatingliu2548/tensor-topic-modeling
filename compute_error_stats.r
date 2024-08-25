
library(combinat)
library(clue)


error2_A <- function(A, A_hat){
  K <- dim(A)[2]
  used <- rep(1,K)
  A_perm <- matrix(0,dim(A)[1],dim(A)[2])
  
  for (k in 1:K){
    dis <- colSums(abs(A-A_hat[,k]))*used
    index <- which(dis == min(dis))
    index <- index[1]
    A_perm[,k] <- A[,index]
    used[index] <- Inf
  }
  
  return(mean(colSums(abs(A_perm-A_hat))))
}

l2_error <- function(A, B) {
  sqrt(sum((A - B)^2))
}

l1_error <- function(A, B) {
  sum(abs(A - B))
}


matrix_lp_distance <- function(A, B, 
                               lp=2){
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
  if (lp == 2){
    error <- l2_error(A, B_permuted)
  }else{
    error <- l1_error(A, B_permuted)
  } 
  return(list(error=error, permutation=permutation))
}


error_update <- function(error, K, n, p, t, nb_words_per_doc,
                         mode, method = NULL,
                         time = NULL){
  error_temp <- data.frame("error" = error,
                           "K" = K,
                           "n" = n,
                           "p" = p,
                           "t" = t,
                           "nb_words_per_doc" = nb_words_per_doc,
                           "mode" = mode, 
                           "method" = method, 
                           "time" = time)
  return(error_temp)
}


compute_error_stats <- function(hatA1=NULL,
                                A1, 
                                hatA2=NULL,
                                A2,
                                hatA3=NULL,
                                A3,
                                hatcore=NULL, core, 
                                K_n, K_t, K_p, 
                                n, t, p, nb_words_per_doc, 
                                time, method, 
                                error=c()){
  if (is.null(hatA1)){
    errorl1_1=NA
    error_res1 = NULL
  }else{
    error_res1 = matrix_lp_distance(hatA1, A1, lp=1)
    errorl1_1 = error_res1$error
    error_temp <- error_update(error=errorl1_1, K=K_n, 
                               n=n,
                               p=p, t=t, nb_words_per_doc = nb_words_per_doc,
                               mode="A1",method=method, time=time)
    error <- rbind(error, error_temp)
  }
  if (is.null(hatA2)){
    errorl1_2 <- NA
    error_res2 = NULL
  }else{
    error_res2 <- matrix_lp_distance(hatA2, A2, lp=1)
    errorl1_2 <- error_res2$error
    error_temp <- error_update(error=errorl1_2,
                               K=K_t, n=n, 
                               p=p, t=t, nb_words_per_doc=nb_words_per_doc,
                               mode="A2",method=method,
                               time=time)
    error <- rbind(error, error_temp)
  }

  if (is.null(hatA3)){
    errorl1_3 = NA
    core_est = NULL
    error_res3 = NULL
  }else{
    error_res3 = matrix_lp_distance(hatA3, A3, lp=1)
    errorl1_3 = error_res3$error
    error_temp <- error_update(error=errorl1_3, K=K_p, n=n,
                               p=p, t=t, 
                               nb_words_per_doc = nb_words_per_doc,
                               mode="A3", method=method, time=time)
    error <- rbind(error, error_temp)
  }
  if (is.null(hatcore)){
    error_core = NA
  }else{
    G = core[error_res1$permutation, 
             error_res2$permutation, 
             error_res3$permutation]
    error_core = sum(abs(G - hatcore))  
    error_temp <- error_update(error=error_core,
                               K=K_p, n=n, 
                               p=p, t=t, 
                               nb_words_per_doc=nb_words_per_doc, 
                               mode="core", 
                               method=method, time=time)
    error <- rbind(error, error_temp)
  }
  return(error)
}