library(VGAM)
library(tidyverse)
#library(ggtern)
library(tm)
library(topicmodels)
library(R.matlab)
library(tidyverse)
library(reticulate)
library(tidytext)
library(rTensor)
library(einsum)


source("tensor_operations.r")


anchor_document <- function(A, n_anchors=1, delta_anchor=1) {
  result_matrix <- A
  ncolarray = 1:ncol(A)
  # For each column, find the index of the maximum value
  for (col in 1:ncol(A)) {
    sorted_index <- sort(result_matrix[, col], decreasing = TRUE, index.return=TRUE)
    max_index = sorted_index$ix[1:n_anchors]
    # Set the corresponding position in the result matrix to 1
    result_matrix[max_index, col] <- delta_anchor
    result_matrix[max_index, ncolarray[-col]] <- 0

  }
  return(result_matrix)
}



#' Synthetic dataset creation
#'
#' This function creates a synthetic dataset for tensor topic modeling. 
#'
#' @param Q1 The number of documents (int )
#' @param Q2 The number of times points (int )
#' @param p The vocabulary size (int )
#' @return A list with the generated data Y, the latents A1, A2 and A3, the core G)
#' @examples
#' sum_nums(4, 5)
#' 
#' 
synthetic_dataset_creation <- function(Q1, Q2,
                                       p, K1, K2, K3, alpha_dirichlet = 1,
                                       n_max_zipf = 5 * 1e3,
                                       a_zipf = 1,
                                       offset_zipf = 2.7,
                                       n_anchors = 0,
                                       delta_anchor = 1,
                                       N = 500,
                                       seed = 123, vary_by_topic=FALSE,
                                       sparsity = TRUE){
  ##############

  set.seed(seed)
  A1 <- rdiric(Q1, rep(alpha_dirichlet, K1)) #Q1 X K1, rowSums(A1)=rep(1,10)
  
  A2 <- rdiric(Q2, rep(alpha_dirichlet, K2))
  ### Make sure that there is an anchor in the set
  A1 <- anchor_document(A1)  
  A2 <- anchor_document(A2)
  G <- rdiric(K1 * K2, rep(alpha_dirichlet, K3))
  G <- array(G, dim = c(K1, K2, K3))
  G <- as.tensor(G)
  ### Generate the topic matrix A3
  if (sparsity){
    if (n_anchors > 0){
      A = matrix(0, nrow=K3, ncol=p)
      for (k in 1:K3){
        A[k, ((k-1) * n_anchors + 1) : (k * n_anchors)] = delta_anchor
      }
      for (k in 1:K3){
        resample_index = sample(1: (p - n_anchors * K3), (p - n_anchors * K3))
        A[k,(K3 * n_anchors+1):p] <- sapply(1/(resample_index + offset_zipf)^a_zipf, function(u){rexp(1, u)})
      }

      A = t(A)
      A[(K3 * n_anchors+1):p, ] = A[(K3 * n_anchors+1):p, ] %*% diag(((1 - apply(A[1:(K3 * n_anchors),],2,sum)) )/ apply(A[(K3 * n_anchors+1):p, ], 2, sum))
    }else{
      A <- sapply(1/(1:p + offset_zipf)^a_zipf, function(u){rexp(K3, u)})
      A = t(A)
      A = A %*% diag(1/apply(A, 2, sum))
    }

  }else{
    if (n_anchors > 0){
      A = matrix(0, nrow = K3, ncol=p)
      for (k in 1:K3){
        A[k, ((k-1) * n_anchors + 1) : (k * n_anchors)] = delta_anchor
      }
      A[, (K3 * n_anchors+1):p] <- matrix(runif(n=K3 * (p  - K3 * n_anchors)), nrow = K3)
      A = t(A)
      A[(K3 * n_anchors+1):p, ] = A[(K3 * n_anchors+1):p, ] %*% diag(((1 - apply(A[1:(K3 * n_anchors),],2,sum)) )/apply(A[(K3 * n_anchors+1):p, ], 2, sum))
    }else{
      A <- matrix(runif(n = K3 * p), ncol = K3)
      A = A %*% diag(1/apply(A, 2, sum))
    }
  }
  A3 <- A
  D0 <- create_tensor(G, A1, A2, A3)
  D3 <- matricization(D0, 3)
  D <- D0@data
  #D0 = A %*% W
  Y3 <- sapply(1:(Q1 * Q2), function(i){rmultinom(1, N, D3[,i])})
  D3  <- D3[which(apply(Y3,1, sum) >0),]
  Y3  <- Y3[which(apply(Y3,1, sum) >0 ),]
  A3  <- A3[which(apply(Y3,1, sum) >0 ),]%*% diag(1/apply(A3[which(apply(Y3,1, sum) >0 ),], 2, sum))
  vocab   <-  which(apply(Y3,1, sum) >0 )
  D <- tensorization(D3,mode=3,Q1,Q2, length(vocab))
  Y <- tensorization(Y3,mode=3,Q1,Q2, length(vocab))
  return(list(D=Y, A1=A1, A2=A2, A3=A3, vocab=vocab, D0=D, G=G))
}



synthetic_dataset_creation_uniform <- function(Q1, Q2,
                                       p, K1, K2, K3, alpha_dirichlet = 1,
                                       n_max_zipf = 5 * 1e3,
                                       a_zipf = 1,
                                       offset_zipf = 2.7,
                                       n_anchors = 0,
                                       delta_anchor = 1,
                                       N = 500,
                                       seed = 123, vary_by_topic=FALSE,
                                       sparsity = TRUE){
  ##############
  
  set.seed(seed)
  A1 <- rdiric(Q1, rep(alpha_dirichlet, K1)) #Q1 X K1, rowSums(A1)=rep(1,10)
  A2 <- rdiric(Q2, rep(alpha_dirichlet, K2))
  A1 <- anchor_document(A1)  ### makes sure that there is an anchor in the set
  A2 <- anchor_document(A2)
  G <- array(dim=c(K1, K2, K3))
  for (k in 1:K3){
    G[,,k] = rdiric(1, rep(alpha_dirichlet, K1*K2))
  }
  #G <- rdiric(K1 * K2, rep(alpha_dirichlet, K3))
  #G <- array(G, dim = c(K1, K2, K3))
  G <- as.tensor(G)
  if (sparsity){
    if (n_anchors > 0){
      A = matrix(0, nrow=K3, ncol=p)
      for (k in 1:K3){
        A[k, ((k-1) * n_anchors + 1) : (k * n_anchors)] = delta_anchor
      }
      if(vary_by_topic){
        for (k in 1:K3){
          resample_index = sample(1: (p - n_anchors * K3), (p - n_anchors * K3))
          A[k,(K3 * n_anchors+1):p] <- sapply(1/(resample_index + offset_zipf)^a_zipf, function(u){rexp(1, u)})
        }
        
      }else{
        A[,(K3 * n_anchors+1):p] <- sapply(1/(1: (p-n_anchors * K3) + offset_zipf)^a_zipf, function(u){rexp(K3, u)})
      }
      A = t(A)
      A[(K3 * n_anchors+1):p ] = A[(K3 * n_anchors+1):p, ] %*% diag(((1 - apply(A[1:(K3 * n_anchors),],2,sum)) )/apply(A[(K3 * n_anchors+1):p, ], 2, sum))
    }else{
      A <- sapply(1/(1:p + offset_zipf)^a_zipf, function(u){rexp(K3, u)})
      A = t(A)
      A = A %*% diag(1/apply(A, 2, sum))
    }
    
  }else{
    if (n_anchors > 0){
      
      A = matrix(0, nrow = K3, ncol=p)
      for (k in 1:K3){
        A[k, ((k-1) * n_anchors + 1) : (k * n_anchors)] = delta_anchor
      }
      A[, (K3 * n_anchors+1):p] <- matrix(runif(n=K3 * (p  - K3 * n_anchors)), nrow = K3)
      A = t(A)
      A[(K3 * n_anchors+1):p, ] = A[(K3 * n_anchors+1):p, ] %*% diag(((1 - apply(A[1:(K3 * n_anchors),],2,sum)) )/apply(A[(K3 * n_anchors+1):p, ], 2, sum))
    }else{
      A <- matrix(runif(n = K3 * p), ncol = K3)
      A = A %*% diag(1/apply(A, 2, sum))
    }
  }
  
  A3 <- A
  D0 <- tensor_create(G, A1, A2, A3)
  D3 <- matrization_tensor(D0, 3)
  D <- D0@data
  #D0 = A %*% W
  Y3 <- sapply(1:(Q1 * Q2), function(i){rmultinom(1, N, D3[,i])})
  D3  <- D3[which(apply(Y3,1, sum) >0),]
  Y3  <- Y3[which(apply(Y3,1, sum) >0 ),]
  A3  <- A3[which(apply(Y3,1, sum) >0 ),]%*% diag(1/apply(A3[which(apply(Y3,1, sum) >0 ),], 2, sum))
  vocab   <-  which(apply(Y3,1, sum) >0 )
  D <- tensorization(D3,mode=3,Q1,Q2, length(vocab))
  Y <- tensorization(Y3,mode=3,Q1,Q2, length(vocab))
  return(list(D=Y, A1=A1, A2=A2, A3=A3, vocab=vocab, D0=D, G=G))
}


synthetic_dataset_creation2 <- function(n, t, p, 
                                        K_n, K_t, K_p, alpha_dirichlet = 1,
                                        n_anchors = 0,
                                        delta_anchor = 1,
                                        nb_words_per_doc = 500,
                                        seed = 123){
  ##############
  
  set.seed(seed)
  core  <- rdiric(n = K_n * K_t, shape = rep(1, K_p), dimension=K_p)
  core <- array(core, dim = c(K_n, K_t, K_p))
  
  A1 <- rdiric(n=n, shape = rep(1, K_n), dimension=K_n)
  A2 <- rdiric(n=t, shape = rep(1, K_t), dimension=K_t)
  A3 <- t(rdiric(n=K_p, shape = rep(1, p), dimension=p))
  # apply(A1,1,sum)
  # apply(A2,1,sum)
  # apply(A3,2,sum)
  A1 <- anchor_document(A1, n_anchors=n_anchors, delta_anchor=delta_anchor)  ### makes sure that there is an anchor in the set
  A1 <- diag(1/apply(A1,1,sum)) %*% A1
  A2 <- anchor_document(A2, n_anchors=n_anchors, delta_anchor=delta_anchor) ### makes sure that there is an anchor in the set
  A2 <- diag(1/apply(A2,1,sum)) %*% A2
  A3 <- anchor_document(A3, n_anchors=n_anchors, delta_anchor=delta_anchor)
  A3 <- A3 %*% diag(1/apply(A3,2,sum))
  
  
  G <- rdiric(K_n * K_t, rep(alpha_dirichlet, K_p))
  G <- array(G, dim = c(K_n, K_t, K_p))
  
  data = einsum("ni, ijk -> njk", A1, G)
  data = einsum("Tj, njk -> nTk", A2, data)
  data = einsum("Rk, nTk -> nTR", A3, data)
  D0 = as.tensor(data)
  
  Y =array(0, dim= c(n, t, p))
  for (i in 1:n){
    for (j in 1:t){
      Y[i,j,] = rmultinom(size=nb_words_per_doc, n=1, prob = D0@data[i,j,])
    }
  }
  Y = as.tensor(Y)
  ### check whether some words are unused
  Y3 = k_unfold(Y, 3)
  selected_words <- which(apply(Y3@data, 1, sum) >0 )
  Y <- Y[, , selected_words]
  A3  <- A3[selected_words, ] %*% diag(1/ apply(A3[selected_words, ], 2, sum))
  return(list(D=Y, A1=A1, A2=A2, A3=A3, 
              D0=D0, G=G))
}
