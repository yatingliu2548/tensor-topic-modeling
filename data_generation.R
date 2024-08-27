library(VGAM)
library(tidyverse)
library(rTensor)


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
                                       R, K1, K2, K3, alpha_dirichlet = 1,
                                       n_max_zipf = 5 * 1e3,
                                       a_zipf = 1,
                                       offset_zipf = 2.7,
                                       n_anchors = 0,
                                       delta_anchor = 1,
                                       M = 500,
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
  ### Generate the topic matrix A3
  if(delta_anchor  * n_anchors > 1 ){
    print("Delta anchor chosen too high. Reducing it.")
    delta_anchor = min(c(delta_anchor, 0.99)) / n_anchors
  }
  if (sparsity){
    if (n_anchors > 0){
      A = matrix(0, nrow=K3, ncol=R)
      for (k in 1:K3){
        A[k, ((k-1) * n_anchors + 1) : (k * n_anchors)] = delta_anchor
      }
      for (k in 1:K3){
          resample_index = sample(1: (R - n_anchors * K3), (R - n_anchors * K3))
          A[k,(K3 * n_anchors+1):R] <- sapply(1/(resample_index + offset_zipf)^a_zipf, function(u){rexp(1, u)})
      }

      A = t(A)
      A[(K3 * n_anchors+1):R,] = A[(K3 * n_anchors+1):R, ] %*% diag(((1 - apply(A[1:(K3 * n_anchors),],2,sum)) )/apply(A[(K3 * n_anchors+1):R, ], 2, sum))
    }else{
      A <- sapply(1/(1:R + offset_zipf)^a_zipf, function(u){rexp(K3, u)})
      A = t(A)
      A = A %*% diag(1/apply(A, 2, sum))
    }

  }else{
    if (n_anchors > 0){

      A = matrix(0, nrow = K3, ncol=R)
      for (k in 1:K3){
        A[k, ((k-1) * n_anchors + 1) : (k * n_anchors)] = delta_anchor
      }
      A[, (K3 * n_anchors+1):R] <- matrix(runif(n=K3 * (R  - K3 * n_anchors)), nrow = K3)
      A = t(A)
      A[(K3 * n_anchors+1):R, ] = A[(K3 * n_anchors+1):R, ] %*% diag(((1 - apply(A[1:(K3 * n_anchors),],2,sum)) )/apply(A[(K3 * n_anchors+1):R, ], 2, sum))
    }else{
      A <- matrix(runif(n = K3 * R), ncol = K3)
      A = A %*% diag(1/apply(A, 2, sum))
    }
  }

  A3 <- A
  D0 <- create_tensor(as.tensor(G), A1, A2, A3)
  D3 <- matricization(D0, 3)
  D <- D0@data

  Y3 <- sapply(1:(Q1 * Q2), function(i){rmultinom(1, M, D3[,i])})
  D3  <- D3[which(apply(Y3, 1, sum) >0),]
  Y3  <- Y3[which(apply(Y3, 1, sum) >0 ),]
  A3  <- A3[which(apply(Y3, 1, sum) >0 ),]%*% diag(1/apply(A3[which(apply(Y3,1, sum) >0 ),], 2, sum))
  vocab   <-  which(apply(Y3, 1, sum) >0 )
  D <- tensorization(D3, mode=3, Q1, Q2, length(vocab))
  Y <- tensorization(Y3, mode=3, Q1, Q2, length(vocab))
  return(list(Y=Y, A1=A1, A2=A2, A3=A3, vocab=vocab, D=D, G=G))
}


