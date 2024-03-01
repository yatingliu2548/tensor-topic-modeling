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

anchor_document<-function(A){
  result_matrix <- A
  ncolarrary=1:ncol(A)
  # For each column, find the index of the maximum value
  for (col in 1:ncol(A)) {
    max_index <- which.max(result_matrix[,col])
    # Set the corresponding position in the result matrix to 1
    result_matrix[max_index, col] <- 1
    result_matrix[max_index, ncolarrary[-col]] <- 0
  }

  return(result_matrix)
}
tensor_create<- function(G,A1,A2,A3){
  # First, multiply G by A along the first mode
  T1 <- ttm(G, A1, 1)

  # Then, multiply the result by B along the second mode (now mode 1 of T1)
  T2 <- ttm(T1, A2, 2)

  # Finally, multiply by C along the third mode (now mode 2 of T2)
  reconstructed_tensor <- ttm(T2, A3, 3)
  return(reconstructed_tensor)
}
matrization_tensor<-function(G,mode){
  tensordata=G@data
  if(mode==1){
    matrix_G=aperm(tensordata, c(1, 3, 2)) %>% as.vector() %>% matrix(nrow = dim(G)[1], byrow = FALSE)

  }
  if(mode==2){
    matrix_G=aperm(tensordata, c(2, 3, 1)) %>% as.vector() %>% matrix(nrow = dim(G)[2], byrow = FALSE)

  }
  if(mode==3){
    matrix_G=aperm(tensordata, c(3, 2, 1)) %>% as.vector() %>% matrix(nrow = dim(G)[3], byrow = FALSE)

  }
  return(matrix_G)
}
tensorization<-function(M,mode,Q1,Q2,Q3){

  if(mode ==1){
    reshaped_tensor <- array(as.vector(M), dim = c(Q1,Q3,Q2))

    tensor_back <- aperm(reshaped_tensor, c(1, 3, 2))
  }
  if(mode ==2){
    reshaped_tensor <- array(as.vector(M), dim = c(Q2,Q3,Q1))

    tensor_back <- aperm(reshaped_tensor, c(3, 1, 2))
  }
  if(mode ==3){
    reshaped_tensor <- array(as.vector(M), dim = c(Q3,Q2,Q1))

    tensor_back <- aperm(reshaped_tensor, c(3, 2, 1))
  }
  return(tensor_back)
}
synthetic_dataset_creation <- function(Q1, Q2,R, K1,K2,K3, alpha_dirichlet = 1,
                                       n_max_zipf=5 * 1e3,
                                       a_zipf=1,
                                       offset_zipf = 2.7,
                                       n_anchors=0, delta_anchor=1,
                                       M=500,
                                       seed=123, vary_by_topic=FALSE,
                                       sparsity = TRUE){

  set.seed(seed)
  A1 <- rdiric(Q1, rep(alpha_dirichlet, K1)) #Q1 X K1, rowSums(A1)=rep(1,10)
  A2<- rdiric(Q2, rep(alpha_dirichlet, K2))
  A1<-anchor_document(A1)
  A2<-anchor_document(A2)
  G=rdiric(K1*K2, rep(alpha_dirichlet, K3))
  G=array(G,dim=c(K1,K2,K3))
  G=as.tensor(G)
  #A1 <- t(A1) # colSums(A1)=1
  #A2 <- t(A2)
  p=R
  K=K3
  if (sparsity){
    if (n_anchors >0){
      A = matrix(0, nrow=K3, ncol=R)
      for (k in 1:K3){
        A[k, ((k-1)*n_anchors +1) : (k * n_anchors)] = delta_anchor
      }
      if(vary_by_topic){
        for (k in 1:K3){
          resample_index = sample(1: (R-n_anchors * K3), (R-n_anchors * K3))
          A[k,(K3 * n_anchors+1):R] <- sapply(1/(resample_index + offset_zipf)^a_zipf, function(u){rexp(1, u)})
        }

      }else{
        A[,(K3 * n_anchors+1):R ] <- sapply(1/(1: (R-n_anchors * K3) + offset_zipf)^a_zipf, function(u){rexp(K3, u)})
      }
      A = t(A)
      A[(K3 * n_anchors+1):R, ] = A[(K3 * n_anchors+1):R, ] %*% diag(((1 - apply(A[1:(K3 * n_anchors),],2,sum)) )/apply(A[(K3 * n_anchors+1):R, ], 2, sum))
    }else{
      A <- sapply(1/(1:R + offset_zipf)^a_zipf, function(u){rexp(K3, u)})
      A = t(A)
      A = A %*% diag(1/apply(A, 2, sum))
    }

  }else{
    if (n_anchors >0){

      A = matrix(0, nrow=K3, ncol=R)
      for (k in 1:K3){
        A[k, ((k-1)*n_anchors +1) : (k * n_anchors)] = delta_anchor
      }
      A[, (K3 * n_anchors+1):R ] <- matrix(runif(n=K3 * (R  - K3 * n_anchors)), nrow = K3)
      A = t(A)
      A[(K3 * n_anchors+1):R, ] = A[(K3 * n_anchors+1):R, ] %*% diag(((1 - apply(A[1:(K3 * n_anchors),],2,sum)) )/apply(A[(K3 * n_anchors+1):R, ], 2, sum))
    }else{
      A <- matrix(runif(n=K3 * (R)), ncol = K3)
      A = A %*% diag(1/apply(A, 2, sum))
    }
  }
  A3=A
  D0=tensor_create(G,A1,A2,A3)
  D3=matrization_tensor(D0,3)
  D=D0@data
  #D0 = A %*% W
  Y3 <- sapply(1:(Q1*Q2), function(i){rmultinom(1, M, D3[,i])})
  D3=D3[which(apply(Y3,1, sum) >0 ),]
  A3= A3[which(apply(Y3,1, sum) >0 ),]%*% diag(1/apply(A3[which(apply(Y3,1, sum) >0 ),], 2, sum))
  vocab =which(apply(Y3,1, sum) >0 )
  D=tensorization(D3,mode=3,Q1,Q2,length(vocab))
  Y=tensorization(Y3,mode=3,Q1,Q2,length(vocab))

  return(list(Y=Y,A1=A1,A2=A2,A3=A3, vocab=vocab, D=D))
}



#  data = synthetic_dataset_creation(Q1,Q2,R, K1,K2,K3, alpha_dirichlet=0.5, n_max_zipf=50000, a_zipf=1,n_anchors=2, delta_anchor=0.3, M=100, seed=123,offset_zipf=2.7,vary_by_topic=FALSE, sparsity = FALSE)
