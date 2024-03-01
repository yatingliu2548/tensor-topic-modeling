library('rARPACK')
#library('Matrix')
library(roxygen2)
library(quadprog)

source("r/vertex_hunting_functions.R")
source("r/simplex_dist.R")
library(Matrix)


score <- function(D, K, scatterplot=FALSE, K0=NULL, m=NULL, M=NULL, threshold=FALSE,
                  Mquantile=0.00, VHMethod = 'SP', normalize="none",
                  alpha=0.5, max_K=150, returnW=FALSE, estimateK=TRUE,
                  as.sparse = TRUE){
  #' This function computes the estimates for the A and W matrix based on the algorithm proposed in Ke and Wang's work: https://arxiv.org/pdf/1704.07016.pdf
  #'
  #'
  #' @param D the document term matrix. Can be a dense or sparse matrix. The dimensions of this matrix should be p x n, with
  #'          p the number of words in the dictionary, and n the number of documents. D is assumed to have been appropriately
  #'          preprocessed beforehand.
  #' @param K the number of topics. If null, it is computed from the data using the SVD decomposition of D.
  #' @param normalize Normalization method used on the topic document matrix. Choices include "none" (no normalization), "norm" (row-wise mean normalization),
  #'                  and "norm_score_N" (mean/N).
  #'
  #' @param N the document length. Only necessary for the prenormalization option "norm_score_N" and
  #' @param threshold boolean. Should the words in the dictionary be thresholded or not (corresponds to a sparsity assumption on A)
  #' @param VHMethod vertex hunting method. Choice between SVS (sketched vertex search), SP (successive projections) and SVS-SP.
  #'         Note that the paper by Tracy Ke recommends using SVS, as it tends to be more robust to noise and outliers. SVS requires additional parameters K0 and m.
  #' @param m parameter for the SVS vertex hunting procedure. Corresponds to the number of clusters used in the denoising step of SVS (clusters are taken as
  #'          a proxy for the r computed in the previous step, and the averaging is supposed to make them less noisy).
  #'          Ke recommends using m >>> K, but smaller than p. The value m = 10 * K is used in the paper.
  #' @param K0 parameter for the SVS vertex hunting procedure. Corresponds to the number of clusters closest to the overall
  #'           cluster grand mean (amongst the m) that are kept. The value K0= ceiling(1.5 * K) is recommended in the paper and used as default here.
  #' @param Mquantile quantile of M to cap the lowest values of M to. In real data analysis, it is sometimes beneficial to use a positive value of quantile tau,
  #'                  to avoid over-weighting those extremelylow-frequency words in the pre-SVD normalization
  #'
  #' @return The square of the input value.
  #'
  #' @examples
  #' square(4)
  #'
  #'
  #' @export
  #'
  #'
  if(typeof(D) != "sparseMatrix" & as.sparse){
    D <- as(D, "sparseMatrix")
  }
  p <- dim(D)[1]
  n <- dim(D)[2]
  print(c(n, p, K, M))


  newD=D
  new_p=p
  M <- as.numeric(rowMeans(D)) #### average frequency at which each word appears in each document
  #transpose_newD=  t(newD)

  #Step 1: SVD
  if (as.sparse){
    newD <- switch(normalize,
                   #"norm" = Diagonal(x=sqrt(M_trunk^(-1))) %*% newD,
                   #"norm_score_N" = Diagonal(x=sqrt(M2^(-1)))%*% newD,
                   "C2" = newD %*% t(newD)  - n/M * Diagonal(x=M),
                   "C1" = newD %*% t(newD),
                   "none" = newD)
  }else{
    newD <- switch(normalize,
                   #"norm" = diag(sqrt(M_trunk^(-1))) %*% newD,
                   #"norm_score_N" = diag(sqrt(M2^(-1)))%*% newD,
                   "C2" = newD %*% t(newD)  - n/M * diag(M),
                   "C1" = newD %*% t(newD),
                   "none" = newD)
  }



  if (K >= min(dim(newD))){
    obj = svd(newD, min(K, min(dim(newD))))
  }else{
    obj = svds(newD, K)
  }

  if (estimateK){
    if (max_K >= min(dim(newD))){
      obj_full = svd(newD, min(max_K, min(dim(newD))))
    }else{
      obj_full = svds(newD, max_K)
    }
    eigenvalues = obj_full$d
  }else{
    eigenvalues = obj$d
  }

  Xi <- obj$u





  #Step 2: Post-SVD normalization
  Xi[,1] <- abs(Xi[,1]) ### to get rid of some small irregularities, but should be positive and bounded away from 0 (this is guaranteed by Perron's theorem)
  if (K==1){
    H = Xi
  }else{
    if(normalize=="C2"){
      H <- apply(as.matrix(Xi[, 2:K]),2, function(x) x/ Xi[,1])
    }else{
      H=Xi
    }
  }





 #step 3 VH algorithm
  print("Start VH")
  if (VHMethod == 'SVS'){
    vertices_est_obj <- vertices_est(H, K0, m)
    V <- as.matrix(vertices_est_obj$V, ncol=ncol(H))
    theta <- vertices_est_obj$theta
  } else if (VHMethod == 'SP' || (VHMethod == "AA" & K<3)){
    if (K<3){
      i1 = which.max(H)
      i2 = which.min(H)
      V <- as.matrix(H[c(i1, i2),], ncol=ncol(H))
    }else{
      vertices_est_obj <- successiveProj(H, K)
      V <- as.matrix(vertices_est_obj$V, ncol=ncol(H))
    }
    theta <- NULL
  } else if (VHMethod == 'SVS-SP'){
    vertices_est_obj <- vertices_est_SP(H, m)
    V <- as.matrix(vertices_est_obj$V, ncol=ncol(H))
    theta <- NULL
  }else if (VHMethod == 'AA'){
    vertices_est_obj <- ArchetypeA(r_to_py(H), as.integer(K))
    V<-vertices_est_obj$V
    theta<-NULL

  }

  if (scatterplot & K>2){
    par(mar=c(1,1,1,1))
    ggplot(data.frame(R), aes(x=X1, y=X2))+
      geom_point( size=3.5)+
      geom_point(data=data.frame(V), colour="red", size=3.5)
  }


  #Step 4: Topic matrix estimation
  print("Start Step 4")
  if (K == 1){
    Pi = R %*% diag(1/V)
  }else{
    if (normalize=="C2"){
      if(rankMatrix(cbind(V,rep(1,nrow(V))))[1]<K || min(dim(V)) < max(dim(V))){
        Pi <- cbind(H, rep(1,new_p)) %*% MASS::ginv(cbind(V,rep(1,K)))
      }else{
        Pi <- cbind(H, rep(1,new_p)) %*% solve(cbind(V, rep(1,K)))
      }
    }else{
      if(rankMatrix(V)[1]<K || min(dim(V)) < max(dim(V))){
        Pi <- H %*% MASS::ginv(V)
      }else{
        Pi <- H %*% solve(V)
      }
    }
  }


  Pi <- pmax(Pi,matrix(0,dim(Pi)[1],dim(Pi)[2])) ### sets negative entries to 0
  temp <- rowSums(Pi)
  Pi <- apply(Pi,2,function(x) x/temp)

  #Step 5
  if (normalize=="C2"){
    A_star <- Xi[,1] * Pi


    #Step  5b: normalize each column to have a unit l1 norm
    temp <- colSums(A_star)
    A_hat <- t(apply(A_star,1,function(x) x/temp))

  }else{
    A_star= Pi
    A_hat=Pi
  }




  #Step 6

  return(list(A_hat=A_hat,A_star=A_star, H=H,V=V, Pi=Pi, theta=theta,Xi=Xi,
              eigenvalues = eigenvalues))
}








