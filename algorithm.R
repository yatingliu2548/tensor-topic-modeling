library('rARPACK')
#library('Matrix')
library(roxygen2)
#library(quadprog)
source("VH_algo.R")
library(Matrix)
library(rTensor)
library(tensr)
library(cluster)


score <- function(D, K1, K2, K3, 
                  scatterplot=FALSE, K0=NULL, m=NULL, M=NULL, threshold=FALSE,
                  Mquantile=0.00, 
                  VHMethod = 'SP', method="HOSVD", normalization="TTM",
                  alpha=0.005, max_K=150, returnW=FALSE, estimateK=FALSE,
                  as.sparse = FALSE, estimate_core_via_svd=TRUE){
  #' This function computes the estimates for the A and W matrix based on the algorithm proposed in Ke and Wang's work: https://arxiv.org/pdf/1704.07016.pdf
  #'
  #'
  #' @param D the document term matrix. Can be a dense or sparse matrix. The dimensions of this matrix should be p x n, with
  #'          p the number of words in the dictionary, and n the number of documents. D is assumed to have been appropriately
  #'          preprocessed beforehand.
  #' @param K1 the number of clusters in dimension 1. If null, it is computed from the data using the SVD decomposition of M_1(D).
  #' @param K2 the number of clusters in dimension 2. If null, it is computed from the data using the SVD decomposition of M_2(D).
  #' @param K3 the number of topics. If null, it is computed from the data using the SVD decomposition of  M_3(D).
  #' @param normalize Normalization method used on the topic document matrix. 
  #'                  Choices include "none" (no normalization), "norm" (row-wise mean normalization),
  #'                  and "norm_score_N" (mean/N).
  #' @param M the document length. Only necessary for the prenormalization option "norm_score_N" and
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
  #' @param alpha parameter  for the threshoding of the words in the dictionary. The threshold is set to alpha * sqrt(log(max(p,n))/(M *n)).

  #' @return The square of the input value.
  #'
  #' @examples
  #' square(4)
  #'
  #'
  #' @export
  #'
  #'
  #'
  #'
  if (is.null(K0)){
    K0 = ceiling(1.5 * K3)
  }

  Q1 <- dim(D@data)[1]
  Q2 <- dim(D@data)[2]
  R <- dim(D@data)[3]

  D3 <- matricization(D, 3)
  print(c(dim(D3), Q1, Q2, R))
  print(paste0("Dim voc init: ", R))

  
  X=t(D3)
  if (is.null(M)){
    M = median(apply(X, 1, sum))
  }
  #### Select words with non-zero entries
  active_words = which(apply(X[1:nrow(X),], 2, sum)>0)
  if(sum(X[1,])!=1){
    print("Processing data to frequency")
    print(sum(X[1,]))
    #### Convert counts to frequencies
    doc_length = apply(X[, active_words],1, sum)
    x_train = diag(1/ doc_length) %*% X[, active_words]
  }else{
    print("Skipped Frequency processing")
    #active_words = 1:R
    x_train = X[, active_words]
  }
  nb_docs = dim(x_train)[1]
  R = dim(x_train)[2]
  #x_train = X[1:nrow(X), active_words]
  x_train = t(x_train)
  D <- tensorization(as.matrix(x_train), 3, Q1, Q2, R)
  #Dtilde <- tensorization(as.matrix(x_train), 3, n, t, dim(x_train)[1])
  tildeM <- as.numeric(rowMeans(x_train))

  if (normalization == "TTM"){
    if (threshold){
      threshold_J = alpha * sqrt(log(max(R, nb_docs))/(M *nb_docs))
      print(sprintf("Threshold for alpha = %f  is %f ", alpha, threshold_J))
      setJ = which(tildeM > threshold_J)
      print(sprintf("Nb of elected words = %i  ( %f percent) ", length(setJ), 100 * length(setJ)/R))
      if (length(setJ) < 0.1 * length(tildeM)){
        setJ = sort(tildeM, decreasing=TRUE, index.return=TRUE)$ix[1:ceiling( 0.1 * length(tildeM))]
      }
      newD3 = as.matrix(x_train[setJ,])
      newD3 = as.matrix(x_train[setJ,])
      tildeM = tildeM[setJ]
      print(paste0(R-length(setJ), " words were thresholded (", (R-length(setJ))/R * 100, "%)"))
      print(paste0(length(setJ), " words remain"))
      new_p <- length(setJ)
      #print(dim(newD3))
    }else{
      new_p = R
      newD3 = as.matrix(x_train)
      setJ = 1:length(tildeM)
    }
  }else{
    new_p = R
    newD3 = as.matrix(x_train)
    setJ = 1:length(tildeM)
  }

  D=tensorization(as.matrix(newD3), 3, Q1, Q2, dim(newD3)[1])
  if (normalization =="TTM"){
    normM=nb_docs/M * diag(tildeM)
    newD =  newD3 %*% t(newD3)  - normM
    print("Normalization by  TTM yields dimensions:")
    print(c(dim(D)))
  }
  if (normalization =="TopicScore"){
    D3 <- matricization(D, 3)
    tildeM <- as.numeric(rowMeans(D3))
    D3_ts <- diag(sqrt(tildeM^(-1))) %*% D3
    D <- tensorization(D3_ts, mode=3, Q1=Q1, Q2=Q2, Q3=dim(D3_ts)[1])
  }
  
  D3 <- matricization(D, 3)
  K <- min(K1,K2,K3)
  ranks <- c(K1,K2,K3)
  
  if (normalization=="TTM"){
    if (K >= min(dim(D3))){
      K <- min(K, min(dim(D3)))
      ranks <- c(K, K, K)
      obj3 <- svd(newD, K)
    }else{
      obj3 <- svds(newD, K3)
    }
  }
  
  
  obj12 <- switch(method,
                "HOSVD" = rTensor::hosvd(D, ranks=ranks),
                "HOOI" =  hooi(D@data,r=ranks, itermax = 100))

  Xi1 <- obj12$U[[1]]
  Xi2 <- obj12$U[[2]]
  
  if(normalization == "TTM"){
    Xi3 <- obj3$u
  }else{
    Xi3 <- obj12$U[[3]]
  }


  Xi1[,1] <- abs(Xi1[,1])#
  Xi2[,1] <- abs(Xi2[,1])#
  Xi3[,1] <- abs(Xi3[,1])#


  #Step 2-6:
  est1 <- steps_procedure( Xi=Xi1, normalize="C1", K=K1, K0=K0, VHMethod=VHMethod)
  est2 <- steps_procedure( Xi=Xi2, normalize="C1", K=K2, K0=K0, VHMethod=VHMethod)
  est3 <- steps_procedure( Xi=Xi3, normalize="C2", K=K3, K0=K0, VHMethod=VHMethod)
  
  hatA3_new=matrix(0, dim(D@data)[3], K3)
  if(threshold){
    A_temp=matrix(0, R, K3)
    A_temp[setJ, ] = est3$A_hat
    hatA3_new[active_words,]=A_temp
    hatA3=hatA3_new
  }else{
    hatA3_new[active_words,] = est3$A_hat
    hatA3=hatA3_new
  }
  
  ### Compute the core tensor
  if (estimate_core_via_svd){
    S_hat <- create_tensor(D, t(Xi1), t(Xi2), t(Xi3))
    a_0 <- colSums(abs(est3$A_star))
    check_V3 <- a_0 * est3$V
    G <- create_tensor(S_hat, est1$V, est2$V, check_V3)
    G3 <- matricization(G,3)
    G3 <- pmax(G3,matrix(0, dim(G3)[1], dim(G3)[2])) ### sets negative entries to 0
    temp <- colSums(G3)
    G3 <- t(apply(G3,1,function(x) x/temp))
    G3[is.na(G3)] <- 0
    Gnew=tensorization(G3, 3, dim(G)[1], dim(G)[2], dim(G)[3])
  }else{
    ##### Estimate the core through regression
    # Compute the Kronecker product
    A1A2 <- kronecker(est1$A_hat,est2$A_hat)
    # Define the beta variable as a matrix with K1 * K2 rows and K3 columns
    beta <- Variable(K1 * K2, K3)
    
    # Objective: minimize the sum of squared residuals
    D3 = matricization(data$D, 3)
    tA3D3 = t(hatA3) %*% D3
    Y = t(tA3D3)
    objective <- Minimize(sum_squares( Y- A1A2 %*% beta))
    
    # Constraints: non-negativity and row sums equal to 1
    constraints <- list(beta >= 0, sum_entries(beta, axis = 2) == 1)
    
    # Define the problem
    problem <- Problem(objective, constraints)
    
    # Solve the problem
    result <- solve(problem)
    
    # Get the optimized beta
    beta_optimized <- as.matrix(result$getValue(beta), nrow = K1 * K2, ncol = K3)
    beta_optimized[which(beta_optimized < 1e-10)] = 0
    
    Gnew = array(rep(0, K1 * K2 *K3),
                     dim=c(K1,K2,K3))
    for (k1 in 1:K1){
      for (k2 in 1:K2){
        for (k3 in 1:K3){
          Gnew[k1,k2,k3] = beta_optimized[(k1 - 1) * K2 + k2, k3]
        }
      }
    }
    
    
  }
  
  return(list(hatA1=est1$A_hat,hatA2=est2$A_hat,hatA3=hatA3,
              hatcore=Gnew))
}

steps_procedure <- function(Xi ,K, normalize, K0, VHMethod){
  #' This function computes the estimates for the A and W matrix based on the algorithm proposed in Ke and Wang's work: https://arxiv.org/pdf/1704.07016.pdf
  #'
  #'
  #' @param D the document term matrix. Can be a dense or sparse matrix. The dimensions of this matrix should be p x n, with
  #'          p the number of words in the dictionary, and n the number of documents. D is assumed to have been appropriately
  #'          preprocessed beforehand.
  #' @param K1 the number of clusters in dimension 1. If null, it is computed from the data using the SVD decomposition of M_1(D).
  #' @param K2 the number of clusters in dimension 2. If null, it is computed from the data using the SVD decomposition of M_2(D).
  #' @param K3 the number of topics. If null, it is computed from the data using the SVD decomposition of  M_3(D).
  #' 
  Xi[,1] <- abs(Xi[,1])### to get rid of some small irregularities, but should be positive and bounded away from 0 (this is guaranteed by Perron's theorem)
  new_p=dim(Xi)[1]
  if (K==1){
    H = Xi
  }else{
    if(normalize=="C2"){
      H <- apply(as.matrix(Xi[, 2:K]),2, function(x) x/ Xi[,1])
      H[is.na(H)]<-0

    }else{
      H=Xi
    }
  }
  
  #step 3 VH algorithm
  print("Start VH")
  if (VHMethod == 'SVS'){
    vertices_est_obj <- vertices_est(H, K0, m=p/10)
    V <- as.matrix(vertices_est_obj$V, ncol=ncol(H))
    theta <- vertices_est_obj$theta
  } else if (VHMethod == 'SP' || (VHMethod == "AA" & K<3)){
    if (K<3){
      i1 = which.max(H[,1])
      i2 = which.min(H[,1])

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

  # if (scatterplot & K>2){
  #   par(mar=c(1,1,1,1))
  #   ggplot(data.frame(R), aes(x=X1, y=X2))+
  #     geom_point( size=3.5)+
  #     geom_point(data=data.frame(V), colour="red", size=3.5)
  # }


  #Step 4: Topic matrix estimation
  print("Start Step 4")
  if (K == 1){
    Pi = H %*% diag(1/V)
  }else{
    if (normalize=="C2"){
      if(rankMatrix(cbind(V,rep(1,nrow(V))))[1]<K || min(dim(V)) < max(dim(V))){
        Pi <- cbind(H, rep(1,new_p)) %*% MASS::ginv(cbind(V,rep(1,K)))
      }else{
        Pi <- cbind(H, rep(1,new_p)) %*% solve(cbind(V, rep(1,K)))
      }
    }else{
      if(rankMatrix(V)[1]<K || min(dim(V)) < max(dim(V))){
      #print(V)
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
    if (K==1){
      V=as.matrix(max(V),1,1)
    }else{
      V=cbind(rep(1,K),V)
    }
    #Step  5b: normalize each column to have a unit l1 norm
    A_star=replace(A_star, is.na(A_star), 0)
    temp <- colSums(A_star)
    A_hat <- t(apply(A_star,1,function(x) x/temp))

  }else{
    if (K==1){
      V=as.matrix(max(V),1,1)
    }
    A_star= Pi
    A_hat=Pi
  }

  return(list(A_hat=A_hat,A_star=A_star, H=H,V=V, Pi=Pi, theta=theta,Xi=Xi))


}


