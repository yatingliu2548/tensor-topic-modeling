library('rARPACK')
#library('Matrix')
library(roxygen2)
library(quadprog)
library(Matrix)
library(rTensor)
library(tensr)


score <- function(D, K1,K2,K3, scatterplot=FALSE, K0=NULL, m=NULL, M=NULL, threshold=FALSE,
                  Mquantile=0.00, VHMethod = 'SP', normalize="none",
                  alpha=0.005, max_K=150, returnW=FALSE, estimateK=FALSE,
                  as.sparse = FALSE){
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
 
  Q1=dim(D@data)[1]
  Q2=dim(D@data)[2]
  R=dim(D@data)[3]
  print(dim(D))
  n=Q1*Q2
  D3=matrization_tensor(D,3)
  X=t(D3)
  active_train = which(apply(X[1:nrow(X),], 2, sum)>0)
  x_train = t(diag(1/ apply(X[1:nrow(X), active_train],1, sum)) %*% X[1:nrow(X), active_train])
  print(dim(x_train))
  D=tensorization(as.matrix(x_train),3,Q1,Q2,dim(x_train)[1])
  n=Q1*Q2
  tildeM <- as.numeric(rowMeans(x_train))
  p=dim(x_train)[1]
  n=dim(x_train)[2]
  if (threshold){
      threshold_J = alpha * sqrt(log(max(p,n))/(M *n))
      print(sprintf("Threshold for alpha = %f  is %f ", alpha, threshold_J))
      setJ = which(tildeM > threshold_J)
      print(sprintf("Nb of elected words = %i  ( %f percent) ", length(setJ), 100 * length(setJ)/p))
      if (length(setJ) < 0.1 * length(tildeM)){
        setJ = sort(tildeM, decreasing=TRUE, index.return=TRUE)$ix[1:ceiling( 0.1 * length(tildeM))]
      }
      newD3 = as.matrix(x_train[setJ,])
      tildeM = tildeM[setJ]
      print(paste0(p-length(setJ), " words were thresholded (", (p-length(setJ))/p * 100, "%)"))
      print(paste0(length(setJ), " words remain"))
      new_p <- length(setJ)
      print(dim(newD3))
    
    }else{
      new_p = p
      newD3 = as.matrix(x_train)
      setJ = 1:length(tildeM)
    }
  D=tensorization(as.matrix(newD3),3,Q1,Q2,dim(newD3)[1])
  if (normalize =="Ours"){
    normM=n/M * diag(tildeM)
    #tensorM=tensorization(normM,mode=3,Q1=Q1,Q2=Q2,R=R)
    D3_ours =  newD3 %*% t(newD3)  - normM
  }
  if (normalize=="Tracy"){
    D3=matrization_tensor(D,3)
    tildeM <- as.numeric(rowMeans(D3))
    D3_tracy=diag(sqrt(tildeM^(-1))) %*% D3
    D=tensorization(D3_tracy,mode=3,Q1=Q1,Q2=Q2,Q3=dim(newD3)[1])
  }
  
  
 
  
  #'
  # if(typeof(D) != "sparseMatrix" & as.sparse){
  #   D <- as(D, "sparseMatrix")
  # }
  # p <- dim(D)[1]
  # n <- dim(D)[2]
  # print(c(n, p, K, M))
  # 
  # 
  # newD=D
  # new_p=p
  # M <- as.numeric(rowMeans(D)) #### average frequency at which each word appears in each document
  # #transpose_newD=  t(newD)
  # 
  # #Step 1: SVD
  # if (as.sparse){
  #   newD <- switch(normalize,
  #                  #"norm" = Diagonal(x=sqrt(M_trunk^(-1))) %*% newD,
  #                  #"norm_score_N" = Diagonal(x=sqrt(M2^(-1)))%*% newD,
  #                  "C2" = newD %*% t(newD)  - n/M * Diagonal(x=M),
  #                  "C1" = newD %*% t(newD),
  #                  "none" = newD)
  # }else{
  #   newD <- switch(normalize,
  #                  #"norm" = diag(sqrt(M_trunk^(-1))) %*% newD,
  #                  #"norm_score_N" = diag(sqrt(M2^(-1)))%*% newD,
  #                  "C2" = newD %*% t(newD)  - n/M * diag(M),
  #                  "C1" = newD %*% t(newD),
  #                  "none" = newD)
  # }
  # 
  # 

  K=min(K1,K2,K3)
  ranks=c(K1,K2,K3)
  
  if (normalize=="Ours"){
    if (K >= min(dim(D3_ours))){
      K=min(K, min(dim(D3_ours)))
      ranks= c(K,K,K)
      obj3 = svd(D3_ours, K)
    }else{
      obj3 = svds(D3_ours, K3)
    }
  }
  
  
  obj12 <-switch(normalize,
                "Ours" = rTensor::hosvd(D,ranks=ranks),
                "HOOI" =  hooi(D@data,r=ranks,itermax = 50),
                "HOSVD" =  rTensor::hosvd(D,ranks=ranks),
                "Tracy"=rTensor::hosvd(D,ranks=ranks))

  # if (estimateK){
  #   if (max_K >= min(dim(newD))){
  #     obj_full = svd(newD, min(max_K, min(dim(newD))))
  #   }else{
  #     obj_full = svds(newD, max_K)
  #   }
  #   eigenvalues = obj_full$d
  # }else{
  #   eigenvalues = obj$d
  # }

  Xi1 <- obj12$U[[1]]
  Xi2 <- obj12$U[[2]]
  #Xi3 <- obj12$U[[3]]
  
  if(normalize=="Ours"){
    Xi3<- obj3$u
  }else{
    Xi3 <- obj12$U[[3]]
  }
  
  
  Xi1[,1] <- abs(Xi1[,1])#
  Xi2[,1] <- abs(Xi2[,1])#
  Xi3[,1] <- abs(Xi3[,1])#




  #Step 2-6:
  est1<- steps_procedure( Xi=Xi1, normalize="C1",K=K1,K0=K0,VHMethod=VHMethod)
  est2<- steps_procedure( Xi=Xi2, normalize="C1",K=K2,K0=K0,VHMethod=VHMethod)
  est3<- steps_procedure( Xi=Xi3, normalize="C2",K=K3,K0=K0,VHMethod=VHMethod)
  
  if (scatterplot & min(ranks)>2){
    par(mar=c(1,1,1,1))
    #print(ggplot(data.frame(est1$H), aes(x=X1, y=X2))+
    #  geom_point( size=3.5)+
    #  geom_point(data=data.frame(est1$V), colour="red", size=3.5))
    
    #print(ggplot(data.frame(est2$H), aes(x=X1, y=X2))+
    #  geom_point( size=3.5)+
    #  geom_point(data=data.frame(est2$V), colour="red", size=3.5))
    
    #print(ggplot(data.frame(est3$H), aes(x=X1, y=X2))+
    #  geom_point( size=3.5)+
    #  geom_point(data=data.frame(est3$V), colour="red", size=3.5))
    sv1 <-svd(matrization_tensor(D,1), K1)
    sv2=svd(matrization_tensor(D,2), K2)
    sv3=svd(matrization_tensor(D,3), K3)
  
    svd_data <- data.frame(Mode1 = sv1$d[1:min(Q1,Q2,R)],Mode2 = sv2$d[1:min(Q1,Q2,R)],Mode3 = sv3$d[1:min(Q1,Q2,R)])

# Convert data to long format for ggplot
    svd_data_long <- reshape2::melt(svd_data, variable.name = "Mode", value.name = "SingularValue")

# Plot using ggplot2
    print(ggplot(svd_data_long, aes(x = 1:length(SingularValue), y = SingularValue, group = Mode, color = Mode)) +
  geom_line() +  # Add line
  geom_point() + # Add points
  scale_x_log10()+
  labs(title = "Scree Plot for Each Mode",
       x = "Component Number",
       y = "Singular Values") +
      theme_minimal())
  }
  

  S_hat=tensor_create(D,t(Xi1),t(Xi2),t(Xi3))
  
  a_0=colSums(abs(est3$A_star))
  check_V3=a_0 *est3$V
  G=tensor_create(S_hat,est1$V,est2$V,check_V3)
  G3=matrization_tensor(G,3)
  G3 <- pmax(G3,matrix(0,dim(G3)[1],dim(G3)[2])) ### sets negative entries to 0
  temp <- colSums(G3)
  G3 <- t(apply(G3,1,function(x) x/temp))
  G3[is.na(G3)] <- 0
  Gnew=tensorization(G3,3,dim(G)[1],dim(G)[2],dim(G)[3])
  hatA3_new=matrix(0,R,K3)
  A_temp=matrix(0,length(active_train),K3)
   if(threshold){
      A_temp[setJ, ] = est3$A_hat
      hatA3_new[active_train,]=A_temp
      hatA3=hatA3_new
    }else{
      hatA3_new[active_train,]=est3$A_hat
      hatA3=hatA3_new
    }

  return(list(hatA1=est1$A_hat,hatA2=est2$A_hat,hatA3=hatA3,hatcore=Gnew))
}

steps_procedure <- function(Xi,K,normalize,K0,VHMethod){
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
    vertices_est_obj <- vertices_est(H, K0, m)
    V <- as.matrix(vertices_est_obj$V, ncol=ncol(H))
    theta <- vertices_est_obj$theta
  } else if (VHMethod == 'SP' || (VHMethod == "AA" & K<3)){
    if (K<3){
      i1 = which.max(H[,1])
      i2 = which.min(H[,1])
      
      V <- as.matrix(H[c(i1, i2),], ncol=ncol(H))
    }else{
     
      #write_csv(as.data.frame(H), paste0(getwd(), paste0("/synthetic/results/","H")))
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



run_topic_models <- function(X, train_index,K1,K2,Q1,Q2, #test_index, 
                             list_params=1:9, 
                             normalize="Ours"){
  ####
  active_train = which(apply(X[train_index,], 2, sum)>0)
  x_train = t(diag(1/ apply(X[train_index, active_train],1, sum)) %*% X[train_index, active_train])
  print(dim(x_train))
  topic_models <- vector("list", length(list_params))  
  tensor_data=tensorization(as.matrix(x_train),3,Q1,Q2,dim(x_train)[1])
  D3=colnames(matrization_tensor(tensor_data,3))
  it = 1
  for (k in list_params){
    
    tm <- score(tensor_data, K1=K1,K2=K2,K3=k,M=median(apply(X, 1, sum)), normalize=normalize)
    A_hat = matrix(0, ncol(X), k)
    G=tm$hatcore
    tensordata=G@data
    W_hat=aperm(tensordata, c(3, 2, 1))
    
    # Reshape the permuted tensor into a matrix
    W_hat <- matrix(W_hat, nrow = dim(tensordata)[3], byrow = FALSE)
    print(dim(W_hat))
    W_hat=W_hat%*% kronecker(t(tm$hatA1),t(tm$hatA2))
    
    A_hat[active_train, ] = tm$hatA3
    topic_models[[it]] <- list(
      beta = log(t(A_hat)) %>% magrittr::set_colnames(colnames(X)),
      gamma =  t(W_hat) %>% magrittr::set_rownames(D3)
    )
    it <- it + 1
  }
  names(topic_models) <- sapply(list_params, function(x){paste0("k", x)})
  #names(topic_models_test) <- sapply(list_params,function(x){paste0("k",x)})
  return(topic_models)
  
}




