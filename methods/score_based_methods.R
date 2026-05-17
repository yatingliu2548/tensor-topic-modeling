if (requireNamespace("rARPACK", quietly = TRUE)) {
  svds <- rARPACK::svds
} else {
  svds <- function(A, k) {
    k_eff <- min(as.integer(k), nrow(A), ncol(A))
    s <- svd(A, nu = k_eff, nv = k_eff)
    list(
      u = s$u[, seq_len(k_eff), drop = FALSE],
      d = s$d[seq_len(k_eff)],
      v = s$v[, seq_len(k_eff), drop = FALSE]
    )
  }
}

library(quadprog)
library(Matrix)
library(rTensor)
library(tensr)


score <- function(empirical_D, K1,K2,K3, scatterplot=FALSE, K0=NULL, m=NULL, M=NULL, threshold=FALSE,
                  Mquantile=0.00, VHMethod = 'SP', normalize="none",
                  alpha=0.005, max_K=150, returnW=FALSE, estimateK=FALSE,
                  as.sparse = FALSE, return_diagnostics = FALSE){
  # Estimate latent factors and core tensor from an empirical tensor input.

  Q1=dim(empirical_D@data)[1]
  Q2=dim(empirical_D@data)[2]
  R=dim(empirical_D@data)[3]
  print(dim(empirical_D))
  n=Q1*Q2
  D3=matrization_tensor(empirical_D,3)
  X=t(D3)
  active_train = which(apply(X[1:nrow(X),], 2, sum)>0)
  x_train = t(diag(1/ apply(X[1:nrow(X), active_train],1, sum)) %*% X[1:nrow(X), active_train])
  print(dim(x_train))
  working_D=tensorization(as.matrix(x_train),3,Q1,Q2,dim(x_train)[1])
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
  working_D=tensorization(as.matrix(newD3),3,Q1,Q2,dim(newD3)[1])
  if (normalize =="Ours"){
    normM=n/M * diag(tildeM)
    #tensorM=tensorization(normM,mode=3,Q1=Q1,Q2=Q2,R=R)
    D3_ours =  newD3 %*% t(newD3)  - normM
  }
  if (normalize=="Tracy"){
    D3=matrization_tensor(working_D,3)
    tildeM <- as.numeric(rowMeans(D3))
    D3_tracy=diag(sqrt(tildeM^(-1))) %*% D3
    working_D=tensorization(D3_tracy,mode=3,Q1=Q1,Q2=Q2,Q3=dim(newD3)[1])
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
                "Ours" = rTensor::hosvd(working_D,ranks=ranks),
                "HOOI" =  hooi(working_D@data,r=ranks,itermax = 50),
                "HOSVD" =  rTensor::hosvd(working_D,ranks=ranks),
                "Tracy"=rTensor::hosvd(working_D,ranks=ranks))

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
  est1<- steps_procedure( Xi=Xi1, normalize="C1",K=K1,K0=K0,VHMethod=VHMethod,m=m)
  est2<- steps_procedure( Xi=Xi2, normalize="C1",K=K2,K0=K0,VHMethod=VHMethod,m=m)
  est3<- steps_procedure( Xi=Xi3, normalize="C2",K=K3,K0=K0,VHMethod=VHMethod,m=m)

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
    sv1 <-svd(matrization_tensor(working_D,1), K1)
    sv2=svd(matrization_tensor(working_D,2), K2)
    sv3=svd(matrization_tensor(working_D,3), K3)

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


  S_hat=tensor_create(working_D,t(Xi1),t(Xi2),t(Xi3))

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

  out <- list(hatA1=est1$A_hat,hatA2=est2$A_hat,hatA3=hatA3,hatcore=Gnew)
  if (isTRUE(return_diagnostics)) {
    est1_anchor <- est1$anchor_indices
    est2_anchor <- est2$anchor_indices
    if (threshold) {
      est3_anchor_global <- active_train[setJ[est3$anchor_indices]]
    } else {
      est3_anchor_global <- active_train[est3$anchor_indices]
    }
    out$diagnostics <- list(
      normalize = normalize,
      VHMethod = VHMethod,
      active_train = active_train,
      setJ = setJ,
      anchor_indices = list(
        A1 = est1_anchor,
        A2 = est2_anchor,
        A3 = est3$anchor_indices,
        A1_global = est1_anchor,
        A2_global = est2_anchor,
        A3_global = est3_anchor_global
      ),
      est1 = list(
        H = est1$H,
        V = est1$V,
        V_raw = est1$V_raw,
        Pi = est1$Pi,
        anchor_indices = est1$anchor_indices,
        anchor_indices_global = est1_anchor
      ),
      est2 = list(
        H = est2$H,
        V = est2$V,
        V_raw = est2$V_raw,
        Pi = est2$Pi,
        anchor_indices = est2$anchor_indices,
        anchor_indices_global = est2_anchor
      ),
      est3 = list(
        H = est3$H,
        V = est3$V,
        V_raw = est3$V_raw,
        Pi = est3$Pi,
        anchor_indices = est3$anchor_indices,
        anchor_indices_global = est3_anchor_global
      )
    )
  }

  return(out)
}

steps_procedure <- function(Xi,K,normalize,K0,VHMethod,m=NULL){
  Xi[,1] <- abs(Xi[,1])### to get rid of some small irregularities, but should be positive and bounded away from 0 (this is guaranteed by Perron's theorem)
  new_p=dim(Xi)[1]
  if (is.null(m)) {
    m <- ceiling(10 * K)
  }
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
    vertices_est_obj <- vertices_est(H, K0 = K0, m = m, K = K)
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
    vertices_est_obj <- vertices_est_SP(H, m = m, K = K)
    V <- as.matrix(vertices_est_obj$V, ncol=ncol(H))
    theta <- NULL
  }else if (VHMethod == 'AA'){
    vertices_est_obj <- ArchetypeA(H, as.integer(K))
    V<-vertices_est_obj$V
    theta<-NULL

  }

  V_raw <- as.matrix(V)
  nearest_vertex_indices <- vapply(seq_len(nrow(V_raw)), function(j) {
    distances <- rowSums((H - matrix(V_raw[j, ], nrow = nrow(H), ncol = ncol(H), byrow = TRUE))^2)
    which.min(distances)
  }, integer(1))

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

  return(list(A_hat=A_hat,A_star=A_star, H=H,V=V,V_raw=V_raw,Pi=Pi,theta=theta,Xi=Xi,
              anchor_indices=nearest_vertex_indices))


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



spectral_clustering <- function(A, K,max_iter=100,tol=1e-6,mix=T) {
  #matrix is N1 times N1 matrix = W1\times t(W1)
  # Step 1: Calculate U (leading K eigenvectors of A)
  eig <- svds(A, K) # nv = number of eigenvectors
  U <- eig$u  # Leading K eigenvectors
  A1=matrix(0,nrow=nrow(U),ncol=K)

  # Step 2: Perform approximate k-means on U
  # Use kmeans function with a fixed number of iterations or other stopping criteria
  if(mix){
    U[,1] <- abs(U[,1])
    Theta=steps_procedure(Xi=U, normalize="C1",K=K,K0=10,VHMethod="SP",m=ceiling(10 * K))
    A1=Theta$A_hat
    A1[is.na(A1)]=A1
  }else{
    print(mix)
    kmeans_result <- kmeans(U, centers = K, nstart = 25)
    Theta <- matrix(0, nrow = nrow(U), ncol = K)
    for (i in 1:nrow(U)) {
      Theta[i, kmeans_result$cluster[i]] <- 1
    }

    for (iter in 1:max_iter) {

      # Step 2: Solve for X given Theta
      X <- solve(t(Theta) %*% Theta) %*% t(Theta) %*% U

      # Step 3: Update Theta given X
      Theta_new <- matrix(0, nrow = nrow(U), ncol = K)
      for (i in 1:nrow(U)) {
        best_cluster <- which.min(colSums((t(X) - U[i, ])^2))
        Theta_new[i, best_cluster] <- 1
      }

      # Check for convergence
      if (norm(Theta_new - Theta, "F") < tol) {
        return(Theta)
        break
      }

      Theta<- Theta_new
      A1=Theta
      return(Theta)
    }
  }
  return(A1)
}


compute_G_from_WA <- function(A_hat, D){

  K <-dim(A_hat)[2]
  n <- dim(D)[2]

  W_hat <- matrix(0, K, n)
  M <- rbind(diag(K-1), rep(-1,K-1))
  bM <- diag(K)[,K]
  Dmat <- 2*t(A_hat %*% M) %*% (A_hat %*% M)
  Amat <- t(M)
  bvec <- -bM

  AM <- A_hat %*% M
  AbM <- A_hat %*% bM
  for (i in 1:n){
    dvec <- 2*t(D[,i]-AbM) %*% AM
    # Dmat <- matrix(nearPD(Dmat)$mat, nrow(Dmat), ncol(Dmat))
    # Dmat <- nearPD(Dmat)
    qp_sol <- solve.QP(Dmat, dvec, Amat, bvec)$solution
    W_hat[,i] <- c(qp_sol, 1-sum(qp_sol))
  }
  W_hat <- pmax(W_hat,0)
  ### sets negative entries to 0
  G=t(W_hat)
  #hatcore_es=tensorization(t(G_LDA),3,K1,K2,dim(G_LDA)[2])
  temp <- colSums(G)
  G <- t(apply(G,1,function(x) x/temp))
  G[is.na(G)] <- 0
  W_hat=t(G)
  return(W_hat)
}
