library(Matrix)
library(combinat)
library(clue)

replaceWithLeastPositive <- function(vec){
  vec[vec<=0] = min(vec[vec>0])
  return(vec)
}

nearPD <- function(mat){
  mat <- (mat+t(mat))/2
  eigenObj <- eigen(mat)
  values <- eigenObj$values
  vectors <- eigenObj$vectors
  values <- replaceWithLeastPositive(values)
  return(vectors%*%diag(values)%*%t(vectors))
}

successiveProj <- function(R, K){
  # succesive projection on rows of R
  n <- dim(R)[1]
  
  Y <- cbind(rep(1,n),R)
  indexSet = c()
  it = 1
  valid_indices <- 1:dim(Y)[1]
  while (length(indexSet) < K & dim(Y)[1]>0){
    #print(paste0("here it is ", it))
    #print(sprintf("Length set is %d and dim(Y)[1]=%d", length(indexSet), dim(Y)[1] ))
    l2Norms <- apply(Y,1,function(x) sqrt(sum(x^2)))
    #### check if they are in the same document
    index <- which(l2Norms == max(l2Norms))
   
    if (length(index) >1){
      r = rankMatrix(R[valid_indices[index], ])[1]
      if (r < length(index)){
        ### only select 1
        chosen = sample(index,  1)
        u <- Y[chosen,] / sqrt(sum(Y[chosen,]^2))
        indexSet <- c(indexSet, valid_indices[chosen])
        Y <- Y[-setdiff(index, c(chosen)), ]
        valid_indices <- setdiff(valid_indices, valid_indices[setdiff(index, c(chosen))])
        Y <- t(apply(Y, 1, function(x) x-(x%*%t(u))%*%(u)))
      }else{
        u <- Y[index,] / sqrt(sum(Y[index,]^2))
        indexSet <- c(indexSet, valid_indices[index])
        Y <- t(apply(Y, 1, function(x) x-(x%*%t(u))%*%(u)))
      }
    }else{
      indexSet <- c(indexSet, valid_indices[index])
      u <- Y[index,] / sqrt(sum(Y[index,]^2))
      Y <- t(apply(Y,1,function(x) x-sum(x*u)*u))
    }
    it = it + 1
    
    
  }
  if(length(indexSet)>K){
    indexSet = sample(indexSet, K)
  }
  
  return(list(V=R[indexSet,], indexSet=indexSet))
}

vertices_est_SP <- function(R,m){
  library(quadprog)
  K <- dim(R)[2] + 1
  
  obj <- kmeans(R,m,iter.max=K*100, nstart = K*10)
  theta <- as.matrix(obj$centers)
  return(successiveProj(theta, K))
}


vertices_est <- function(R, K0, m){
  library(quadprog)
  K <- dim(R)[2] + 1
  #Step 2a
  print(paste0("min is ", min(m, nrow(R))))
  print(c(m, dim(R)))
  print("K0 is:")
  print(K0)
  obj <- kmeans(R, centers = min(m, nrow(R)-1), iter.max=K*100, nstart = 1)
  
  theta <- as.matrix(obj$centers)
  theta_original <- theta
  
  #Step 2b'
  inner <- theta%*%t(theta)
  distance <- diag(inner)%*%t(rep(1,length(diag(inner)))) + rep(1,length(diag(inner)))%*%t(diag(inner)) - 2*inner
  top2 <- which(distance==max(distance),arr.ind=TRUE)[1,]
  theta0 <- as.matrix(theta[top2,])
  theta <- as.matrix(theta[-top2,])
  
  if (K0 > 2){
    for (k0 in 3:K0){
      print(k0)
      inner <- theta%*%t(theta)
      distance <- rep(1,k0-1)%*%t(diag(inner))-2*theta0%*%t(theta)
      ave_dist <- colMeans(distance)
      index <- which(ave_dist==max(ave_dist))[1]
      theta0 <- rbind(theta0, theta[index,])
      theta <- as.matrix(theta[-index,])
    }
    theta <- theta0
  }
  
  #Step 2b
  if (K0<=K){
    #plot(theta[,1],theta[,2])
    #points(theta[comb[,min_index],1],theta[comb[,min_index],2],col=2,pch=2)
    
    return(list(V=theta, theta=theta_original))
  }else{
    comb <- combn(1:K0, K)
    max_values <- rep(0, dim(comb)[2])
    for (i in 1:dim(comb)[2]){
      for (j in 1:K0){
        max_values[i] <- max(simplex_dist(as.matrix(theta[j,]), as.matrix(theta[comb[,i],])), max_values[i])
      }
    }
    min_index <- which(max_values == min(max_values))
    new_theta = theta[comb[,min_index[1]],]
    
    #plot(theta[,1],theta[,2])
    #points(theta[comb[,min_index],1],theta[comb[,min_index],2],col=2,pch=2)
    
    return(list(V=new_theta, theta=theta_original))
  }
  
}

ArchetypeA <- function(R,K){
  library(reticulate)
  use_condaenv("r-reticulate")
  source_python(paste0(getwd(), "/NMF.py"))
  resultfromAA=  acc_palm_nmf(X=R, r=K,proj_method='wolfe', m=5, maxiter=1000, 
                              c1 =1, c2 = 1, method = 'fista', fixed_max_size=5)
  return(list(V=resultfromAA$V,Weigh_hat=resultfromAA$weight))
}

simplex_dist <- function(theta, V){
  VV <- cbind(diag(rep(1,dim(V)[1]-1)), -rep(1,dim(V)[1]-1))%*%V
  D <- VV%*%t(VV)
  d <- VV%*%(theta-V[dim(V)[1],])
  
  A <- cbind(diag(rep(1,dim(V)[1]-1)), -rep(1,dim(V)[1]-1))
  b0 <- c(rep(0,dim(V)[1]-1),-1)
  
  # D <- matrix(nearPD(D)$mat, nrow(D), ncol(D))
  # D <- nearPD(D)
  obj <- solve.QP(D, d, A, b0)
  return(sum((theta-V[dim(V)[1],]) ^2)+ 2*obj$value)
}

error1_A <- function(A, A_hat){
  K <- dim(A)[2]
  all_perm <- permn(1:K)
  error <- Inf
  
  for (i in 1:length(all_perm)){
    error <- min(error, mean(colSums(abs(A[,all_perm[[i]]]-A_hat))))
  }
  
  return(error)
}

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
matrix_lp_distance <- function(A, B, lp=2){
  K <- dim(A)[2]
  if (lp ==2){
    error_matrix <- outer(seq_len(ncol(A)), seq_len(ncol(B)), Vectorize(function(i, j) l2_error(A[, i], B[, j])))
  }else{
    error_matrix <- outer(seq_len(ncol(A)), seq_len(ncol(B)), Vectorize(function(i, j) l1_error(A[, i], B[, j])))
  }
  # Find the optimal column permutation using the Hungarian algorithm
  permutation <- solve_LSAP(error_matrix) 
  B_permuted <- B[, permutation]
  
  if (lp ==2){
    error <- l2_error(A, B_permuted)
  }else{
    error <- l1_error(A, B_permuted)
  }
  # Construct the permutation matrix based on the assignment
  permutation_matrix <- matrix(0, nrow = ncol(A), ncol = ncol(A))
  for (i in 1:length(permutation)) {
    permutation_matrix[i, permutation[i]] <- 1
  }
  return(list(error=error,permutation=permutation_matrix))
}
l2_error <- function(A, B) {
  sqrt(sum((A - B)^2))
}

l1_error <- function(A, B) {
  sum(abs(A - B))
}

