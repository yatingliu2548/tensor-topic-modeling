library(nnTensor)

fit_NTD <- function(D, num.iter=2){
  ntd_res <- NTD(D, rank=c(K1,K2,K3), algorithm="KL",init="NMF",num.iter=num.iter)
  hatA_ntd <- ntd_res$A
  
  ### Extract first factor
  Pi <- t(hatA_ntd$A1)
  Pi <- pmax(Pi,matrix(0,dim(Pi)[1], dim(Pi)[2])) ### sets negative entries to 0
  temp <- rowSums(Pi)
  Pi <- apply(Pi, 2, function(x) x/temp)
  hatA1=Pi
  
  Pi2  <- t(hatA_ntd$A2)
  Pi2 <- pmax(Pi2,matrix(0,dim(Pi2)[1],dim(Pi2)[2])) ### sets negative entries to 0
  temp <- rowSums(Pi2)
  Pi2 <- apply(Pi2,2,function(x) x/temp)
  hatA2 <- Pi2
  
  Pi3 <- t(hatA_ntd$A3)
  Pi3 <- pmax(Pi3,matrix(0,dim(Pi3)[1],dim(Pi3)[2])) ### sets negative entries to 0
  temp <- colSums(Pi3)
  Pi3 <- t(apply(t(Pi3),2,function(x) x/temp))
  hatA3 <- Pi3
  
  hatcore <- ntd_res$S
  G3 <- matricization(hatcore,3)
  G3 <- pmax(G3,matrix(0,dim(G3)[1],dim(G3)[2])) ### sets negative entries to 0
  temp <- colSums(G3)
  G3 <- t(apply(G3,1,function(x) x/temp))
  G3[is.na(G3)] <- 0
  hatcore=tensorization(G3,3,dim(hatcore)[1],dim(hatcore)[2],dim(hatcore)[3])
  
  return(list(A1 = hatA1,
              A2 = hatA2,
              A3 = hatA3,
              core=hatcore
              ))
}


