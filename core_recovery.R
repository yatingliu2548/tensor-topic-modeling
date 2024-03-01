core_recovery(D,Xi1,Xi2,Xi3,V1,V2,V3,A_star3){
  S_hat= tensor_create(D,t(Xi1),t(Xi2),t(Xi3))
  a_0=colSums(abs(A1))
  check_V3=a_0 *V3
  G=tensor_create(S_hat,V1,V2,check_V3)
  G3=matrization_tensor(G,3)
  G3 <- pmax(G3,matrix(0,dim(G3)[1],dim(G3)[2])) ### sets negative entries to 0
  temp <- colSums(G3)
  G3 <- apply(G3,1,function(x) x/temp)
  Gnew=tensorization(G3,3,dim(G)[1],dim(G)[2],dim(G)[3])
  return(list(G=Gnew))
}
