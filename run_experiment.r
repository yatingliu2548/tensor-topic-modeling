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