###Run SLDA



convert_dtm_to_stm_format <- function(dtm) {
  # Ensure that the DTM is in a matrix format (it could be sparse or dense)
  data_cleaned <- dtm[, colSums(dtm) != 0]
  dtm <- as.matrix(data_cleaned)


  # Map Vocabulary to Sequential Indices
  vocab <- colnames(dtm)
  vocab_map <- seq_along(vocab)

  # Convert the DTM to the STM format
  documents <- lapply(1:nrow(dtm), function(i) {
    non_zero_indices <- which(dtm[i, ] != 0)          # Find non-zero elements
    non_zero_words <- vocab_map[non_zero_indices]
    # Map words to indices
    non_zero_counts <- as.integer(dtm[i, unique(non_zero_words)])  # Word counts as integers

    # Create the 2xN matrix for this document
    matrix(c(unique(non_zero_words), non_zero_counts), nrow = 2, byrow = TRUE)
  })

  return(list(documents = documents, vocab = vocab))
}






run_SLDA_models <- function(X, train_index,data,Q1,Q2,
                            list_params=1:9,max_em=5){
  ####
  active_train = which(apply(X[train_index,], 2, sum)>0)
  x_train = t(diag(1/ apply(X[train_index, active_train],1, sum)) %*% X[train_index, active_train])
  print(dim(x_train))
  topic_models <- vector("list", length(list_params))
  tensor_data=tensorization(as.matrix(x_train),3,Q1,Q2,dim(x_train)[1])
  D3=colnames(matrization_tensor(tensor_data,3))
  data=data[data$SampleID%in%train_index,]
  it = 1
  print(dim(data))
  for (k in list_params){
    DD=convert_dtm_to_stm_format(t(matrization_tensor(tensor_data,3)))

    poliblogPrevFit <- stm(documents = Matrix(X[train_index,],sparse=T),
                           K = k, prevalence =~ mense_status*Age+mense_status*Race,
                           max.em.its = max_em,
                           data = data,
                           init.type = "Spectral")
    A_hat = matrix(0,ncol(X),k)
    #W_SLDA=poliblogPrevFit$theta
    #A_SLDA=exp(poliblogPrevFit$beta$logbeta[[1]])
    #colnames(A_hat)=poliblogPrevFit$vocab
    #G=tm$hatcore
    #tensordata=G@data
    print(dim(A_hat))

    W_hat=poliblogPrevFit$theta
    print(dim(W_hat))

    A_hat[active_train, ] =  exp(poliblogPrevFit$beta$logbeta[[1]])
    topic_models[[it]] <- list(
      beta = (log(t(A_hat)) %>% magrittr::set_colnames(colnames(X))),
      gamma =  ((W_hat) %>% magrittr::set_rownames(D3))
    )
    it <- it + 1
  }
  names(topic_models) <- sapply(list_params, function(x){paste0("k", x)})
  #names(topic_models_test) <- sapply(list_params,function(x){paste0("k",x)})
  return(topic_models)

}
