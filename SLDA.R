library(stm)
###Run SLDA

#Y3 is R times N1N2
# SLDA <- stm(documents = Matrix(as.matrix(as.data.frame(t(Y3))), sparse = TRUE),
#             K = 4, prevalence =~ race*time,
#             max.em.its = 10,
#             data = df,
#             init.type = "Spectral")

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
  list_params_it=rep(0,length(list_params))
  for (k in list_params){
    DD=convert_dtm_to_stm_format(t(matrization_tensor(tensor_data,3)))

    poliblogPrevFit <-tryCatch(
        stm(documents = Matrix(X[train_index,],sparse=T),
            K = k, prevalence =~ mense_status*Age+mense_status*Race,
            max.em.its = max_em,
            data = data,
            init.type = "Spectral",gamma.prior='L1'),
        error = function(err) {
          # Code to handle the error (e.g., print an error message, log the error, etc.)
          cat("Error occurred while running lda:", conditionMessage(err), "\n")
          # Return a default value or NULL to continue with the rest of the code
          return(NULL)
        })
    if (is.null(poliblogPrevFit)==FALSE){
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
    list_params_it[it]=k
    it=it+1
    }
  }

  names(topic_models) <- sapply(list_params_it, function(x){paste0("k", x)})
  #names(topic_models_test) <- sapply(list_params,function(x){paste0("k",x)})
  return(topic_models)

}


run_NTD<- function(X, train_index,Q1,Q2,K1,K2,
                   list_params=1:9){
  ####
  active_train = which(apply(X[train_index,], 2, sum)>0)
  x_train = t(diag(1/ apply(X[train_index, active_train],1, sum)) %*% X[train_index, active_train])
  print(dim(x_train))
  topic_models <- vector("list", length(list_params))
  tensor_data=tensorization(as.matrix(x_train),3,Q1,Q2,dim(x_train)[1])
  D3=colnames(matrization_tensor(tensor_data,3))
  it = 1
  list_params_it=rep(0,length(list_params))
  M=median(apply(X, 1, sum))
  for (k in list_params){
   
    NTD_results <-tryCatch(
      NTD(tensor_data/M,rank=c(K1,K2,k),algorithm = "KL"),
      error = function(err) {
        # Code to handle the error (e.g., print an error message, log the error, etc.)
        cat("Error occurred while running lda:", conditionMessage(err), "\n")
        # Return a default value or NULL to continue with the rest of the code
        return(NULL)
      })
    if (is.null(NTD_results)==FALSE){
      A_hat = matrix(0, ncol(X), k)
      tensordata=NTD_results$S
      NTD_G_3=matrization_tensor(tensordata,3)
      NTD_G_3=NTD_G_3/colSums(NTD_G_3)
      tensordata=tensorization(NTD_G_3,3,K1,K2,k)
      #tensordata=G@data
      W_hat=matricization(tensordata,3)
      NTD_A1=t(NTD_results$A$A1)
      NTD_A1=NTD_A1/rowSums(NTD_A1)
      NTD_A2=t(NTD_results$A$A2)
      NTD_A2=NTD_A2/rowSums(NTD_A2)
      NTD_A3=t(NTD_results$A$A3)
      NTD_A3=NTD_A3/colSums(NTD_A3)
      # Reshape the permuted tensor into a matrix
      W_hat <- matrix(W_hat, nrow = dim(tensordata)[3], byrow = FALSE)
      print(dim(W_hat))
      W_hat=W_hat%*% kronecker(t(NTD_A1),t(NTD_A2))
      
      A_hat[active_train, ] = NTD_A3
      topic_models[[it]] <- list(
        beta = log(t(A_hat)) %>% magrittr::set_colnames(colnames(X)),
        gamma =  t(W_hat) %>% magrittr::set_rownames(D3)
      )
      list_params_it[it]=k
      it=it+1
    }
  }
  
  names(topic_models) <- sapply(list_params_it, function(x){paste0("k", x)})
  #names(topic_models_test) <- sapply(list_params,function(x){paste0("k",x)})
  return(topic_models)
  
}
