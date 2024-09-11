tensor_lda<-function(D,K1,K2,K3, use_vb = TRUE,max_retries=10){
  
  elapsed_time_bayes <- system.time({
    bayesian_res <- NULL
    retry <- TRUE
    max_retries <- max_retries # Define the maximum number of retries
    attempt <- 0
    
    while (retry && attempt < max_retries) {
      attempt <- attempt + 1
      message("Attempt: ", attempt)
      
      bayesian_res <- tryCatch(
        fit_bayesian_model(D, K1, K2, K3, use_vb = TRUE),
        error = function(err) {
          message("Error occurred while running the Bayesian method: ", conditionMessage(err), "\nRetrying...")
          return(NULL)  # Return NULL to indicate failure and trigger retry
        }
      )
      
      # If bayesian_res is not NULL, it means the function succeeded
      if (!is.null(bayesian_res)) {
        retry <- FALSE  # Stop retrying if no error occurred
      } else if (attempt >= max_retries) {
        message("Maximum retries reached. Exiting...")
        retry <- FALSE
      }
    }
  })["elapsed"]
  
  
  if (is.null(bayesian_res)==FALSE){
    A1_hat_df = bayesian_res$A1
    A2_hat_df = bayesian_res$A2
    A3_hat_df = bayesian_res$A3
    core_hat_df = bayesian_res$core
    time = elapsed_time_bayes
    
    #### convert into matrices
    
    A1_hat = A1_hat_df %>%
      pivot_wider(id_cols = "Id1", names_from = "Cluster1", values_from = "Probability")
    A1_hat = as.matrix(A1_hat[, 2:ncol(A1_hat)])
    A2_hat = A2_hat_df %>%
      pivot_wider(id_cols = "Id2", names_from = "Cluster2", values_from = "Probability")
    A2_hat = as.matrix(A2_hat[, 2:ncol(A2_hat)])
    A3_hat = A3_hat_df %>%
      pivot_wider(id_cols = "word", names_from = "Topic", values_from = "Probability")
    A3_hat = as.matrix(A3_hat[, 2:ncol(A3_hat)])
    core_hat = array(rep(0, K1 * K2 *K3),
                     dim=c(K1,K2,K3))
    for (k1 in 1:K1){
      for (k2 in 1:K2){
        for (k3 in 1:K3){
          core_hat[k1,k2,k3] = core_hat_df$Probability[which((core_hat_df$cluster1 == k1) & (core_hat_df$cluster2 == k2) & (core_hat_df$Topic == paste0("Topic ", k3)))]
        }
      }
    }
    
  }else{
    A1_hat = NULL
    A2_hat = NULL
    A3_hat = NULL
    core_hat = NULL
    time = elapsed_time_bayes
    A1_hat_df = NULL
    A2_hat_df = NULL
    A3_hat_df = NULL
    core_hat_df = NULL
  }
  return(list(A1 = A1_hat,
              A2 = A2_hat,
              A3 = A3_hat,
              core = core_hat,
              A1_df = A1_hat_df,
              A2_df = A2_hat_df,
              A3_df = A3_hat_df,
              core_hat_df = core_hat_df,
              time = time))
}



post_lda<-function(D,K1,K2,K3){
  elapsed_time_bayes <- system.time({
    bayesian_res <- NULL
    retry <- TRUE
    max_retries <- 3  # Define the maximum number of retries
    attempt <- 0
    
    while (retry && attempt < max_retries) {
      attempt <- attempt + 1
      message("Attempt: ", attempt)
      
      bayesian_res <- tryCatch(
        fit_LDA(D, K1, K2, K3),
        error = function(err) {
          message("Error occurred while running the Bayesian method: ", conditionMessage(err), "\nRetrying...")
          return(NULL)  # Return NULL to indicate failure and trigger retry
        }
      )
      
      # If bayesian_res is not NULL, it means the function succeeded
      if (!is.null(bayesian_res)) {
        retry <- FALSE  # Stop retrying if no error occurred
      } else if (attempt >= max_retries) {
        message("Maximum retries reached. Exiting...")
        retry <- FALSE
      }
    }
  })["elapsed"]
  
  
  if (is.null(bayesian_res)==FALSE){
    A1_hat_df = bayesian_res$A1
    A2_hat_df = bayesian_res$A2
    A3_hat_df = bayesian_res$A3
    core_hat_df = bayesian_res$core
    time = elapsed_time_bayes
    
    #### convert into matrices
    
    A1_hat = A1_hat_df %>%
      pivot_wider(id_cols = "Id1", names_from = "Cluster1", values_from = "Probability")
    A1_hat = as.matrix(A1_hat[, 2:ncol(A1_hat)])
    A2_hat = A2_hat_df %>%
      pivot_wider(id_cols = "Id2", names_from = "Cluster2", values_from = "Probability")
    A2_hat = as.matrix(A2_hat[, 2:ncol(A2_hat)])
    A3_hat = A3_hat_df %>%
      pivot_wider(id_cols = "word", names_from = "Topic", values_from = "Probability")
    A3_hat = as.matrix(A3_hat[, 2:ncol(A3_hat)])
    core_hat = array(rep(0, K1 * K2 *K3),
                     dim=c(K1,K2,K3))
    for (k1 in 1:K1){
      for (k2 in 1:K2){
        for (k3 in 1:K3){
          core_hat[k1,k2,k3] = core_hat_df$Probability[which((core_hat_df$cluster1 == k1) & (core_hat_df$cluster2 == k2) & (core_hat_df$Topic == paste0("Topic ", k3)))]
        }
      }
    }
    
  }else{
    A1_hat = NULL
    A2_hat = NULL
    A3_hat = NULL
    core_hat = NULL
    time = elapsed_time_bayes
    A1_hat_df = NULL
    A2_hat_df = NULL
    A3_hat_df = NULL
    core_hat_df = NULL
  }
  return(list(A1 = A1_hat,
              A2 = A2_hat,
              A3 = A3_hat,
              core = core_hat,
              A1_df = A1_hat_df,
              A2_df = A2_hat_df,
              A3_df = A3_hat_df,
              core_hat_df = core_hat_df,
              time = time))
}






run_tensorLDA_models <- function(X, train_index,Q1,Q2,K1,K2,
                            list_params=1:9){
  ####
  active_train = which(apply(X[train_index,], 2, sum)>0)
  x_train = t( X[train_index, active_train])
  print(dim(x_train))
  topic_models <- vector("list", length(list_params))
  tensor_data=tensorization(as.matrix(x_train),3,Q1,Q2,dim(x_train)[1])
  D3=colnames(matrization_tensor(tensor_data,3))
  it = 1
  list_params_it=rep(0,length(list_params))
  for (k in list_params){
    print(k)
    tm <-tensor_lda(tensor_data,K1,K2,k)
    if (is.null(tm)==FALSE){
    A_hat = matrix(0, ncol(X), k)
    tensordata=tm$core
    #tensordata=G@data
    W_hat=matricization(tensordata,3)
    
    # Reshape the permuted tensor into a matrix
    W_hat <- matrix(W_hat, nrow = dim(tensordata)[3], byrow = FALSE)
    print(dim(W_hat))
    W_hat=W_hat%*% kronecker(t(tm$A1),t(tm$A2))
    
    A_hat[active_train, ] = tm$A3
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




run_pLDA_models <- function(X, train_index,Q1,Q2,K1,K2,
                                 list_params=1:9){
  ####
  active_train = which(apply(X[train_index,], 2, sum)>0)
  x_train = t( X[train_index, active_train])
  print(dim(x_train))
  topic_models <- vector("list", length(list_params))
  tensor_data=tensorization(as.matrix(x_train),3,Q1,Q2,dim(x_train)[1])
  D3=colnames(matrization_tensor(tensor_data,3))
  it = 1
  list_params_it=rep(0,length(list_params))
  for (k in list_params){
    print(k)
    tm <--tryCatch(post_lda(tensor_data@data,K1,K2,k), error = function(err) {
      # Code to handle the error (e.g., print an error message, log the error, etc.)
      cat("Error occurred while running lda:", conditionMessage(err), "\n")
      # Return a default value or NULL to continue with the rest of the code
      return(NULL)
    })
    if (is.null(tm)==FALSE){
      print(k)
    A_hat = matrix(0, ncol(X), k)
    tensordata=tm$core
    #tensordata=G@data
    W_hat=matricization(tensordata,3)
    
    # Reshape the permuted tensor into a matrix
    W_hat <- matrix(W_hat, nrow = dim(tensordata)[3], byrow = FALSE)
    print(dim(W_hat))
    W_hat=W_hat%*% kronecker(t(tm$A1),t(tm$A2))
    
    A_hat[active_train, ] = tm$A3
    topic_models[[it]] <- list(
      beta = log(t(A_hat)) %>% magrittr::set_colnames(colnames(X)),
      gamma =  t(W_hat) %>% magrittr::set_rownames(D3)
    )
    list_params_it[it]=k
    it <- it + 1
    }
  }
  names(topic_models) <- sapply(list_params_it, function(x){paste0("k", x)})
  #names(topic_models_test) <- sapply(list_params,function(x){paste0("k",x)})
  return(topic_models)
  
}
