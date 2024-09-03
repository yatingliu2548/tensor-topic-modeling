library(rstan)
library(CVXR)
source("PLSI.r")

fit_bayesian_model <- function(X, K1, K2, K3,
                               use_vb = TRUE){
  # Compile the Stan model
  stan_model <- stan_model(file = "lda_model.stan", model_name = "tensor_LDA")
  # Data list for Stan
  
  Q1 <- dim(X)[1]  # Number of reviewers
  Q2 <- dim(X)[2]   # Number of papers
  V <- dim(X)[3]   # Vocabulary size
  K <- K3  # Number of topics
  D <- Q1 * Q2  # Total number of reviews (assuming each reviewer reviewed each paper)
  D3 = matricization(X, 3)
  reviewer_ids <- rep(1:Q1, each = Q2)
  paper_ids <- rep(1:Q2, times = Q1)
  data_list <- list(N1 = Q1, N2 = Q2, V = V, 
                    K = K3, K1 = K1, K2 = K2, 
                    D = D, 
                    X = t(D3), 
                    reviewer_ids = reviewer_ids, 
                    paper_ids = paper_ids)
  if (use_vb){
    fit <- vb(stan_model, data = data_list, iter = 10000, output_samples = 1000)
  }else{
    fit <- sampling(stan_model, data = data_list, iter = 2000, chains = 4)
  }
  params <- extract(fit)
  # Access specific parameters
  phi_samples <- params$phi       # Word distributions for each topic
  reviewer_type_samples <- params$reviewer_type  # Reviewer type distribution
  paper_category_samples <- params$paper_category  # Paper category distribution
  core_samples <- params$core     # Core tensor
  
  # Extracting the reviewer type distributions (reviewer_type)
  reviewer_type_mean <- apply(reviewer_type_samples, c(2, 3), mean)  # Mean across iterations
  reviewer_type_mean = data.frame(reviewer_type_mean)
  colnames(reviewer_type_mean) = c("Type 1", "Type 2")
  reviewer_type_mean["sample"] = sapply(1:Q1, function(x){paste0("Reviewer ", x)})
  # Convert to long format
  reviewer_type_df <- pivot_longer(reviewer_type_mean, cols = -c("sample"))
  colnames(reviewer_type_df) <- c("Id1", "Cluster1", "Probability")
  print("here")
  
  #  Extracting the paper category distributions (paper_category)
  paper_category_mean <- apply(paper_category_samples, c(2, 3), mean)  # Mean across iterations
  # Convert to long format
  paper_category_mean = data.frame(paper_category_mean)
  colnames(paper_category_mean) = sapply(1:K2, function(x){paste0("Category ", x)})
  paper_category_mean["paper"] = sapply(1:Q2, function(x){paste0("Paper ", x)})
  # Convert to long format
  paper_category_df <- pivot_longer(paper_category_mean, cols = -c("paper"))
  colnames(paper_category_df) <- c("Id2", "Cluster2", "Probability")
  
  
  # Extracting the word distributions for each topic (phi)
  phi_mean <- apply(phi_samples, c(2, 3), mean)  # Take the mean across iterations
  phi_mean = t(phi_mean)
  phi_mean = data.frame(phi_mean)
  colnames(phi_mean) = sapply(1:K3, function(x){paste0("Topic ", x)})
  phi_mean["word"] = sapply(1:V, function(x){paste0("Word ", x)})
  # Convert to long format
  phi_mean_df <- pivot_longer(phi_mean, cols = -c("word"))
  colnames(phi_mean_df) <- c("word", "Topic", "Probability")
  
  
  # Extracting the core
  core_mean <- apply(core_samples, c(2, 3), mean)  # Take the mean across iterations
  core_mean = data.frame(core_mean)
  colnames(core_mean) = sapply(1:K3, function(x){paste0("Topic ", x)})
  core_mean["cluster1"] =  rep(1:K1, each = K2)
  core_mean["cluster2"] =  rep(1:K2, times = K1)
  # Convert to long format
  core_mean_df <- pivot_longer(core_mean, cols = -c("cluster1", "cluster2"))
  colnames(core_mean_df) <- c("cluster1", "cluster2", "Topic", "Probability")
  
  
  return(list(core = core_mean_df,
              A3 = phi_mean_df,
              A2 = paper_category_df,
              A1 = reviewer_type_df))

  
}


fit_LDA <- function(X, K1, K2, K3){
  D3 = matricization(X, 3)
  R = dim(D3)[1]
  lda3 <- LDA(t(D3), k = K3, 
              control = list(seed = 1234), method = 'VEM')
  ap_topics3 <- tidy(lda3, matrix = "beta")
  ap_topics3["word"] = unlist(lapply(1:R, function(x){rep(x, K3)}))
  ap_topics3 = pivot_wider(ap_topics3, id_cols = "word", names_from = "topic", 
                           values_from = "beta")
  W3 = tidy(lda3, matrix = "gamma")
  W3 <- pivot_wider(W3, id_cols = "document", 
                    names_from = "topic",
                    values_from = "gamma")
  W3_modified = W3
  W3_modified["dim1"] = unlist(lapply(1:Q1, function(x){rep(x, Q2)}))
  W3_modified["dim2"] = unlist(lapply(1:Q1, function(x){1:Q2}))
  #### Transform W3 into a tensor
  W3_modified = W3_modified %>%
    pivot_longer(cols = starts_with("1"):starts_with("4"), 
                 names_to = "topic", 
                 values_to = "value")
  
  W3_modified_A1 <- W3_modified %>%
    select(-c("document"))%>%
    unite("dim2_topic", dim2, topic, sep = "_topic_") %>%
    pivot_wider(names_from = dim2_topic, values_from = value)
  #### Apply SPOC
  A1_hat <- fit_SPOC(W3_modified_A1 %>% select(-c("dim1"))/ Q2, K1)
  
  W3_modified_A2 <- W3_modified %>%
    select(-c("document"))%>%
    unite("dim1_topic", dim1, topic, sep = "_topic_") %>%
    pivot_wider(names_from = dim1_topic, values_from = value)
  #### Apply SPOC
  A2_hat <- fit_SPOC(W3_modified_A2 %>% select(-c("dim2"))/ Q1, K2)
  ##### Estimate the core through regression
  # Compute the Kronecker product
  A1A2 <- kronecker(A1_hat$What, A2_hat$What)
  
  # Define the beta variable as a matrix with K1 * K2 rows and K3 columns
  beta <- Variable(K1 * K2, K3)
  
  # Objective: minimize the sum of squared residuals
  objective <- Minimize(sum_squares(as.matrix(W3 %>% select(-c("document"))) - A1A2 %*% beta))
  
  # Constraints: non-negativity and row sums equal to 1
  constraints <- list(beta >= 0, sum_entries(beta, axis = 2) == 1)
  
  # Define the problem
  problem <- Problem(objective, constraints)
  
  # Solve the problem
  result <- solve(problem)
  
  # Get the optimized beta
  beta_optimized <- matrix(result$getValue(beta), nrow = K1 * K2, ncol = K3)
  beta_optimized[which(beta_optimized < 1e-10)] = 0
  # Print the optimized beta
  return(list(core = beta_optimized,
              A1 = A1_hat,
              A2 = A2_hat,
              A3 = ap_topics3))
  
}





plot_results_dim1 <- function(reviewer_type_df){
  # Plot the reviewer type distributions
  ggplot(reviewer_type_df, aes(x = Reviewer, y = Probability, fill = factor(Reviewer_Type))) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = "Reviewer Type Distributions", x = "Reviewer Type", y = "Probability") +
    theme_minimal()
  
}


plot_results_dim2 <- function(reviewer_type_df){
  # Plot the paper category distributions
  ggplot(paper_category_df, aes(x = Paper, y = Probability, fill = factor(Paper_Category))) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = "Paper Category Distributions", x = "Paper Category", y = "Probability") +
    theme_minimal()
}


plot_topics <- function(){
  # Plot the word distributions for each topic
  ggplot(phi_mean_df, aes(x = word, y = Probability, fill = factor(Topic))) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = "Word Distributions for Each Topic (Phi)", x = "Word", y = "Probability") +
    theme_minimal()
}



