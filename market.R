load("~/Downloads/market_basket_data_small.RData")
library('rARPACK')
#library('Matrix')
library(roxygen2)
#library(quadprog)
setwd("~/Documents/tensor-topic-modeling/")
source("VH_algo.R")
library(Matrix)
library(rTensor)
library(tensr)
library(cluster)
library(tidyverse)
## other methods
library(nnTensor)
library(tidyverse)
library(topicmodels)
library(tidytext)
source("algorithm.R")
source("bayesian.R")
source("NTD.R")
source("tensor_operations.r")

K1 = 4
K2 = 2
K3 = 10

elapsed_timeOurs <- system.time({
  tmp<-score(data_list$X, normalization="TTM", method = "HOSVD", 
             K1=K1, K2=K2, K3=K3, M=M,
          as.sparse = FALSE)
})["elapsed"]


A1_hat = tmp$hatA1
A2_hat = tmp$hatA2
A3_hat = tmp$hatA3
core_hat=tmp$hatcore

results <- list(A1 = A1_hat,
            A2 = A2_hat,
            A3 = A3_hat,
            core = core_hat,
            time = elapsed_timeOurs)
save(results, file=paste0("~/Downloads/market_basket_TTM-HOSVD", 
                          "_K1_final_preproc",
                          K1, "_K2_", K2, "_K3_", K3, ".RData"))

topics <- data.frame(results$A3)
colnames(topics)[1:K3] <- 1:K3
topics["ID"] = as.numeric(colnames(data4analysis)[3:(ncol(data4analysis))][words])

topics_filtered = topics %>%
  pivot_longer(cols=-c("ID")
  )
colnames(topics_filtered) <- c("ID", "Topic", "frequency")
topics_filtered = topics_filtered %>%
  group_by(Topic) %>%
  slice_max(order_by = frequency, n = 15)
topics_filtered_W = topics_filtered
topics_filtered = left_join(topics_filtered, test_product_filtered)
topics_filtered_W = pivot_wider(topics_filtered_W, id_cols = "ID",
                                names_from ="Topic",
                                values_from = "frequency",
                                values_fill = 0)
topics_filtered_W = pivot_longer(topics_filtered_W, cols =-c("ID"))
topics_filtered_W = left_join(topics_filtered_W, test_product_filtered)
df_filtered <- topics_filtered_W[topics_filtered_W$value > 0, ]
ggplot(df_filtered, aes(y = Cleaned_SUB_COMMODITY_DESC, x = as.factor(name),
                            colour = COMMODITY_DESC, size=100 * value)) + 
  geom_point() + theme_bw() + theme(legend.position = "none")

elapsed_timeOurs <- system.time({
  tmp<-score(data_list$X, normalization="TTM", method = "HOOI", K1=K1, K2=K2, K3=K3, M=M,
             as.sparse = FALSE)
})["elapsed"]


A1_hat = tmp$hatA1
A2_hat = tmp$hatA2
A3_hat = tmp$hatA3
core_hat=tmp$hatcore

results <- list(A1 = A1_hat,
                A2 = A2_hat,
                A3 = A3_hat,
                core = core_hat,
                time = elapsed_timeOurs)
save(results, file=paste0("~/Downloads/market_basket_TTM-HOOI", 
                           "_K1_",
                          K1, "_K2_", K2, "_K3_", K3, ".RData"))


elapsed_timeOurs <- system.time({
  tmp<-score(data_list$X, normalization="TopicScore", method = "HOSVD", K1=K1, K2=K2, K3=K3, M=M,
             as.sparse = FALSE)
})["elapsed"]


A1_hat = tmp$hatA1
A2_hat = tmp$hatA2
A3_hat = tmp$hatA3
core_hat=tmp$hatcore

results <- list(A1 = A1_hat,
                A2 = A2_hat,
                A3 = A3_hat,
                core = core_hat,
                time = elapsed_timeOurs)
save(results, file=paste0("~/Downloads/market_basket_TopicScore-HOSVD", 
                          "_K1_",
                          K1, "_K2_", K2, "_K3_", K3, ".RData"))


D3 = matricization(data_list$D, 3)
R = dim(D3)[1]
Q1 = dim(data_list$D)[1]
Q2 = dim(data_list$D)[2] 
lda3 <- LDA(t(D3), k = K3, 
            control = list(seed = 1234), method = 'VEM')
ap_topics3 <- tidy(lda3, matrix = "beta")
ap_topics3["word"] = unlist(lapply(1:R, function(x){rep(x, K3)}))
ap_topics3 = pivot_wider(ap_topics3, id_cols = "word", names_from = "topic", 
                         values_from = "beta")
A3_hat = ap_topics3 %>% select(-c("word"))
W3 = tidy(lda3, matrix = "gamma")
W3 <- pivot_wider(W3, id_cols = "document", 
                  names_from = "topic",
                  values_from = "gamma")
W3_modified = W3
W3_modified["dim1"] = unlist(lapply(1:Q1, function(x){rep(x, Q2)}))
W3_modified["dim2"] = unlist(lapply(1:Q1, function(x){1:Q2}))
#### Transform W3 into a tensor
W3_modified = W3_modified %>%
  pivot_longer(cols = -c("document", "dim1", "dim2"), 
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

A1_hat_df = data.frame(A1_hat)
  colnames(A1_hat_df) = 1:K1
  A1_hat_df["Id1"] = 1:Q1
  A1_hat_df = A1_hat_df %>% pivot_longer(cols = -c('Id1'))
  colnames(A1_hat_df) = c("Id1", "Cluster1", "Probability")
  
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

  core_hat = array(rep(0, K1 * K2 *K3),
                   dim=c(K1,K2,K3))
  for (k1 in 1:K1){
    for (k2 in 1:K2){
      for (k3 in 1:K3){
        core_hat[k1,k2,k3] = beta_optimized[(k1 - 1) * K2 + k2, k3]
      }
    }
  }
  
results <- list(A1 = A1_hat,
            A2 = A2_hat,
            A3 = A3_hat,
            core = core_hat)
save(results, file=paste0("~/Downloads/market_basketLDA", 
                          "_K1_",
                          K1, "_K2_", K2, "_K3_", K3, ".RData"))


#### Need to compare the results

results_list <- list()
topics_df <- c()
topics_filtered <- c()
A1_df <- c()
A2_df <- c()
core_list <- list()
K1<-4
K2<-4
K3 <- 10
method = "TTM-HOSVD"
load(file="~/Downloads/data4analysis_market_basket.RData")
test_product_filtered = read_csv( "~/Downloads/product_filtered.csv")
load( file=paste0("~/Downloads/market_basket_TTM-HOSVD", 
                          "_K1_final_preproc",
                          K1, "_K2_", K2, "_K3_", K3, ".RData"))
results_list[['TTM-HOSVD']] = results
topics <- data.frame(results$A3)
colnames(topics)[1:K3] <- 1:K3
topics["ID"] = as.numeric(colnames(data4analysis)[3:(ncol(data4analysis))][words])

topics_filtered = topics %>%
  pivot_longer(cols=-c("ID")
  )
colnames(topics_filtered) <- c("ID", "Topic", "frequency")
topics_filtered = topics_filtered %>%
  group_by(Topic) %>%
  slice_max(order_by = frequency, n = 15)
topics_filtered_W = topics_filtered
topics_filtered = left_join(topics_filtered, test_product_filtered)
topics_filtered_W = pivot_wider(topics_filtered_W, id_cols = "ID",
                                names_from ="Topic",
                                values_from = "frequency",
                                values_fill = 0)
topics_filtered_W = pivot_longer(topics_filtered_W, cols =-c("ID"))
topics_filtered_W = left_join(topics_filtered_W, test_product_filtered)
df_filtered <- topics_filtered_W[topics_filtered_W$value > 0, ]
df_filtered['method'] = method
topics['method'] = method

topics_filtered_df <-  df_filtered
topics_df <- topics

A1_hat = results$A1_hat
A1_hat['method'] = method
A1_df <- A1_hat

A2_hat = results$A2_hat
A2_hat['method'] = method
A2_df <-  A2_hat



for (method in  c("TTM-HOOI", "TopicScore-HOSVD", "LDA")){
  load(file=paste0("~/Downloads/market_basket_", method,
                            "_K1_",
                            K1, "_K2_", K2, "_K3_", K3, ".RData"))
  results_list[method] = results
  
  topics <- data.frame(results$A3)
  colnames(topics)[1:K3] <- 1:K3
  topics["ID"] = as.numeric(colnames(data4analysis)[3:(ncol(data4analysis))][words])
  
  topics_filtered = topics %>%
    pivot_longer(cols=-c("ID")
    )
  colnames(topics_filtered) <- c("ID", "Topic", "frequency")
  topics_filtered = topics_filtered %>%
    group_by(Topic) %>%
    slice_max(order_by = frequency, n = 15)
  topics_filtered_W = topics_filtered
  topics_filtered = left_join(topics_filtered, test_product_filtered)
  topics_filtered_W = pivot_wider(topics_filtered_W, id_cols = "ID",
                                  names_from ="Topic",
                                  values_from = "frequency",
                                  values_fill = 0)
  topics_filtered_W = pivot_longer(topics_filtered_W, cols =-c("ID"))
  topics_filtered_W = left_join(topics_filtered_W, test_product_filtered)
  df_filtered <- topics_filtered_W[topics_filtered_W$value > 0, ]
  df_filtered['method'] = method
  topics['method'] = method
  
  topics_filtered_df <- rbind(topics_filtered_df, df_filtered)
  topics_df <- rbind(topics_df, topics)
  
  A1_hat = results$A1_hat
  A1_hat['method'] = method
  A1_df <- rbind(A1_df,
                 A1_hat)
  
  A2_hat = results$A2_hat
  A2_hat['method'] = method
  A2_df <- rbind(A2_df,
                 A2_hat)
  
  
}



ggplot(topics_filtered_df %>% filter(method =="LDA"), 
       aes(y = Cleaned_SUB_COMMODITY_DESC, x = as.factor(name),
                        colour = COMMODITY_DESC, size=100 * value)) + 
  geom_point() + theme_bw() + theme(legend.position = "none")

topics_filtered_df = topics_filtered_df %>%
  mutate(name_comm = paste0(Cleaned_SUB_COMMODITY_DESC, ": ", COMMODITY_DESC))
list_comm = unique(topics_filtered_df$name_comm)


# Function to simplify and categorize product names
clean_and_categorize <- function(products) {
  # Simplify product names
  simplified_names <- sapply(products, function(x) {
    # Use regex to extract main descriptive part before the colon
    ifelse(grepl(":", x), sub(".*: (.*)", "\\1", x), x)
  })
  
  categories <- sapply(products, function(name) {
    if (grepl("SNACK|CHIP|NUT|CANDY|CHOCOLATE|GUM", name, ignore.case = TRUE)) {
      return("Candy & Snacks")
    } else if (grepl("FRZN|PIZZA|ENTREE|DINNER|SIDE DISHES|SOUP|LUNCHMEAT|PASTA|ENTREES|SANDWICHES|DISHES", name, ignore.case = TRUE)) {
      return( "Meals")
    } else if (grepl("SOFT DRINK|JUICE|DRINK|WATER|BEVERAGES|BEER|ALE|MALT|WINE", name, ignore.case = TRUE)) {
      return("Beverages")
    } else if (grepl("BAKED|COOKIES|BISCUITS|DOUGH|BAKING|CEREAL|BREAKFAST", name, ignore.case = TRUE)) {
      return( "Baked Goods & Breakfast")
    } else if (grepl("SUGAR|SALSA|CONDIMENTS|SAUCE", name, ignore.case = TRUE)) {
      return(  "Condiments")
    } else if (grepl("BEEF|MEAT|HOT DOGS|SEAFOOD", name, ignore.case = TRUE)) {
      return( "Meat & Seafood")
    } else if (grepl("YOGURT|CHEESE|MILK|DAIRY", name, ignore.case = TRUE)) {
      return("Dairy")
    } else if (grepl("FRUIT|VEGETABLE|SALAD|BERRIES|CORN|CITRUS|BROCCOLI", name, ignore.case = TRUE)) {
      return( "Fruits & Vegetables")
    } else if (grepl("DIAPERS|BABY|INFANT", name, ignore.case = TRUE)) {
      return( "Baby Products")
    } else if (grepl("CAT FOOD|DOG FOOD|CAT LITTER", name, ignore.case = TRUE)) {
      return( "Pet Products")
    } else {
      return( "Miscellaneous")
    }
  })
  
  # Create a data frame to return
  data.frame(Product = products,
    cleaned_name = simplified_names, new_category = categories)
}



# Apply function to product data
result <- clean_and_categorize(list_comm)
print(result)
colnames(result)[1] = c("name_comm")

topics_filtered_df2 = left_join(topics_filtered_df,
                               result, by="name_comm")

topics_filtered_df2$name = factor(topics_filtered_df2$name, levels = 1:K3)
ggplot(topics_filtered_df2 %>% 
         filter(method == "LDA") %>%
         arrange(new_category, cleaned_name),  # Arrange the data before plotting
       aes(x = name,
           y = reorder(cleaned_name, interaction(new_category, cleaned_name)),  # Reorder based on new_category
           colour = new_category, 
           size = 1000 * value)) +
  geom_point() +
  theme_bw() +
  ylab("Item") +
  xlab("Topic") +
  labs(size=NULL, colour="Category") +
  guides(size="none")


library(ggplot2)
library(dplyr)

# Assuming topics_filtered_df2 has been properly filtered for method == "TTM-HOOI"
# Prepare the data by specifying the order within each category
topics_filtered_df3 <- topics_filtered_df2 %>%
  filter(method ==  "TopicScore-HOSVD") %>%
  arrange(new_category, Cleaned_SUB_COMMODITY_DESC) %>%
  mutate(cleaned_name = factor(Cleaned_SUB_COMMODITY_DESC, levels = unique(Cleaned_SUB_COMMODITY_DESC)))  # Set factor levels based on current order

# Create the plot
ggplot(topics_filtered_df3, 
       aes(x = name, 
           y = name_comm, 
           colour = new_category, 
           size = 1000 * value)) +
  geom_point() +
  theme_bw() +
  ylab("Item") +
  xlab("Topic") +
  labs(size=NULL, colour="Category") +
  guides(size="none")  # Remove size legend



