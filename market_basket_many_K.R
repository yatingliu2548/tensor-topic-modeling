library(tidyverse)
## other methods
library(nnTensor)
library(tidyverse)
library(topicmodels)
library(tidytext)
setwd("~/Documents/tensor-topic-modeling/")
source("algorithm.R")
source("bayesian.R")
source("NTD.R")
source("tensor_operations.r")
source("run_experiments.R")

load("~/Downloads/market_basket_data.RData")

D3 <- matricization(data$D, 3)
X=t(D3)
active_words = which(apply(X[1:nrow(X),], 2, sum)>0)
Q1 <- dim(D_new@data)[1]
Q2 <- dim(D_new@data)[2]
R <- dim(D_new@data)[3]
#### Convert counts to frequencies
doc_length = apply(X[, active_words], 1, sum)
x_train = t(diag(1/ doc_length) %*% X[, active_words])
R <- length(active_words)
X_train <- tensorization(as.matrix(x_train), 3, Q1, Q2, R)
data_list <- list(D = D_new,
             X =  X_train)

#for (K3 in c(23, 20, 18, 15, 12, 10, 8, 7, 6 ,5)){
M = median(apply(D3, 2, sum))
stoppppp
save(data_list, K1, K2, M, file= "~/Downloads/market_basket_data_small.RData"
     )
for (K3 in c(23, 20, 18, 15, 12)){
  for (method in c("TTM-HOOI", "TTM-HOSVD",  "TopicScore-HOSVD", "NTD")){
    results <- run_method(data, K1=K1, K2=K2, K3=K3, M=M, 
                          method=method)
    save(results, file=paste0("~/Downloads/market_basket", 
                              method, "_K1_",
                              K1, "_K2_", K2, "_K3_", K3, ".RData"))
    
    print(paste0("Done with method ", method))
    #print(error)
    #write_csv(error, paste0("/scratch/midway3/cdonnat/tensor-topic-modeling/tensor-topic-modeling/synthetic/results/",result_file, ".csv"))
  }
}