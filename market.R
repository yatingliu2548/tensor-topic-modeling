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

K3 = 23

elapsed_timeOurs <- system.time({
  tmp<-score(data_list$X, normalization="TTM", method = "HOSVD", K1=K1, K2=K2, K3=K3, M=M,
          as.sparse = FALSE)
})["elapsed"]
STOP

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
                          , "_K1_",
                          K1, "_K2_", K2, "_K3_", K3, ".RData"))



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
                          , "_K1_",
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
                          , "_K1_",
                          K1, "_K2_", K2, "_K3_", K3, ".RData"))
