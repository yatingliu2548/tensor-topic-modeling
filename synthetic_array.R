source("algorithm.R")
source("data_generation.R")
source("run_experiments.R")
source("VH_algo.R")

setwd("~/Documents/tensor-topic-modeling/")
args = commandArgs(trailingOnly=TRUE)
seed = ceiling(as.numeric(args[1]))
result_file = args[2]
Q1 = as.integer(ceiling(as.numeric(args[3])))
Q2 = 10
K1 = 2
K2 = 3
K3 = 4
matlab_path = args[4]
error <- c()
print(Q2)
print(Q1)
print(seed)

Rlist <- c(100, 1000)# 
Q1list <- c(2500, 3000, 4000, 5000, 10000)
Mlist <- c(1000, 5000, 10000)#
for (Q1 in Q1list){
  for (M in Mlist){
    for (R in Rlist){
      data <- synthetic_dataset_creation(Q1=Q1,
                                        Q2=Q2,R=R, K1=K1, K2=K2, K3=K3, 
                                        alpha_dirichlet=0.1, n_max_zipf=50000, a_zipf=1, 
                                        n_anchors=2, 
                                        delta_anchor=0.1, M=M, seed=seed, offset_zipf=2.7, 
                                        vary_by_topic=FALSE, sparsity = TRUE)
      #write_csv(as.data.frame(matrization_tensor(data$Y,3)), paste0(getwd(), paste0("/synthetic/results/","data")))
      error <- run_experiment(data=data, R=R, K1=K1, K2=K2, K3=K3, M=M, error=error)
      write_csv(error, paste0(getwd(), paste0("/synthetic/results/",result_file)))
    }
  }
}