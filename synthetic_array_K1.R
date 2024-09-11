source("algorithm.R")
source("data_generation.R")
source("run_experiments.R")
source("VH_algo.R")

#setwd("~/Documents/tensor-topic-modeling/")
args = commandArgs(trailingOnly=TRUE)
seed = ceiling(as.numeric(args[1]))
result_file = args[2]
print(" result file; ")
print(result_file)
Q1 =  as.numeric(args[3])
R =  as.numeric(args[4])
sparse = as.logical(args[5])
n_anchors = 2
error <- c()
print(seed)

Mlist <- c(500, 1000, 5000,  10000)# 
K1list <- c(2, 5, 7, 10, 15, 20)
K3list <- c(5)#
for (K1 in K1list){
  for (K3 in K3list){
    for (M in Mlist){
	    Q2=Q1
	    K2=K1
	    print(paste0("R = ", R, " Q1 = ", Q1, " Q2=", Q1, " K1 = ", K1, " K2 = ", K2, " K3= ", K3, " M= ", M," seed = ", seed))
      data <- synthetic_dataset_creation(Q1=Q1,
                                        Q2=Q2,p=R, K1=K1, K2=K2, K3=K3, 
                                        alpha_dirichlet=1, n_max_zipf=50000, a_zipf=1, 
                                        n_anchors=n_anchors, 
                                        delta_anchor=0.1, N=M, seed=seed, offset_zipf=2.7, 
                                        vary_by_topic=FALSE, sparsity = sparse)
      #write_csv(as.data.frame(matrization_tensor(data$Y,3)), paste0(getwd(), paste0("/synthetic/results/","data")))
      for (method in c("bayesian", "LDA","STM", "NTD", "TopicScore-HOSVD", "TTM-HOOI", "TTM-HOSVD")){
        results <- run_experiment(data=data, K1=K1, K2=K2, K3=K3, 
                                M=M, method=method)
        error <- update_error(hatA1=results$A1, data$A1,
                              hatA2=results$A2, data$A2,
                              hatA3=results$A3, data$A3,
                              hatcore=results$core, data$G@data, K1, K2, K3, 
                              Q1, Q2, R, M, 
                              results$time,
                              method, error)
        
        print(paste0("Done with method ", method))
	print(error)
        write_csv(error, paste0("/scratch/midway3/cdonnat/tensor-topic-modeling/tensor-topic-modeling/synthetic/results/",result_file, ".csv"))
      }
    }
  }
}
