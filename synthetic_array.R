source("our_method.R")
source("data_generation.R")
source("run_experiments.R")
source("VH_algo.R")
source("SLDA.R")


args = commandArgs(trailingOnly=TRUE)
seed = ceiling(as.numeric(args[1]))
result_file = args[2]
Q1 = as.integer(ceiling(as.numeric(args[3])))
Q2 = 10
K1=3
K2=3
K3=3
matlab_path = args[4]
error <- c()
print(Q2)
print(Q1)
print(seed)

Rlist=c(100,1000)#
#K1list=c(4,5,6,7,8,9,10,11,12,13,14,15)
Q1list=c(2500,3000,4000,5000,10000)
Mlist=c(1000,5000,10000)#
for (k in 1:length(Q1list)){
for (i in 1:length(Mlist)){
  for (j in 1:length(Rlist)){
    data = synthetic_dataset_creation(Q1=Q1list[k],Q2=Q2,R=Rlist[j], K1=K1,K2=K2,K3=K3, alpha_dirichlet=0.1, n_max_zipf=50000, a_zipf=1,n_anchors=2, delta_anchor=0.1, M=Mlist[i], seed=seed,offset_zipf=2.7,vary_by_topic=FALSE, sparsity = TRUE)
    #write_csv(as.data.frame(matrization_tensor(data$Y,3)), paste0(getwd(), paste0("/synthetic/results/","data")))
    error=run_experiment(data=data,R=Rlist[j],K1=K1,K2=K2,K3=K3,M=Mlist[i],error=error)

    write_csv(error, paste0(getwd(), paste0("/synthetic/results/",result_file)))

  }
}
}
