
setwd("~/Documents/tensor-topic-modeling/")
source("algorithm.R")
source("data_generation.R")
source("run_experiments.R")
source("VH_algo.R")
source("SLDA.R")


seed = 123
K1 =  2
K2 =  2
K3 =  4
sparse = FALSE
n_anchors = 2
R = 500
error <- c()
print(c(seed, R))
M =100
Q1 = 10
Q2=Q1
print(paste0("R = ", R, " Q1 = ", Q1, " Q2=", Q1, " K1 = ", K1, " K2 = ", K2, " K3= ", K3, " M= ", M," seed = ", seed))
data <- synthetic_dataset_creation(Q1=Q1,
                                         Q2=Q2,p=R, K1=K1, K2=K2, K3=K3, 
                                         alpha_dirichlet=0.1, n_max_zipf=50000, a_zipf=1, 
                                         n_anchors=n_anchors, 
                                         delta_anchor=0.1, N=M, seed=seed, offset_zipf=2.7, 
                                         vary_by_topic=FALSE, sparsity = sparse)



A1_df = data.frame(data$A1)
colnames(A1_df) <- 1:K1
A1_df["ID1"] = 1:Q1
A1_df <- pivot_longer(A1_df, cols = -c("ID1")
                      )
colnames(A1_df) <- c("ID1", "Cluster", "Value")

ggplot(A1_df, aes(x=ID1, y=Cluster, fill =Value)) +
  geom_tile() +
  theme_bw()


core = data$G
core = data.frame(t(matricization(data$G,3)))
colnames(core) <- 1:K3
core["Dim1"] = unlist(lapply(1:K1, function(x){rep(x, K2)}))
core["Dim2"] = unlist(lapply(1:K1, function(x){1:K2}))
core <- pivot_longer(core, cols = -c("Dim1", "Dim2")
)
colnames(core) <- c("Dim1", "Dim2", "Topic", "Value")

ggplot(core, aes(x= Dim1, y=Topic, fill =Value)) +
  geom_tile() +
  facet_wrap(~Dim2)+
  theme_bw()
error<-c()
results_store <-list()
for (method in c("bayesian", "LDA", "STM", "NTD", 
                 "TopicScore-HOSVD", "TTM-HOOI",
                 "TTM-HOSVD")){
  results <- run_experiment(data=data, K1=K1, K2=K2, K3=K3, 
                            M=M, method=method)
  error <- update_error(hatA1=results$A1, data$A1,
                        hatA2=results$A2, data$A2,
                        hatA3=results$A3, data$A3,
                        hatcore=results$core, data$G@data, K1, K2, K3, 
                        Q1, Q2, R, M, 
                        results$time,
                        method, error)
  
  results_stm = results
  ggplot(A1_df, aes(x=ID1, y=Cluster, fill =Value)) +
    geom_tile() +
    theme_bw()
  
  print(paste0("Done with method ", method))
  print(error)
}
A1_bayes_df = results_bayesian$A1_df
ggplot(A1_bayes_df, aes(x=Id1, y=Cluster1, fill =Probability)) +
  geom_tile() +
  theme_bw()

core_bayes_df = results_bayesian$core_hat_df
ggplot(core_bayes_df, aes(x=cluster1, y=Topic, fill =Probability)) +
  geom_tile() +
  facet_grid(.~ cluster2)+
  theme_bw()



ggplot(core_bayes_df, aes(x=cluster1, y=Topic, fill =Probability)) +
  geom_tile() +
  facet_grid(.~ cluster2)+
  theme_bw()


A1_df = data.frame(data$A1)
colnames(A1_df) <- 1:K1
A1_df["ID1"] = 1:Q1
A1_df <- pivot_longer(A1_df, cols = -c("I1")
)
colnames(A1_df) <- c("ID1", "Cluster", "Value")

ggplot(A1_df, aes(x=ID1, y=Cluster, fill =Value)) +
  geom_tile() +
  theme_bw()


core_df = data.frame(matricization(data$G, 3))
colnames(core_df) <- 1:K3
core_df["cluster1"] = unlist(lapply(1:K1, function(x){rep(x, K2)}))
core_df["cluster2"] = unlist(lapply(1:K1, function(x){1:K2}))
core_df <- pivot_longer(core_df, cols = -c("cluster1", "cluster2")
)
colnames(core_df)[3:4] <- c("Topic", "Value")

ggplot(core_df, aes(x=cluster1, y=Topic, fill =Value)) +
  geom_tile() +
  facet_grid(.~ cluster2)+
  theme_bw()


core_stm_df = results_stm$core_hat_df
ggplot(core_stm_df, aes(x=cluster1, y=Topic, fill =Probability)) +
  geom_tile() +
  facet_grid(.~ cluster2)+
  theme_bw()


core_bayes_df = results_bayesian$core_hat_df
ggplot(core_bayes_df, aes(x=cluster1, y=Topic, fill =Probability)) +
  geom_tile() +
  facet_grid(.~ cluster2)+
  theme_bw()


core_hooi_df = data.frame(matricization(results_hooi$core, 3))
colnames(core_hooi_df) <- 1:K3
core_hooi_df["cluster1"] = unlist(lapply(1:K1, function(x){rep(x, K2)}))
core_hooi_df["cluster2"] = unlist(lapply(1:K1, function(x){1:K2}))
core_hooi_df <- pivot_longer(core_hooi_df, cols = -c("cluster1", "cluster2")
)
colnames(core_hooi_df)[3:4] <- c("Topic", "Value")

ggplot(core_hooi_df, aes(x=cluster1, y=Topic, fill =Value)) +
  geom_tile() +
  facet_grid(.~ cluster2)+
  theme_bw()
