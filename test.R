library(tidyverse)
theme_set(theme_bw(base_size = 12))
Rlist=c(1000,5000,10000)
Q2=5
Mlist=c(100,500,1000,2500,5000,6000,7000,8000,9000,10000)
K1=3
K2=3
K3=3
Q1=1000
result_Q=c()
replications=50
error_update <- function(error,K,Q1,R,Q2,M=M,mode,method=NULL,time=NULL){
  error_temp <- data.frame(error=error,
                           K = K,
                           Q1=Q1,
                           R=R,
                           Q2=Q2,
                           M=M,
                           mode=mode,method=method,time=time)
  return(error_temp)
}
error_tmp=c()
for (rep in 1: replications){
for (i in 1:length(Mlist)){
  for (j in 1:length(Rlist)){
    data = synthetic_dataset_creation(Q1,Q2,Rlist[j], K1,K2,K3, alpha_dirichlet=0.1, n_max_zipf=50000, a_zipf=1,n_anchors=2, delta_anchor=0.1, M=Mlist[i], seed=123,offset_zipf=2.7,vary_by_topic=FALSE, sparsity = TRUE)
    error_tmp=run_experiment(data=data,K1=K1,K2=K2,K3=K3,M=Mlist[i],error=error_tmp)
  
  }
}
}
# data = synthetic_dataset_creation(Q1,Q2,R, K1,K2,K3, alpha_dirichlet=0.5, n_max_zipf=50000, a_zipf=1,n_anchors=2, delta_anchor=0.1, M=M, seed=123,offset_zipf=2.7,vary_by_topic=FALSE, sparsity = FALSE)
# tmp_1=score(matrization_tensor(data$D,1),K=K1,normalize = "C1",as.sparse = FALSE)
# tmp_2=score(matrization_tensor(data$D,2),K=K2,normalize = "C1")
# tmp_3=score(matrization_tensor(data$D,3),K=K3,normalize = "C2")
# errorl1_1=error1_A(data$A1,tmp_1$A_hat)
# errorl1_2=error1_A(data$A2,tmp_2$A_hat)
# errorl1_3=error1_A(data$A3,tmp_3$A_hat)
# hat_G=core_recovery(D,tmp_1$Xi,tmp_2$Xi,tmp_3$Xi,tmp_1$V,tmp_2$V,tmp_3$V,tmp_3$A_star)
# 
# errorl1_core=error1_A(matrization_tensor(data$G,3),matrization_tensor(G,3))
# errorl1_core

write.csv(result_Q,file='tensor_Q2_5_K_3_Q1_1000.csv', row.names=FALSE)
result_Q=error_tmp
res = result_Q %>% 
  filter( mode %in% c("A1","A2","A3","core")) %>%
  group_by(mode, R, Q1, Q2,method,M) %>%
  summarise(l1_error_mean = mean(error),
            l1_A_q50 = quantile(error, 0.5),
            l1_A_q25 = quantile(error, 0.75),
            l1_A_q75 = quantile(error, 0.25)
            
  )

labels=as_labeller(c(`100` = "Q2(Time Factors)=5,R(Vocabulary size) = 100",
                     `A1`="A1",
                     `A2`="A2",
                     `A3`="A3",
                     `core`="core"
))
legend_order <-  c( "HOOI", "Tracy",  "Olga","Tracy_Olga","LDA","NTD")#, "AWR", "LDA")
my_colors <-  c( "dodgerblue",  "red","red","red","chartreuse2","pink")
labels_n <- c( "HOOI", "Tracy",  "Olga","Tracy_Olga","LDA","NTD")
ggplot(res%>% 
         filter(mode %in% c("A1","A2","A3","core"),
                R %in% c(100,1000)
                ,method %in% c("Tracy","Olga","Tracy_Olga","NTD","HOOI")#,"Tracy","Olga","Tracy_Olga"
                ),
       aes(x=Q1, y=(l1_error_mean),color=method)) +
  geom_line(linewidth=0.5) +# 
  geom_point(size=1.1)+
  scale_color_manual(values = my_colors, breaks = legend_order,
                     labels = labels_n) +
  #scale_y_log10() +
  scale_x_log10() +
  geom_errorbar(aes(ymin = l1_A_q25, ymax = l1_A_q75), width = 0.1, alpha=0.4) +
  geom_point() + facet_grid(mode~R, scales="free",labeller=as_labeller(labels))+
  xlab("Q1 (Number of Documents)") +  labs(colour="Method") + 
  ylab("l1_error")


ggplot(result_Q%>% 
         filter(mode %in% c("A2","A3","core"),
                R %in% c(100)
                ,method %in% c("LDA","NTD","Ours")),
       aes(x=Q1, y=(time),color=method)) +
  geom_line(linewidth=1) +# 
  geom_point(size=1.2)+
  #scale_y_log10() +
  scale_x_log10() +
  #geom_errorbar(aes(ymin = l1_A_q25, ymax = l1_A_q75), width = 0.1, alpha=0.4) +
  #geom_point() + facet_grid(mode~R, scales="free") 
  facet_grid(.~R, scales="free",labeller=as_labeller(labels))+
  xlab("Q1 (Number of Documents)") +  labs(colour="Method") + 
  ylab("Execution Time \n(Seconds)")
# 