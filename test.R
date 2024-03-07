Q1list=c(100,250,300,500,1000)
Q2=10
Rlist=c(100,250,500)
K1=3
K2=3
K3=3
M=100
result_Q=c()
replications=1000
error_update <- function(hatA,A,mode,K,Q1,Q2,R){
  error_temp <- data.frame(error=matrix_lp_distance(hatA,A, lp=1),
                           K = K,
                           Q1=Q1,
                           R=R,
                           Q2=Q2,
                           mode=mode)
  return(error_temp)
}
for (rep in 1: replications){
for (i in 1:length(Q1list)){
  for (j in 1:length(Rlist)){
    data = synthetic_dataset_creation(Q1list[i],Q2,Rlist[j], K1,K2,K3, alpha_dirichlet=0.5, n_max_zipf=50000, a_zipf=1,n_anchors=2, delta_anchor=0.1, M=M, seed=123,offset_zipf=2.7,vary_by_topic=FALSE, sparsity = TRUE)
    tmp_1=score(matrization_tensor(data$D,1),K=K1,normalize = "C1",as.sparse = TRUE)
    tmp_2=score(matrization_tensor(data$D,2),K=K2,normalize = "C1")
    tmp_3=score(matrization_tensor(data$D,3),K=K3,normalize = "C2")
    #errorl1_1=error1_A(data$A1,tmp_1$A_hat)
    #errorl1_2=error1_A(data$A2,tmp_2$A_hat)
    #errorl1_3=error1_A(data$A3,tmp_3$A_hat)
    hat_G=core_recovery(data$D,tmp_1$Xi,tmp_2$Xi,tmp_3$Xi,tmp_1$V,tmp_2$V,tmp_3$V,tmp_3$A_star)$G
    hatG3=matrization_tensor(hat_G,3)
    G3=matrization_tensor(data$G,3)
    #errorl1_core=error1_A(matrization_tensor(data$G,3),matrization_tensor(hat_G,3))
    result_Q=rbind(result_Q,
                   error_update(hatA=tmp_1$A_hat,A=data$A1,mode="A1",K=K1,Q1=Q1list[i],R=R[j],Q2=Q2))
    result_Q=rbind(result_Q,
                   error_update(hatA=tmp_2$A_hat,A=data$A2,mode="A2",K=K2,Q1=Q1list[i],R=R[j],Q2=Q2))
    result_Q=rbind(result_Q,
                   error_update(hatA=tmp_3$A_hat,A=data$A3,mode="A3",K=K3,Q1=Q1list[i],R=R[j],Q2=Q2))
    result_Q=rbind(result_Q,
                   error_update(hatA=hatG3,A=G3,mode="A2",K=K3,Q1=Q1list[i],R=R[j],Q2=Q2))
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

res<-  pivot_longer(result_Q, 
                              cols = c(l1_A1_1, l1_A2_1,l1_A3_1,l1_core_1), 
                              names_to = "Method_1", 
                              values_to = "error_1")
res<-  pivot_longer(res, 
                    cols = c(l1_A1_2, l1_A2_2,l1_A3_2,l1_core_2), 
                    names_to = "Method_2", 
                    values_to = "error_2")

ggplot(res, 
       aes(x=Q1, y=log(error_1), color=Method_1)) +
  geom_line()+
  geom_point() + facet_grid(.~R, scales="free") 
