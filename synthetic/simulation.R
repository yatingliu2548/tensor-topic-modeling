##### more synthetic experiment
#source("C:/Users/建新/Desktop/tensor-topic-modeling/tensor-topic-modeling/our_method.R")
#source("C:/Users/建新/Desktop/tensor-topic-modeling/tensor-topic-modeling/data_generation.R")

#source("C:/Users/建新/Desktop/tensor-topic-modeling/tensor-topic-modeling/VH_algo.R")
#source("D:/yatingliu/CODE/tensor-topic-modeling/analysis_functions.R")
#source("D:/yatingliu/CODE/tensor-topic-modeling/SLDA.R")
library(reshape2)
library(reshape)
library(dplyr)
library(tidyverse)
library(alto)
library(ggplot2)
theme_set(theme_bw(base_size = 14))

setwd("C:/Users/yichg/yating/tensor-topic-modeling")
source("our_method.R")
source("data_generation.R")
source("analysis_function.R")
source("our_method.R")
source("data_generation.R")
source("tensor_operations.R")
source("run_experiments.R")
source("VH_algo.R")
source("SLDA.R")
source("SLDA.R")
source("bayesian.R")
source("tensor_lda.R")
library(nnTensor)
theme_set(theme_bw(base_size = 14))

set.seed(2024)
#Generate D
generation<-function(p,K=3,alpha_dirichlet = 1, 
                     n_max_zipf=5 * 1e3, 
                     a_zipf=1,
                     offset_zipf = 2.7,
                     n_anchors=1, delta_anchor=0.1, 
                     N=500, 
                     seed=123, vary_by_topic=FALSE,
                     sparsity = TRUE){
  set.seed(seed)
  if (sparsity){
    if (n_anchors >0){
      A = matrix(0, nrow=K, ncol=p)
      for (k in 1:K){
        A[k, ((k-1)*n_anchors +1) : (k * n_anchors)] = delta_anchor
      }
      if(vary_by_topic){
        for (k in 1:K){
          resample_index = sample(1: (p-n_anchors * K), (p-n_anchors * K))
          A[k,(K * n_anchors+1):p] <- sapply(1/(resample_index + offset_zipf)^a_zipf, function(u){rexp(1, u)})
        }
        
      }else{
        A[,(K * n_anchors+1):p ] <- sapply(1/(1: (p-n_anchors * K) + offset_zipf)^a_zipf, function(u){rexp(K, u)})
      }
      A = t(A)
      A[(K * n_anchors+1):p, ] = A[(K * n_anchors+1):p, ] %*% diag(((1 - apply(A[1:(K * n_anchors),],2,sum)) )/apply(A[(K * n_anchors+1):p, ], 2, sum)) 
    }else{
      A <- sapply(1/(1:p + offset_zipf)^a_zipf, function(u){rexp(K, u)})
      A = t(A)
      A = A %*% diag(1/apply(A, 2, sum)) 
    }
    
  }else{
    if (n_anchors >0){
      A = matrix(0, nrow=K, ncol=p)
      for (k in 1:K){
        A[k, ((k-1)*n_anchors +1) : (k * n_anchors)] = delta_anchor
      }
      A[, (K * n_anchors+1):p ] <- matrix(runif(n=K * (p  - K * n_anchors)), nrow = K)
      A = t(A)
      A[(K * n_anchors+1):p, ] = A[(K * n_anchors+1):p, ] %*% diag(((1 - apply(A[1:(K * n_anchors),],2,sum)) )/apply(A[(K * n_anchors+1):p, ], 2, sum)) 
    }else{
      A <- matrix(runif(n=K * (p)), ncol = K)
      A = A %*% diag(1/apply(A, 2, sum)) 
    }
  }
  
  return(A)
}
Data=generation(50)
D=Data[,1:2]
data=matrix(0,50,300)
data[,1:15]=matrix(rep(D[,1],times=15),nrow=50)
data[,16:30]=matrix(rep(D[,2],times=15),nrow=50)
data[,31:60]=data[,1:30]
data[,61:90]=data[,1:30]
data[,91:120]=data[,1:30]
data[,121:150]=data[,1:30]

data[,151:300]=data[,1:150]

before=rdiric(1,rep(0.1,50))#rdiric(1,rep(0.5,25))*sum(data[1:25,1])
before=Data[1:25,3]/sum(Data[1:25,3])*sum(D[26:50,2])
#before2=rdiric(1,rep(0.5,25))*sum(data[1:25,16])

data[26:50,16:30]=matrix(rep(before,times=15),nrow=25)
#data[,16:30]=matrix(rep(before,times=15),nrow=50)
data[,31:60]=data[,1:30]
data[,61:90]=data[,1:30]
data[,91:120]=data[,1:30]
data[,121:150]=data[,1:30]
data1=data
tensor1=array(t(data1),dim=c(30,10,50))
plot_slice(tensor1,2,"class","mode",option="viridis",limits=c(0,0.5))

data[,11:20]=data[,1:10]*0.5+data[,21:30]*0.5
data[,31:60]=data[,1:30]
data[,61:90]=data[,1:30]
data[,91:120]=data[,1:30]
data[,121:150]=data[,1:30]

data[,161:170]=data[,151:160]*0.5+data[,171:180]*0.5
data[,181:210]=data[,151:180]
data[,211:240]=data[,151:180]
data[,241:270]=data[,151:180]
data[,271:300]=data[,151:180]


tensor_true=array(t(data),dim=c(30,10,50))
Y_true=as.tensor(tensor_true)
plot_slice(tensor_true,2,"Mode 1","Mode 3",option="viridis",limits=c(0,max(data)),yes = F,trans = "sqrt")

D0=as.tensor(tensor_true)
D3=matrization_tensor(D0,3)
D_true=D0@data
#D0 = A %*% W
Q1=30
Q2=10
M=100
Y3 <- sapply(1:(Q1*Q2), function(i){rmultinom(1, M, D3[,i])})
D3=D3[which(apply(Y3,1, sum) >0 ),]
Y3=Y3[which(apply(Y3,1, sum) >0 ),]
#A3= A3[which(apply(Y3,1, sum) >0 ),]%*% diag(1/apply(A3[which(apply(Y3,1, sum) >0 ),], 2, sum))
vocab =which(apply(Y3,1, sum) >0 )
tensor=tensorization(D3,mode=3,Q1,Q2,length(vocab))
print(dim(D3))
Y=tensorization(Y3,mode=3,Q1,Q2,length(vocab))
tensor=Y@data
plot_slice(Y@data/M,2,"Mode 1","Mode 3",option="viridis",limits=c(0,0.1),yes = F,trans = "sqrt")

#D0_true=tensorization(D3,mode=3,Q1,Q2,length(vocab))
K3=3
#noise <- abs(matrix(rnorm(50*300,sd=0.05),50,300) )# Generate random numbers
#datanoise=data+noise
#datanoise=datanoise/colSums(datanoise)

#tensor=array(t(datanoise),dim=c(30,10,50))
plot_slice(tensor,2,"mode1","mode2")
Y=as.tensor(tensor)
Y3=matrization_tensor(Y,3) # first 10 is dim2* first from dim 1

#write.csv(Y3, file = "Y3.csv", row.names = F)
#write.csv(D3, file = "D3.csv", row.names = F)

heatmap_matrix(t(Y3),"Topics","Mode 3")



# tensor LDA
#matrix_data=t(D3)*M
#lda_data=matrix(unlist(lapply(matrix_data, as.integer)), nrow = nrow(matrix_data))
#D0_true_int=tensorization(t(lda_data),3,Q1,Q2,50)

tlda <-tensor_lda(Y@data,K1=2,K2=2,K3=K3)
#tlda_sampling_d0=tensor_lda(D0,K1=2,K2=2,K3=4,use_vb = FALSE)
#tlda=tlda_vb
heatmap_matrix(as.matrix(tlda$A1),"Groups","Mode 1",guide="none",trans="sqrt")
heatmap_matrix2(tlda$A2,"Classes","Mode 2",guide="none",trans="sqrt")
heatmap_matrix(tlda$A3,"Topics","Mode 3",guide="none",trans="sqrt")
tlda_W=kronecker(tlda$A1,tlda$A2)%*%t(matricization(tlda$core,3))
heatmap_matrix(tlda_W,"Topics","Modes 1 & 2")
plot_slice(tlda$core,2,"Groups","Topics",TRUE,trans="sqrt")
hatY=tensor_create(tensorization( matricization(tlda$core,3),3,2,2,K3),tlda$A1,tlda$A2,tlda$A3)
print(l1_error(hatY@data,tensorization(D3,3,30,10,dim(D3)[1])@data))


###our method
ours_results=score(Y/M,K1=2,K2=2,K3=K3,M=M,normalize="Ours")

heatmap_matrix(ours_results$hatA1,"Groups","Mode 1",trans="sqrt",guide="none")
heatmap_matrix2(ours_results$hatA2,"Classes","Mode 2",guide="none",trans="sqrt")
heatmap_matrix(ours_results$hatA3,"Topics","Mode 3",guide="none",trans="sqrt")

our_W=kronecker(ours_results$hatA1,ours_results$hatA2)%*%t(matrization_tensor(ours_results$hatcore,3))
heatmap_matrix(our_W,"Topics","Modes 1 & 2")
plot_slice(ours_results$hatcore@data,2,"Groups","Topics",yes=TRUE,trans="sqrt")
hatY=tensor_create(ours_results$hatcore,ours_results$hatA1,ours_results$hatA2,ours_results$hatA3)
print(l1_error(hatY@data,tensorization(D3,3,30,10,dim(D3)[1])@data))
print(matrix_lp_distance(matricization(hatY,3),D3,1)$error)

##nonnegative Tucker decomposition
NTD_result=NTD(Y/M,rank=c(2,2,3),algorithm = "KL")

NTD_A1=t(NTD_result$A$A1)
NTD_A1=NTD_A1/rowSums(NTD_A1)
heatmap_matrix(NTD_A1,"Groups","Mode 1",guide="none",trans="sqrt")

NTD_A2=t(NTD_result$A$A2)
NTD_A2=NTD_A2/rowSums(NTD_A2)
heatmap_matrix2(NTD_A2,"Classes","Mode 2",guide="none",trans="sqrt")

NTD_A3=t(NTD_result$A$A3)
NTD_A3=NTD_A3/colSums(NTD_A3)
heatmap_matrix(NTD_A3,"Topics","Mode 3",guide="none",trans="sqrt")

NTD_G=NTD_result$S
NTD_W=kronecker(NTD_A1,NTD_A2)%*%t(matrization_tensor(NTD_G,3))
heatmap_matrix(NTD_W,"Topics","Modes 1 & 2",guide="none",trans="sqrt")
NTD_G_3=matrization_tensor(NTD_G,3)
NTD_G_3=NTD_G_3/colSums(NTD_G_3)
NTD_G=tensorization(NTD_G_3,3,2,2,3)
plot_slice(NTD_G@data,2,"Groups","Topics",TRUE,trans="sqrt")
hatY=tensor_create(NTD_G,NTD_A1,NTD_A2,NTD_A3)
print(l1_error(hatY@data,tensorization(D3,3,30,10,dim(D3)[1])@data))
print(matrix_lp_distance(matricization(hatY,3),D3,1)$error)

## nonnegative CP decomposition
NTF_result=cp(Y,num_components =K3)
U1=abs(NTF_result$U[[1]])
U2=abs(NTF_result$U[[2]])
U3=abs(NTF_result$U[[3]])

heatmap_matrix(U1/rowSums(U1),"Topics","Mode 1",guide="none",trans="sqrt")


heatmap_matrix2(U2/rowSums(U2),"Topics","Mode 2",guide="none",trans="sqrt")
heatmap_matrix(U3/colSums(U3),"Topics","Mode 3",guide="none",trans="sqrt")
print(NTF_result$lambdas)

A=t(NTF_result$U[[1]])
B=t(NTF_result$U[[2]])
C=t(NTF_result$A[[3]])
core_values=NTF_result$lambdas
YY=get_cp(U1/rowSums(U1),U2/rowSums(U2),U3/colSums(U3),NTF_result$lambdas)

print(l1_error(YY,tensorization(D3,3,30,10,dim(D3)[1])@data))

## matrix
M=100
#matrix_data=t(Y3)*M
#lda_data=matrix(unlist(lapply(matrix_data, as.integer)), nrow = nrow(matrix_data))
LDA_results=LDA(t(Y3),k=K3,control = list(seed = 1234), method = 'VEM')
lda_A=exp(t(LDA_results@beta))
lda_W=t(LDA_results@gamma)
heatmap_matrix(lda_A,"Topics","Mode 3",guide="none",trans="sqrt")
heatmap_matrix(t(lda_W),"Topics","Modes 1 & 2",guide="none",trans="sqrt")
#hatY=lda_A%*%lda_W
#print(l1_error(hatY,matrization_tensor(D0,3)))

heatmap_matrix(t(lda_W),"Topics","Modes 1 & 2",guide="none",trans="sqrt")
W1_LDA=matrization_tensor(tensorization(lda_W,3,30,10,dim(lda_W)[1]),1)
A1_LDA=spectral_clustering(W1_LDA %*% t(W1_LDA),K=2,mix=T)
heatmap_matrix(A1_LDA,"Groups","Mode 1",guide="none",trans="sqrt")
W2_LDA=matrization_tensor(tensorization((lda_W),3,30,10,dim(lda_W)[1]),2)
A2_LDA=spectral_clustering(W2_LDA %*% t(W2_LDA),K=2,mix=T)
heatmap_matrix2(A2_LDA,"Classes","Mode 1",guide="none",trans="sqrt")
G_LDA=compute_G_from_WA(kronecker(A1_LDA,A2_LDA),t(lda_W))
plot_slice(tensorization(t(G_LDA),3,2,2,dim(G_LDA)[2])@data,2,"Groups of Subject","Topics",yes=T,trans="sqrt")

print(l1_error(tensor_create(tensorization(t(G_LDA),3,2,2,dim(G_LDA)[2]),A1_LDA,A2_LDA,lda_A)@data,tensorization(D3,3,30,10,dim(D3)[1])@data))


##########SLDA
race <- rep(c("Asian", "Asian American", "American"), each = 100)
# Create the time vector
time <- rep(rep(c(rep(1, 5), rep(2, 5)), times = 3),each=10)
# Combine into a data frame
df <- data.frame(race = race, time = time)
SLDA <- stm(documents = Matrix(as.matrix(as.data.frame(t(Y3))), sparse = TRUE),
                       K = K3, prevalence =~ race*time,
                       max.em.its = 10,
                       data = df,
                       init.type = "Spectral")

##finding optimal K in SLDA
#Y3_doc=convert_dtm_to_stm_format(as.matrix(as.data.frame(t(Y3))))$documents

#findingk <- searchK(documents=Y3_doc,vocab=SLDA$vocab, K = c(3:20),
#                    prevalence =~race*time, data = df, verbose=FALSE)

#plot(findingk)
##heldout-likelihood high, better, then we can see K=5 get highest plot
W_SLDA_sim=SLDA$theta
A_SLDA_sim=exp(SLDA$beta$logbeta[[1]])

#colnames(A_SLDA_sim)=SLDA$vocab

heatmap_matrix(t(A_SLDA_sim),"Topics","Mode 3",guide="none",trans="sqrt")
heatmap_matrix(W_SLDA_sim,"Topics","Modes 1 & 2",guide="none",trans="sqrt")
W1_SLDA=matrization_tensor(tensorization(t(W_SLDA_sim),3,30,10,dim(W_SLDA_sim)[2]),1)
A1_SLDA=spectral_clustering(W1_SLDA %*% t(W1_SLDA),K=2,mix=T)

heatmap_matrix(A1_SLDA,"Groups","Mode 1",guide="none",trans="sqrt")

W2_SLDA=matrization_tensor(tensorization(t(W_SLDA_sim),3,30,10,dim(W_SLDA_sim)[2]),2)
A2_SLDA=spectral_clustering(W2_SLDA %*% t(W2_SLDA),K=2,mix=T)

heatmap_matrix2(A2_SLDA,"Classes","Mode 2",guide="none",trans="sqrt")
G_SLDA=compute_G_from_WA(kronecker(A1_SLDA,A2_SLDA),W_SLDA_sim)

plot_slice(tensorization(t(G_SLDA),3,2,2,dim(G_SLDA)[2])@data,2,"Groups of Subject","Topics",yes=T,trans="sqrt")
print(l1_error(tensor_create(tensorization(t(G_SLDA),3,2,2,dim(G_SLDA)[2]),A1_SLDA,A2_SLDA,t(A_SLDA_sim))@data,tensorization(D3,3,30,10,dim(D3)[1])@data))



heatmap(t(data),
        Rowv = NA, Colv = NA, # Disable clustering
        col = heat.colors(256), # Color scheme
        scale = "none", # Do not scale rows
        margins = c(5,5), # Adjust margins
        xlab = "Columns", ylab = "Rows", # Axis labels
        main = "Heatmap of Matrix Data") # Title


errors=c()

for (i in 1:30){
  NTD_result=NTD(Y/M,rank=c(2,2,4),algorithm="KL",nmf.algorithm = "KL")
  NTD_D=tensor_create(NTD_result$S,t(NTD_result$A$A1),t(NTD_result$A$A2),t(NTD_result$A$A3))
  errors=rbind(errors,error_sim(D0@data,NTD_D@data,"NTD"))
  NTF_result=NTF(Y/M,rank=4)
  NNCPD_D=get_cp(t(NTF_result$A[[1]]),t(NTF_result$A[[2]]),t(NTF_result$A[[3]]),NTF_result$S)
  errors=rbind(errors,error_sim(D0@data,NNCPD_D,"NNCPD"))
  LDA_results=LDA(t(Y3),k=4, control = list(seed=i), method = 'VEM')
  lda_A=exp(t(LDA_results@beta))
  lda_W=t(LDA_results@gamma)
  hatY=lda_A%*%lda_W

  errors=rbind(errors,error_sim(D0@data,tensorization(hatY,3,30,10,50)@data,"LDA"))
  ours_results=score(Y/M,K1=2,K2=2,K3=4,M=M,normalize="Ours")
  our_D=tensor_create(ours_results$hatcore,ours_results$hatA1,ours_results$hatA2,ours_results$hatA3)
  errors=rbind(errors,error_sim(D0@data,our_D@data,"ours"))
}


ggplot(errors %>% filter(method %in% c("NTD","ours","LDA")),
       aes(x=method,y=l1,fill=method))+
  geom_boxplot()+
  theme_minimal()

