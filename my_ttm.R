##### Synthetic experiments
library(VGAM)
library("nnTensor")
setwd("~/Documents/tensor-topic-modeling/")
set.seed(1234)

K = 4 #### nb of topics
p = 50   #### nb of words
T = 10 ### nb of time points
K_T = 2  #### nb of time "topics"
n = 30 ### nb of samples
K_n = 3  #### nb of reviewer "personae"

#### Generate the topics
D= t(rdiric(K,rep(0.8, p)))

#### Generate the data matrix
core =  array(0, dim = c(K_n, K_T, K))
core  = rdiric(n=K_n* K_T, shape = rep(1, K), dimension=K)
core = array(core, dim = c(K_n, K_T, K))

A1 = rdiric(n=n, shape = rep(1, K_n), dimension=K_n)
A2 = rdiric(n=T, shape = rep(1, K_T), dimension=K_T)
A3 = t(rdiric(n=K, shape = rep(1, p), dimension=p))

library(einsum)
library(rTensor)
library(tidyverse)
data = einsum("ni, ijk -> njk", A1,core)
data = einsum("Tj, njk -> nTk", A2,data)
data = einsum("Rk, nTk -> nTR", A3,data)
#### Have to make sure that the resulting matrix has the right properties
apply(data, 1, sum)
apply(data, 2, sum)

D0 = as.tensor(data)


matrization_tensor <- function(G, mode){
  tensordata = G@data
  if (mode == 1){
    matrix_G = aperm(tensordata, c(1, 3, 2)) %>% 
      as.vector() %>% 
      matrix(nrow = dim(G)[1], 
             byrow = FALSE)
    
  }
  if (mode == 2){
    matrix_G = aperm(tensordata, c(2, 3, 1)) %>% 
      as.vector() %>% 
      matrix(nrow = dim(G)[2],
             byrow = FALSE)
    
  }
  if (mode == 3){
    matrix_G = aperm(tensordata, c(3, 2, 1)) %>% 
      as.vector() %>% 
      matrix(nrow = dim(G)[3], 
             byrow = FALSE)
    
  }
  return(matrix_G)
}


nb_words_per_doc = 100
D3=matrization_tensor(D0,3)
Y =array(0, dim= c(n, T, p))
for (i in 1:n){
  for (j in 1:T){
    Y[i,j,] = rmultinom(size=nb_words_per_doc, n=1, prob = D0@data[i,j,])
  }
}

##nonnegative Tucker decomposition


NTD_result=NTD(as.tensor(Y)/nb_words_per_doc,rank=c(K_n,K_T,K),algorithm="KL",nmf.algorithm = "KL")

NTD_A1=t(NTD_result$A$A1)
NTD_A1=NTD_A1/rowSums(NTD_A1)
heatmap_matrix(NTD_A1,"Groups","Mode 1")
heatmap_matrix(A1,"Groups","Mode 1")

NTD_A2=t(NTD_result$A$A2)
NTD_A2=NTD_A2/rowSums(NTD_A2)
heatmap_matrix(NTD_A2,"Classes","Mode 2")
heatmap_matrix(A2,"Claire","Mode 2")

NTD_A3=t(NTD_result$A$A3)
NTD_A3=NTD_A3/colSums(NTD_A3)


compute_error_stats

heatmap_matrix(NTD_A3,"Topics","Mode 3")

NTD_G=NTD_result$S
NTD_G_3=matrization_tensor(NTD_G,3)
NTD_G_3=NTD_G_3/colSums(NTD_G_3)
NTD_G=tensorization(NTD_G_3,3,2,2,4)
plot_slice(NTD_G@data,2,"Groups","Topics",TRUE)
hatY=tensor_create(NTD_G,NTD_A1,NTD_A2,NTD_A3)
print(l1_error(hatY@data,D0@data))

## nonnegative CP decomposition
NTF_result=cp(Y,num_components =3)
U1=abs(NTF_result$U[[1]])
U2=abs(NTF_result$U[[2]])
U3=abs(NTF_result$U[[3]])

heatmap_matrix(U1/rowSums(U1),"Topics","Mode 1")

heatmap_matrix2(U2/rowSums(U2),"Topics","Mode 2")
heatmap_matrix(U3/colSums(U3),"Topics","Mode 3")
print(NTF_result$S)

A=t(NTF_result$A[[1]])
B=t(NTF_result$A[[2]])
C=t(NTF_result$A[[3]])
core_values=NTF_result$S

print(l2_error(get_cp(A,B,C,core_values),Ytrue@data))

## matrix
M=100
matrix_data=t(Y3)*M
lda_data=matrix(unlist(lapply(matrix_data, as.integer)), nrow = nrow(matrix_data))
LDA_results=LDA(t(Y3),k=4,control = list(seed = 234), method = 'VEM')
lda_A=exp(t(LDA_results@beta))
lda_W=t(LDA_results@gamma)
heatmap_matrix(lda_A,"Topics","Mode 3")
heatmap_matrix(t(lda_W),"Topics","Modes 1 & 2")
hatY=lda_A%*%lda_W

print(l1_error(hatY,matrization_tensor(D0,3)))


###our method
ours_results=score(Y/M,K1=2,K2=2,K3=4,M=M,normalize="Ours")

heatmap_matrix(ours_results$hatA1,"Groups","Mode 1")
heatmap_matrix2(ours_results$hatA2,"Classes","Mode 2")
heatmap_matrix(ours_results$hatA3,"Topics","Mode 3")


plot_slice(ours_results$hatcore@data,2,"Groups","Topics",TRUE)
hatY=tensor_create(ours_results$hatcore,ours_results$hatA1,ours_results$hatA2,ours_results$hatA3)
print(l1_error(hatY@data,D0@data))


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



library(ggplot2)
library(reshape2) # For melt function


plot_slice <- function(tensor, k=1,xlab,ylab,yes=FALSE) {
  df_long <- data.frame()
  # Extract a slice from the 2nd dimension
  # Convert each slice (along the second dimension) of the tensor to long format
  for (i in 1:dim(tensor)[k]) {
    if (k==1){
      ts=tensor[i, ,]
    }else if (k==2){
      ts=tensor[,i ,]
    }else{
      ts=tensor[, ,i ]
    }
    slice_df <- as.data.frame(ts) %>%
      mutate(X = row_number(), Slice = i) %>%
      pivot_longer(cols = -c(X, Slice), names_to = "Y", values_to = "Value") %>%
      mutate(Y = as.numeric(gsub("V", "", Y))) # Convert Y to numeric
    
    df_long <- rbind(df_long, slice_df) # Bind each slice's data frame to the main data frame
  }
  
  # Adjust factors for plotting
  df_long$Slice <- factor(df_long$Slice)
  
  if (yes==TRUE){
    df_long$X=as.factor(df_long$X)
    df_long$Y=as.factor(df_long$Y)
  }
  # Plotting
  g=ggplot(df_long, aes(x = X, y =Y, fill = Value)) +
    geom_tile() +  # Create the heatmap
    scale_fill_viridis_c(limits=c(0,1),option="H",trans="sqrt") +  # Use a color scale that's visually appealing for heatmaps
    facet_wrap(~Slice, ncol = 10) +  # Facet by slice, adjust ncol for desired layout
    labs(x = xlab, y = ylab, fill = "") +
    theme_minimal() +  # Use a minimal theme
    theme(axis.text.x = element_text(angle = 45, hjust = 1), # Improvehttp://127.0.0.1:18569/graphics/5001c0ec-4c59-4d8d-8e0d-a42a99b7917e.png x-axis label readability
          #strip.background = element_rect(fill = "lightblue"), # Customize facet label background
          strip.text = element_text(face = "bold"))  # Bold facet labels
  
  
  print(g)
}


heatmap_matrix<- function(matrix_data,xlab,ylab){
  # Convert matrix to data frame in long format
  df <- as.data.frame(matrix_data) %>%
    rownames_to_column("Row") %>%
    pivot_longer(cols = -Row, names_to = "Column", values_to = "Value") %>%
    mutate(Row = as.numeric(Row), Column = as.numeric(gsub("V", "", Column))) # Convert Row and Column to numeric
  
  # Create heatmap
  g=ggplot(df, aes(x = as.factor(Column), y =( Row), fill = Value)) +
    geom_tile() +
    scale_fill_viridis_c(limits=c(0,1),option="H",trans="sqrt") +  # Use a color scale that's visually appealing for heatmaps
    # scale_fill_viridis_c() +  # Us
    labs(x = xlab, y = ylab, fill = "") +
    #scale_y_continuous(breaks=seq(0,max(as.numeric(df$Row)),by=5))+
    theme_minimal() +  # Use a minimal theme
    theme(#axis.text.x = element_text(angle = 45, hjust = 1), # Improve x-axis label readability
      strip.background = element_rect(fill = "lightblue"), # Customize facet label background
      strip.text = element_text(face = "bold"))  # Bold facet labels
  
  
  print(g)
}
heatmap_matrix2<- function(matrix_data,xlab,ylab){
  # Convert matrix to data frame in long format
  df <- as.data.frame(matrix_data) %>%
    rownames_to_column("Row") %>%
    pivot_longer(cols = -Row, names_to = "Column", values_to = "Value") %>%
    mutate(Row = as.numeric(Row), Column = as.numeric(gsub("V", "", Column))) # Convert Row and Column to numeric
  
  # Create heatmap
  g=ggplot(df, aes(x = as.factor(Column), y =( Row), fill = Value)) +
    geom_tile() +
    scale_fill_viridis_c(limits=c(0,1),option="H",trans="sqrt") +  # Use a color scale that's visually appealing for heatmaps
    # scale_fill_viridis_c() +  # Us
    labs(x = xlab, y = ylab, fill = "") +
    scale_y_continuous(breaks=seq(0,max(as.numeric(df$Row)),by=5))+
    theme_minimal() +  # Use a minimal theme
    theme(#axis.text.x = element_text(angle = 45, hjust = 1), # Improve x-axis label readability
      strip.background = element_rect(fill = "lightblue"), # Customize facet label background
      strip.text = element_text(face = "bold"))  # Bold facet labels
  
  
  print(g)
}

error_sim <- function(D,hatD,method=NULL,i){
  error_temp <- data.frame(l1=l1_error(D,hatD),
                           l2=l2_error(D,hatD),
                           method=method,i=i)
  return(error_temp)
}

get_cp=function(A,B,C,core_values){
  
  tensor_dims <- c(nrow(A), nrow(B), nrow(C))
  cp_tensor <- array(0, dim = tensor_dims)
  
  for (r in 1:length(core_values)) {
    rank_one_tensor <- outer(A[, r], B[, r])
    rank_one_tensor <- array(apply(rank_one_tensor, 1:2, function(x) outer(x, C[, r])), dim = tensor_dims)
    cp_tensor <- cp_tensor + 1* rank_one_tensor
  }
  return(cp_tensor)
}




heatmap_matrix<- function(matrix_data,xlab,ylab){
  # Convert matrix to data frame in long format
  df <- as.data.frame(matrix_data) %>%
    rownames_to_column("Row") %>%
    pivot_longer(cols = -Row, names_to = "Column", values_to = "Value") %>%
    mutate(Row = as.numeric(Row), Column = as.numeric(gsub("V", "", Column))) # Convert Row and Column to numeric
  
  # Create heatmap
  g=ggplot(df, aes(x = as.factor(Column), y =( Row), fill = Value)) +
    geom_tile() +
    scale_fill_viridis_c(limits=c(0,1),option="H",trans="sqrt") +  # Use a color scale that's visually appealing for heatmaps
    # scale_fill_viridis_c() +  # Us
    labs(x = xlab, y = ylab, fill = "") +
    #scale_y_continuous(breaks=seq(0,max(as.numeric(df$Row)),by=5))+
    theme_minimal() +  # Use a minimal theme
    theme(#axis.text.x = element_text(angle = 45, hjust = 1), # Improve x-axis label readability
      strip.background = element_rect(fill = "lightblue"), # Customize facet label background
      strip.text = element_text(face = "bold"))  # Bold facet labels
  
  
  print(g)
}
