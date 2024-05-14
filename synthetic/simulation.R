##### more synthetic experiment
source("D:/yatingliu/CODE/tensor-topic-modeling/our_method.R")
set.seed(1234)
D= t(rdiric(2,rep(0.8,50)))
data=matrix(0,50,300)
data[,1:15]=matrix(rep(D[,1],times=15),nrow=50)
data[,16:30]=matrix(rep(D[,2],times=15),nrow=50)
data[,31:60]=data[,1:30]
data[,61:90]=data[,1:30]
data[,91:120]=data[,1:30]
data[,121:150]=data[,1:30]

data[,151:300]=data[,1:150]

before=rdiric(1,rep(0.8,50))#rdiric(1,rep(0.5,25))*sum(data[1:25,1])
before2=rdiric(1,rep(0.5,25))*sum(data[1:25,16])

#data[26:50,1:15]=matrix(rep(before2,times=15),nrow=25)
data[,16:30]=matrix(rep(before,times=15),nrow=50)
data[,31:60]=data[,1:30]
data[,61:90]=data[,1:30]
data[,91:120]=data[,1:30]
data[,121:150]=data[,1:30]
data1=data
tensor1=array(t(data1),dim=c(30,10,50))
plot_slice(tensor1,2)

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
plot_slice(tensor_true,2,"Mode 1","Mode 3")

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
Y=tensorization(Y3,mode=3,Q1,Q2,length(vocab))
tensor=Y@data


#noise <- abs(matrix(rnorm(50*300,sd=0.05),50,300) )# Generate random numbers
#datanoise=data+noise
#datanoise=datanoise/colSums(datanoise)

#tensor=array(t(datanoise),dim=c(30,10,50))
plot_slice(tensor,2)
Y=as.tensor(tensor)
Y3=matrization_tensor(Y,3) # first 10 is dim2* first from dim 1

heatmap_matrix(t(Y3))
##nonnegative Tucker decomposition
NTD_result=NTD(Y/M,rank=c(2,2,4),algorithm="KL",nmf.algorithm = "KL")

NTD_A1=t(NTD_result$A$A1)
NTD_A1=NTD_A1/rowSums(NTD_A1)
heatmap_matrix(NTD_A1,"Groups","Mode 1")

NTD_A2=t(NTD_result$A$A2)
NTD_A2=NTD_A2/rowSums(NTD_A2)
heatmap_matrix2(NTD_A2,"Classes","Mode 2")

NTD_A3=t(NTD_result$A$A3)
NTD_A3=NTD_A3/colSums(NTD_A3)
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
