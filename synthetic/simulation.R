##### more synthetic experiment

D= t(rdiric(2,rep(0.8,50)))
data=matrix(0,50,300)
data[,1:15]=matrix(rep(D[,1],times=15),nrow=50)
data[,16:30]=matrix(rep(D[,2],times=15),nrow=50)
data[,31:60]=data[,1:30]
data[,61:90]=data[,1:30]
data[,91:120]=data[,1:30]
data[,121:150]=data[,1:30]

data[,151:300]=data[,1:150]

before=rdiric(1,rep(0.5,25))*sum(data[1:25,1])
before2=rdiric(1,rep(0.5,25))*sum(data[1:25,16])

data[1:25,1:15]=matrix(rep(before,times=15),nrow=25)
data[1:25,16:30]=matrix(rep(before2,times=15),nrow=25)
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


tensor=array(t(data),dim=c(30,10,50))
plot_slice(tensor,2)

Ytrue=as.tensor(tensor)

noise <- matrix(rnorm(50*300,sd=0.01),50,300) # Generate random numbers
datanoise=data+noise
datanoise=datanoise/colSums(datanoise)
tensor=array(t(datanoise),dim=c(30,10,50))
Y=as.tensor(tensor)
Y3=matrization_tensor(Y,3) # first 10 is dim2* first from dim 1

heatmap_matrix(Y3)
##nonnegative Tucker decomposition
NTD_result=NTD(Y,rank=c(2,2,4))

heatmap_matrix(t(NTD_result$A$A1))

heatmap_matrix(t(NTD_result$A$A2))
heatmap_matrix(t(NTD_result$A$A3))

plot_slice(NTD_result$S@data,1)
hatY=tensor_create(NTD_result$S,t(NTD_result$A$A1),t(NTD_result$A$A2),t(NTD_result$A$A3))
print(l2_error(hatY@data,Ytrue@data))

## nonnegative CP decomposition
NTF_result=NTF(Y,rank=4)
heatmap_matrix(t(NTF_result$A[[1]]))

heatmap_matrix(t(NTF_result$A[[2]]))
heatmap_matrix(t(NTF_result$A[[3]]))
print(NTF_result$S)

A=t(NTF_result$A[[1]])
B=t(NTF_result$A[[2]])
C=t(NTF_result$A[[3]])
tensor_dims <- c(nrow(A), nrow(B), nrow(C))
cp_tensor <- array(0, dim = tensor_dims)
for (r in 1:length(core_values)) {
  rank_one_tensor <- outer(A[, r], B[, r])
  rank_one_tensor <- array(apply(rank_one_tensor, 1:2, function(x) outer(x, C[, r])), dim = tensor_dims)
  cp_tensor <- cp_tensor + 1* rank_one_tensor
}

print(l2_error(cp_tensor,Ytrue@data))

## matrix
M=100
matrix_data=t(Y3)*M
lda_data=matrix(unlist(lapply(matrix_data, as.integer)), nrow = nrow(matrix_data))
LDA_results=LDA(lda_data,k=4,method = 'VEM',control=list(seed=1234))
lda_A=exp(t(LDA_results@beta))
lda_W=t(LDA_results@gamma)
heatmap_matrix(lda_A)
heatmap_matrix(lda_W)
hatY=lda_A%*%lda_W

print(l2_error(hatY,matrization_tensor(Ytrue,3)))


###our method
ours_results=score(Y,K1=2,K2=2,K3=4,M=M,normalize="Ours")

heatmap_matrix(ours_results$hatA1)
heatmap_matrix(ours_results$hatA2)
heatmap_matrix(ours_results$hatA3)


plot_slice(ours_results$hatcore@data,1)
hatY=tensor_create(ours_results$hatcore,ours_results$hatA1,ours_results$hatA2,ours_results$hatA3)
print(l2_error(hatY@data,Ytrue@data))


heatmap(t(data), 
        Rowv = NA, Colv = NA, # Disable clustering
        col = heat.colors(256), # Color scheme
        scale = "none", # Do not scale rows
        margins = c(5,5), # Adjust margins
        xlab = "Columns", ylab = "Rows", # Axis labels
        main = "Heatmap of Matrix Data") # Title








library(ggplot2)
library(reshape2) # For melt function

plot_slice <- function(tensor, k=1) {
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
  
  # Plotting
  g=ggplot(df_long, aes(x = Y, y = X, fill = Value)) +
    geom_tile() +  # Create the heatmap
    scale_fill_viridis_c() +  # Use a color scale that's visually appealing for heatmaps
    facet_wrap(~Slice, ncol = 2) +  # Facet by slice, adjust ncol for desired layout
    labs(x = "X Dimension", y = "Y Dimension", fill = "Value") +
    theme_minimal() +  # Use a minimal theme
    theme(axis.text.x = element_text(angle = 45, hjust = 1), # Improve x-axis label readability
          strip.background = element_rect(fill = "lightblue"), # Customize facet label background
          strip.text = element_text(face = "bold"))  # Bold facet labels
  
  
  print(g)
}


heatmap_matrix<- function(matrix_data){
  # Convert matrix to data frame in long format
  df <- as.data.frame(matrix_data) %>%
    rownames_to_column("Row") %>%
    pivot_longer(cols = -Row, names_to = "Column", values_to = "Value") %>%
    mutate(Row = as.numeric(Row), Column = as.numeric(gsub("V", "", Column))) # Convert Row and Column to numeric
  
  # Create heatmap
  g=ggplot(df, aes(x = Column, y = Row, fill = Value)) +
    geom_tile() +
    scale_fill_viridis_c() +  # Us
    labs(x = "X Dimension", y = "Y Dimension", fill = "Value") +
    theme_minimal() +  # Use a minimal theme
    theme(axis.text.x = element_text(angle = 45, hjust = 1), # Improve x-axis label readability
          strip.background = element_rect(fill = "lightblue"), # Customize facet label background
          strip.text = element_text(face = "bold"))  # Bold facet labels
  
  
  print(g)
}
