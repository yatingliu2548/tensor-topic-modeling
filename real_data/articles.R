
library(reshape2)
library(reshape)
library(dplyr)
library(tidyverse)
library(alto)
library(ggplot2)
theme_set(theme_bw(base_size = 14))

setwd("C:/Users/yichg/yating/tensor-topic-modeling/tensor-topic-modeling/real_data")
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
library(readr)



##clean the data

articles= read_csv("C:/Users/yichg/Downloads/sampled_arxiv_papers_2000_2024_500.csv")
articles$text=paste0(articles$title," ",articles$abstract)

data=articles%>%arrange(category,cat_detail,classification,year)

# Function to clean the string and extract the last author
get_last_author <- function(author_str) {
  # Remove square brackets and single quotes
  cleaned_str <- gsub("\\[|\\]|'", "", author_str)
  # Split the string by commas and trim whitespace
  author_list <- trimws(unlist(strsplit(cleaned_str, ", ")))
  # Return the last author
  return(tail(author_list, n = 1))
}

# Apply the function to the 'authors' column
data$last_author <- sapply(data$clean_authors, get_last_author)

data=data%>%filter(category %in% c("cs","math","physics","stat"))%>%arrange(category,cat_detail,classification,year)
df_with_sequence <- data %>%
  group_by(year, category) %>%
  mutate(sequence = row_number()) %>%
  arrange(category,classification,year,sequence,cat_detail)


table(df_with_sequence$classification,df_with_sequence$category)

table(df_with_sequence$year,df_with_sequence$category)
df_with_sequence$word_count <- sapply(strsplit(df_with_sequence$text, "\\s+"), function(x) if (is.null(x)) NA else length(x))





stop_words=c("found","going","man","like","time","years","week","including",'say','said','says','hong','told','tell','one','new','may','year','month','date','day','those', 'on', 'own', '’ve', 'yourselves', 'around', 'between', 'four', 'been', 'alone', 'off', 'am', 'then', 'other', 'can', 'regarding', 'hereafter', 'front', 'too', 'used', 'wherein', '‘ll', 'doing', 'everything', 'up', 'onto', 'never', 'either', 'how', 'before', 'anyway', 'since', 'through', 'amount', 'now', 'he', 'was', 'have', 'into', 'because', 'not', 'therefore', 'they', 'n’t', 'even', 'whom', 'it', 'see', 'somewhere', 'thereupon', 'nothing', 'whereas', 'much', 'whenever', 'seem', 'until', 'whereby', 'at', 'also', 'some', 'last', 'than', 'get', 'already', 'our', 'once', 'will', 'noone', "'m", 'that', 'what', 'thus', 'no', 'myself', 'out', 'next', 'whatever', 'although', 'though', 'which', 'would', 'therein', 'nor', 'somehow', 'whereupon', 'besides', 'whoever', 'ourselves', 'few', 'did', 'without', 'third', 'anything', 'twelve', 'against', 'while', 'twenty', 'if', 'however', 'herself', 'when', 'may', 'ours', 'six', 'done', 'seems', 'else', 'call', 'perhaps', 'had', 'nevertheless', 'where', 'otherwise', 'still', 'within', 'its', 'for', 'together', 'elsewhere', 'throughout', 'of', 'others', 'show', '’s', 'anywhere', 'anyhow', 'as', 'are', 'the', 'hence', 'something', 'hereby', 'nowhere', 'latterly', 'say', 'does', 'neither', 'his', 'go', 'forty', 'put', 'their', 'by', 'namely', 'could', 'five', 'unless', 'itself', 'is', 'nine', 'whereafter', 'down', 'bottom', 'thereby', 'such', 'both', 'she', 'become', 'whole', 'who', 'yourself', 'every', 'thru', 'except', 'very', 'several', 'among', 'being', 'be', 'mine', 'further', 'n‘t', 'here', 'during', 'why', 'with', 'just', "'s", 'becomes', '’ll', 'about', 'a', 'using', 'seeming', "'d", "'ll", "'re", 'due', 'wherever', 'beforehand', 'fifty', 'becoming', 'might', 'amongst', 'my', 'empty', 'thence', 'thereafter', 'almost', 'least', 'someone', 'often', 'from', 'keep', 'him', 'or', '‘m', 'top', 'her', 'nobody', 'sometime', 'across', '‘s', '’re', 'hundred', 'only', 'via', 'name', 'eight', 'three', 'back', 'to', 'all', 'became', 'move', 'me', 'we', 'formerly', 'so', 'i', 'whence', 'under', 'always', 'himself', 'in', 'herein', 'more', 'after', 'themselves', 'you', 'above', 'sixty', 'them', 'your', 'made', 'indeed', 'most', 'everywhere', 'fifteen', 'but', 'must', 'along', 'beside', 'hers', 'side', 'former', 'anyone', 'full', 'has', 'yours', 'whose', 'behind', 'please', 'ten', 'seemed', 'sometimes', 'should', 'over', 'take', 'each', 'same', 'rather', 'really', 'latter', 'and', 'ca', 'hereupon', 'part', 'per', 'eleven', 'ever', '‘re', 'enough', "n't", 'again', '‘d', 'us', 'yet', 'moreover', 'mostly', 'one', 'meanwhile', 'whither', 'there', 'toward', '’m', "'ve", '’d', 'give', 'do', 'an', 'quite', 'these', 'everyone', 'towards', 'this', 'cannot', 'afterwards', 'beyond', 'make', 'were', 'whether', 'well', 'another', 'below', 'first', 'upon', 'any', 'none', 'many', 'serious', 'various', 're', 'two', 'less', '‘ve')

corpus <- Corpus(VectorSource(df_with_sequence$text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
remove_custom_stopwords <- function(text, stopwords) {
  removeWords(text, stopwords)
}
corpus <- tm_map(corpus, content_transformer(remove_custom_stopwords), stopwords = stop_words)

dtm <- DocumentTermMatrix(corpus)
dim(dtm)
dtm_matrix=as.matrix(dtm)
term_freq <- colSums(dtm_matrix)
# Filter terms with frequency >= 5
term_freq_select <- term_freq[term_freq >= 3 ]
# Create a data frame with term names and their frequencies
df <- data.frame(term = names(term_freq_select), freq = term_freq_select)

# Order the data frame by frequency in descending order
df <- df[order(-df$freq), ]

dtm_matrix<- dtm_matrix[,term_freq >= 5 & term_freq <=8000]
sum(colSums(dtm_matrix)==0)
dim(dtm_matrix)
#dtm_matrix=dtm_matrix/(selected_data$word_count)

Q1=dim(dtm_matrix)[1]/length(unique(df_with_sequence$year))
Q2=length(unique(df_with_sequence$year))
count_tensor=tensorization(t(dtm_matrix),3,Q1=Q1,Q2=Q2,Q3=dim(dtm_matrix)[2])
dtm_tensor=tensorization(t(dtm_matrix/(df_with_sequence$word_count)),3,Q1=Q1,Q2=Q2,Q3=dim(dtm_matrix)[2])
print(dim(dtm_tensor))

##############
align_topics<- function(A, B, dist="cosine", do.plot=TRUE){
  if(dist=="cosine"){
    A_normalized <- normalize_rows(A)
    B_normalized <- normalize_rows(B)
    match = A_normalized %*% t(B_normalized)
    match=  data.frame(match)
    permutation <- solve_LSAP(as.matrix(match), maximum=TRUE)
  }else{
    match = cdist(A, B, metric = "euclidean", p = 1)
    match=  data.frame(match)
    permutation <- solve_LSAP(as.matrix(match), maximum=FALSE)
  }
  
  match_permuted <- match[, permutation]
  if (do.plot){
    par(mar=c(1,1,1,1))
    colnames(match_permuted)= 1:ncol(match_permuted)
    match_permuted["X"] = 1:nrow(match_permuted)
    print(ggplot(pivot_longer(match_permuted, cols=-c("X")))+
            geom_tile(aes(x=X, y=name, fill=value)))
    #match = data.frame((exp(lda_models$k12$beta))%*% t((exp(lda_models_test$k12$beta))))
    
  }
  
  B_permuted=B[permutation,]
  return(list("B_permuted"=B_permuted,permutation=permutation, error=mean(diag(as.matrix(match_permuted))),
              "match" = match_permuted))
}

get_A<-function(A1,A2,A3,CORE,lp=1){
  ours_A1=TTM$hatA1
  ours_A2=TTM$hatA2
  ours_A3=TTM$hatA3
 
  error1=matrix_lp_distance(ours_A1,A1,lp=lp)
  perm1=error1$permutation
  A1=A1%*% perm1
  error2=matrix_lp_distance(ours_A2,A2,lp=lp)
  perm2=error2$permutation
  #print("a2")
  A2=A2%*% perm2
  error3=tryCatch(matrix_lp_distance(ours_A3,A3,lp), error = function(err) {
    message("Error occurred while running the Bayesian method: ", conditionMessage(err), "\nRetrying...")
    return(NULL)  # Return NULL to indicate failure and trigger retry
  }
  )
  if (is.null(error3)){
    hatW=matrization_tensor(CORE,3)%*% kronecker(t(A1),t(A2))
    # print(dim(hatW))
    #print(dim(ours$gamma))
    error3=matrix_lp_distance(t(hatW),ours$gamma)
    #print(dim(ours$gamma))
  }
  perm3=error3$permutation
  # print("a3")
  A3=A3%*% perm3
  hatG3=matrization_tensor(CORE,3)
  G3_PER=(perm3)%*% hatG3%*% kronecker(t(perm1),t(perm2))
  hatcore=tensorization(G3_PER,3,dim(A1)[2],dim(A2)[2],dim(A3)[2])
  return(list(A1=A1,A2=A2,A3=A3,core=hatcore,errorA1=error1$error,errorA2=error2$error))
  
}

plot_words_per_group<- function(matrix,words=10){
  #colnames(matrix) <- paste0("Group","_", colnames(matrix) )
  gene_data <- as.data.frame(matrix) %>%
    rownames_to_column(var = "Names") %>%
    pivot_longer(cols = -Names, names_to = "Group", values_to = "Probability")
  
  top_genes <- gene_data  %>%
    group_by(Group)%>%
    top_n(n = words, wt = Probability) %>%
    ungroup()
  
  # Plot
  ggplot(top_genes, aes(x = Group, y = Names, size = Probability)) +
    geom_point(shape = 21, fill = "skyblue",alpha = 0.6) +  # Adjust alpha for transparency, if desired
    scale_size_continuous(range = c(0.1, 5)) +  # Adjust the size range for bubbles
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
    labs(title = "",
         x = "Group_Category",
         y = "",
         size = "Probability")
}

#######starting analysis
means_M=mean(df_with_sequence$word_count)
K1=2
K2=3
K3=4
TTM=score(dtm_tensor,K1=K1,K2=K2,K3=K3,M=means_M,normalize="Ours",threshold = TRUE,scatterplot = TRUE)
hatA1=TTM$hatA1
hatA2=TTM$hatA2
hatA3=TTM$hatA3
hatcore=TTM$hatcore
hatcore_matrix=matrization_tensor(hatcore,3)

ours_A3=TTM$hatA3


heatmap_matrix(TTM$hatA1,"Groups","Mode 1",trans="identity")
heatmap_matrix2(TTM$hatA2,"Categories","Mode 2",trans="identity")
heatmap_matrix(TTM$hatA3,"Topics","Mode 3")
hatA3_2=TTM$hatA3
rownames(hatA3)<-colnames(dtm_matrix)
plot_words_per_group(hatA3,words=10) #
plot_slice(TTM$hatcore@data,2,"Groups","Topics",TRUE)
# 
# save(TTM, file = "ours_arvix.RData")
# saveRDS(TTM, "ours_arvix.rds")

###########tensor LDA
tlda_a=tensor_lda(tensorization(t(dtm_matrix),3,Q1=Q1,Q2=Q2,Q3=dim(dtm_matrix)[2]),K1,K2,K3=K3)
#rownames(W_tlda)=rownames(counts_select)
#plot_dirichlet(tensorlda_full[[K3]]$gamma,Topic_K=K3,word="v")
#plot_dirichlet(tlda_a$A1,Topic_K=K1,by="Subject_m",word="g",guide="none")
heatmap_matrix(tlda_a$A1,"Groups","Subject_m")
#plot_dirichlet(tlda_a$A2,Topic_K=K2,by="expected_status2",word="t",guide="none")
heatmap_matrix2(tlda_a$A2,"Phases","Menstrual cycle")
plot_slice(tlda_a$core,2,"Groups of Subject","Topics",yes=T)
tlda_A3=tlda_a$A3
rownames(tlda_A3)<-colnames(dtm_matrix)
plot_words_per_group(t(align_topics(t(ours_A3),t(tlda_A3),dist = "cosine")$B_permuted),words=10) #

# save(tlda_a, file = "tlda_arvix.RData")
# 
# saveRDS(tlda_a, "tlda_arvix.rds")
# tlda_arvix <- readRDS("C:/Users/yichg/yating/tensor-topic-modeling/tensor-topic-modeling/real_data/tlda_arvix.rds")


#######matrix lda
Y3=t(dtm_matrix)
LDA_results=LDA(t(Y3),k=K3,control = list(seed =1234), method = 'VEM')
lda_A=exp(t(LDA_results@beta))
lda_W=t(LDA_results@gamma)
heatmap_matrix(lda_A,"Topics","Mode 3: Product ID",guide="none",trans="identity")
heatmap_matrix(t(lda_W),"Topics","Modes 1 & 2: Customer i at time t",trans="identity",guide="none")
#hatY=lda_A%*%lda_W

heatmap_matrix(t(lda_W),"Topics","Modes 1 & 2",guide="none",trans="identity")
W1_LDA=matrization_tensor(tensorization(lda_W,3,30,10,dim(lda_W)[1]),1)
A1_LDA=spectral_clustering(W1_LDA %*% t(W1_LDA),K=2,mix=T)
heatmap_matrix(A1_LDA,"Groups","Mode 1: customer ID",guide="none",trans="identity")
W2_LDA=matrization_tensor(tensorization((lda_W),3,30,10,dim(lda_W)[1]),2)
A2_LDA=spectral_clustering(W2_LDA %*% t(W2_LDA),K=2,mix=T)
heatmap_matrix2(A2_LDA,"Events","Mode 2: time slice",guide="none",trans="identity")
G_LDA=compute_G_from_WA(kronecker(A1_LDA,A2_LDA),t(lda_W))
plot_slice(tensorization(t(G_LDA),3,2,2,dim(G_LDA)[2])@data,2,"Groups","Topics",yes=T,trans="identity")

lda_A3=lda_A
rownames(lda_A3)<-colnames(dtm_matrix)
plot_words_per_group(t(align_topics(t(ours_A3),t(lda_A3),dist = "cosine")$B_permuted),words=10) #

#saveRDS(LDA_results, "lda_arvix.rds")

######SLDA
SLDA <- stm(documents = Matrix(as.matrix(as.data.frame(t(Y3))), sparse = TRUE),
            K = K3, prevalence =~ classification,
            max.em.its = 5,
            data = df_with_sequence,
            init.type = "Spectral")

# save(my_list, file = "my_list.RData")

W_SLDA_sim=SLDA$theta
A_SLDA_sim=exp(SLDA$beta$logbeta[[1]])
print(l1_error(t(A_SLDA_sim)%*%t(W_SLDA_sim),D3))

slda_A3=t(A_SLDA_sim)
rownames(slda_A3)<-colnames(dtm_matrix)
plot_words_per_group(t(align_topics(t(ours_A3),t(slda_A3),dist = "cosine")$B_permuted),words=10) #
saveRDS(SLDA, "slda_arvix.rds")

#colnames(A_SLDA_sim)=SLDA$vocab

heatmap_matrix(t(A_SLDA_sim),"Topics","Mode 3: Product ID",guide="none",trans="identity")
heatmap_matrix(W_SLDA_sim,"Topics","Modes 1 & 2: Customer i at time t",trans="identity")
W1_SLDA=matrization_tensor(tensorization(t(W_SLDA_sim),3,30,10,dim(W_SLDA_sim)[2]),1)
A1_SLDA=spectral_clustering(W1_SLDA %*% t(W1_SLDA),K=2,mix=T)

heatmap_matrix(A1_SLDA,"Groups","Mode 1: customer ID",guide="none",trans="identity")

W2_SLDA=matrization_tensor(tensorization(t(W_SLDA_sim),3,30,10,dim(W_SLDA_sim)[2]),2)
A2_SLDA=spectral_clustering(W2_SLDA %*% t(W2_SLDA),K=2,mix=T)

heatmap_matrix2(A2_SLDA,"Events","Mode 2: time slice",guide="none",trans="identity")
G_SLDA=compute_G_from_WA(kronecker(A1_SLDA,A2_SLDA),W_SLDA_sim)

plot_slice(tensorization(t(G_SLDA),3,2,2,dim(G_SLDA)[2])@data,2,"Groups","Topics",yes=T,trans="identity")






#####NTD
NTD_result=NTD(dtm_tensor,rank=c(K1,K2,K3),algorithm = "KL")

NTD_A1=t(NTD_result$A$A1)
NTD_A1=NTD_A1/rowSums(NTD_A1)
heatmap_matrix(NTD_A1,"Groups","Mode 1: customer ID",guide="none",trans="identity")

NTD_A2=t(NTD_result$A$A2)
NTD_A2=NTD_A2/rowSums(NTD_A2)
heatmap_matrix2(NTD_A2,"Events","Mode 2: time slice",guide="none",trans="identity")

NTD_A3=t(NTD_result$A$A3)
NTD_A3=NTD_A3/colSums(NTD_A3)
heatmap_matrix(NTD_A3,"Topics","Mode 3: product ID",guide="none",trans="identity")

NTD_G=NTD_result$S
NTD_W=kronecker(NTD_A1,NTD_A2)%*%t(matrization_tensor(NTD_G,3))
heatmap_matrix(NTD_W,"Topics","Modes 1 & 2",guide="none",trans="identity")
NTD_G_3=matrization_tensor(NTD_G,3)
NTD_G_3=NTD_G_3/colSums(NTD_G_3)
NTD_G=tensorization(NTD_G_3,3,2,2,3)
plot_slice(NTD_G@data,2,"Groups","Topics",TRUE,trans="identity")




rownames(NTD_A3)<-colnames(dtm_matrix)
plot_words_per_group(t(align_topics(t(ours_A3),t(NTD_A3),dist = "cosine")$B_permuted),words=10) #
saveRDS(NTD_result, "NTD_arvix.rds")

