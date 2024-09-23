
library(reshape2)
library(reshape)
library(dplyr)
library(tidyverse)
library(alto)
library(ggplot2)
theme_set(theme_bw(base_size = 14))

setwd("C:/Users/yichg/yating/tensor-topic-modeling/tensor-topic-modeling")
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

articles= read_csv("C:/Users/yichg/Downloads/selected_arxiv_papers_2010_2024.CSV")
articles$text=paste0(articles$title," ",articles$abstract)

data=articles%>%arrange(category,year)

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

data=data%>%filter(category %in% c("cs","math","physics","stat"))%>%arrange(selected_author,year)
df_with_sequence <- data %>%
  group_by(year, selected_author) %>%
  mutate(sequence = row_number()) %>%
  arrange(selected_author,sequence,year)



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
dtm_tensor=tensorization(t(dtm_matrix/(df_with_sequence$word_count)),3,Q1=Q1,Q2=Q2,Q3=dim(dtm_matrix)[2])
print(dim(dtm_tensor))


#######starting analysis
means_M=mean(df_with_sequence$word_count)
K1=3
K2=2
K3=3
TTM=score(dtm_tensor,K1=K1,K2=K2,K3=K3,M=means_M,normalize="Ours",threshold = TRUE,scatterplot = TRUE)
hatA1=TTM$hatA1
hatA2=TTM$hatA2
hatA3=TTM$hatA3
hatcore=TTM$hatcore
hatcore_matrix=matrization_tensor(hatcore,3)




heatmap_matrix(TTM$hatA1,"Groups","Mode 1",trans="identity")
heatmap_matrix2(TTM$hatA2,"Categories","Mode 2",trans="identity")
heatmap_matrix(TTM$hatA3,"Topics","Mode 3")
hatA3_2=TTM$hatA3
rownames(hatA3)<-colnames(dtm_matrix)
plot_words_per_group(hatA3,words=10) #
plot_slice(TTM$hatcore@data,2,"Groups","Topics",TRUE)
