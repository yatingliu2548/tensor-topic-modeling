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
CNN_Articles_clean <- read_csv("real_data/CNN_Articels_clean.csv")
guardian_articles <- read_csv("real_data/guardian_articles.csv")

CNN=CNN_Articles_clean
CNN$date=as.Date(CNN$`Date published`)
library(stringr)
CNN$headlinetext <- stringi::stri_replace_first_regex(CNN$`Headline`, "\\(CNN Business\\).*", "")


gua=guardian_articles

CNN$date=as.Date(CNN$`Date published`)
gua$date=as.Date(gua$webPublicationDate)
gua$Section=gua$sectionName
gua$Section <- gsub(" news", "", gua$Section)
gua$Journal="gua"
CNN$text=paste0(CNN$Headline," ",CNN$`Article text`)
CNN$text=paste0(CNN$text," ",CNN$Keywords)
gua$text=paste0(gua$webTitle," ",gua$bodyContent)
CNN$Journal="CNN"
categories=c("news") 

CNN$Section[CNN$Section == "china"] <- "asia"
CNN$Section[CNN$Section == "americas"] <- "us"


gua=gua %>% mutate(text = tolower(text),
                   Section=tolower(Section)) 
#gua$Section[gua$Section == "US"] <- "us"
#gua$Section[gua$Section == "UK"] <- "uk"

CNN_full=CNN%>%
  filter(Category %in% c("news"),
         Section %in% c("australia","uk","us"))%>%
  select(c('date',"Section","text","Journal"))#sport","politics","business","travel

gua_full=gua%>%
  filter(
    Section %in% c("australia","uk","us"))%>%
  select(c('date',"Section","text","Journal"))

data_full=as.data.frame(rbind(gua_full,CNN_full))%>%arrange(Section,date)
#CNN=clean_data(CNN,categories)
data_full$text =sub("^.*?,\\s*", "", data_full$text )
data_full$text =sub("CNN", "", data_full$text )
categories <- c("sports", "politics","business","health")

keywords <- list(
  sports = c("football", "basketball", "tennis", "soccer", "olympics", "athletics", "marathon", "world cup", "tournament", "league","sport","air","world"),
  politics = c("election", "government", "senate", "congress", "democracy", "legislation", "policy", "president", "political party", "vote","international","war"),
  #technology = c("technology", "AI", "artificial intelligence", "software", "hardware", "programming", "coding", "science", "machine", "cybersecurity","tech","auto","mobile"),
  business = c("business", "economy", "market", "investment", "finance", "startup", "entrepreneur", "stock", "revenue", "profit","market","commerce","nonprofit"),
  health = c("health", "medicine", "doctor", "hospital", "nursing", "wellness", "disease", "treatment", "surgery", "vaccine","killed","kill","covid")
)
# Revised count_keywords function to work on individual texts
count_keywords <- function(text, keywords) {
  sum(str_count(text, paste0("\\b(", paste(keywords, collapse = "|"), ")\\b")))
}

# Iterate over each category in 'keywords' and apply the count_keywords function to each text
for(category in names(keywords)) {
  data_full[[category]] <- sapply(data_full$text, count_keywords, keywords = keywords[[category]])
}
data_full=as.data.frame(data_full)
len=length(categories)
data_full$total=rowSums(data_full[,(length(names(data_full))-(len-1)):length(names(data_full))])
data_full=data_full[data_full$total!=0,]
data_full$groups <- apply(data_full[,(length(names(data_full))-(len)):(length(names(data_full))-1)], 1, function(x) names(data_full[,(length(names(data_full))-len):(length(names(data_full))-1)])[which.max(x)])
data_full$sec_cate<- paste0(data_full$Section,",",data_full$groups)

data_full1=data_full
data_full=data_full%>%
  group_by(Section)
#CNN_full2 =CNN_full%>%
# filter(Section %in% c("us","uk","europe"))
data_full$Index <- 1:nrow(data_full)


##### extract the news related to covid 19
covid=c("covid","covid-19","coronavirus","quarantine")
get_covid=function(sub,df){
  Index_s=c()
  df=df%>%filter(date>as.Date("2020-01-01"))
  for(i in sub){
    covid_related <- grepl(i, data_full$text, ignore.case = TRUE)
    contain_COVID=df[covid_related, ]
    Index_s=c(Index_s,contain_COVID$Index)
  }
  data=df%>%
    filter(Index %in% Index_s)
  return(data)
}


contain_COVID <-get_covid(covid,data_full)


before2020=data_full%>%filter(date<as.Date("2020-01-01"))
after2020=data_full%>%filter(date>as.Date("2020-01-01"))



#data_full$groups[data_full$groups %in% c("business","sports","technology")] <- "others"
#table(contain_COVID$Category)
set.seed(1234)
during_covid<- data_full %>%
  filter(
    date >as.Date("2020-01-01"),
    Index %in% contain_COVID$Index )%>%
  group_by(groups)%>%
  sample_n(size=200,replace=FALSE)%>%
  ungroup()%>% arrange(sec_cate,Journal,date)


before_covid<- data_full %>%
  filter(
    date <as.Date("2020-01-01"),
    !Index %in% contain_COVID$Index )%>%
  group_by(groups)%>%
  sample_n(size=200,replace=FALSE)%>%
  ungroup()%>% arrange(sec_cate,Journal,date)






###construct data

data_selected=c()

# Define the number of rows to alternate and total rows in each dataframe
chunk_size <- 5
total_rows <- nrow(during_covid)#nrow(us_before_covid)  # Assuming dfA and dfB have the same number of rows

# Loop through each chunk

for (i in seq(1, total_rows, by = chunk_size)) {
  # Bind 10 rows from dfA
  data_selected <- rbind(data_selected, before_covid[i:(i + chunk_size - 1), ])
  # Bind 10 rows from dfB
  data_selected <- rbind(data_selected, during_covid[i:(i + chunk_size - 1), ])
}

selected_data=data_selected
###clean text
selected_data$text <- gsub("CNN", " ", selected_data$text)

selected_data$text<- gsub("[^A-Za-z0-9]", " ", selected_data$text)

# Print the cleaned text
#print(cleaned_text)
# Count the number of words in each row of text_column
selected_data$word_count <- sapply(strsplit(selected_data$text, "\\s+"), function(x) if (is.null(x)) NA else length(x))


stop_words=c('say','said','says','hong','told','tell','one','new','may','year','month','date','day','those', 'on', 'own', '’ve', 'yourselves', 'around', 'between', 'four', 'been', 'alone', 'off', 'am', 'then', 'other', 'can', 'regarding', 'hereafter', 'front', 'too', 'used', 'wherein', '‘ll', 'doing', 'everything', 'up', 'onto', 'never', 'either', 'how', 'before', 'anyway', 'since', 'through', 'amount', 'now', 'he', 'was', 'have', 'into', 'because', 'not', 'therefore', 'they', 'n’t', 'even', 'whom', 'it', 'see', 'somewhere', 'thereupon', 'nothing', 'whereas', 'much', 'whenever', 'seem', 'until', 'whereby', 'at', 'also', 'some', 'last', 'than', 'get', 'already', 'our', 'once', 'will', 'noone', "'m", 'that', 'what', 'thus', 'no', 'myself', 'out', 'next', 'whatever', 'although', 'though', 'which', 'would', 'therein', 'nor', 'somehow', 'whereupon', 'besides', 'whoever', 'ourselves', 'few', 'did', 'without', 'third', 'anything', 'twelve', 'against', 'while', 'twenty', 'if', 'however', 'herself', 'when', 'may', 'ours', 'six', 'done', 'seems', 'else', 'call', 'perhaps', 'had', 'nevertheless', 'where', 'otherwise', 'still', 'within', 'its', 'for', 'together', 'elsewhere', 'throughout', 'of', 'others', 'show', '’s', 'anywhere', 'anyhow', 'as', 'are', 'the', 'hence', 'something', 'hereby', 'nowhere', 'latterly', 'say', 'does', 'neither', 'his', 'go', 'forty', 'put', 'their', 'by', 'namely', 'could', 'five', 'unless', 'itself', 'is', 'nine', 'whereafter', 'down', 'bottom', 'thereby', 'such', 'both', 'she', 'become', 'whole', 'who', 'yourself', 'every', 'thru', 'except', 'very', 'several', 'among', 'being', 'be', 'mine', 'further', 'n‘t', 'here', 'during', 'why', 'with', 'just', "'s", 'becomes', '’ll', 'about', 'a', 'using', 'seeming', "'d", "'ll", "'re", 'due', 'wherever', 'beforehand', 'fifty', 'becoming', 'might', 'amongst', 'my', 'empty', 'thence', 'thereafter', 'almost', 'least', 'someone', 'often', 'from', 'keep', 'him', 'or', '‘m', 'top', 'her', 'nobody', 'sometime', 'across', '‘s', '’re', 'hundred', 'only', 'via', 'name', 'eight', 'three', 'back', 'to', 'all', 'became', 'move', 'me', 'we', 'formerly', 'so', 'i', 'whence', 'under', 'always', 'himself', 'in', 'herein', 'more', 'after', 'themselves', 'you', 'above', 'sixty', 'them', 'your', 'made', 'indeed', 'most', 'everywhere', 'fifteen', 'but', 'must', 'along', 'beside', 'hers', 'side', 'former', 'anyone', 'full', 'has', 'yours', 'whose', 'behind', 'please', 'ten', 'seemed', 'sometimes', 'should', 'over', 'take', 'each', 'same', 'rather', 'really', 'latter', 'and', 'ca', 'hereupon', 'part', 'per', 'eleven', 'ever', '‘re', 'enough', "n't", 'again', '‘d', 'us', 'yet', 'moreover', 'mostly', 'one', 'meanwhile', 'whither', 'there', 'toward', '’m', "'ve", '’d', 'give', 'do', 'an', 'quite', 'these', 'everyone', 'towards', 'this', 'cannot', 'afterwards', 'beyond', 'make', 'were', 'whether', 'well', 'another', 'below', 'first', 'upon', 'any', 'none', 'many', 'serious', 'various', 're', 'two', 'less', '‘ve')

corpus <- Corpus(VectorSource(selected_data$text))
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
term_freq_select <- term_freq[term_freq >= 5 ]
# Create a data frame with term names and their frequencies
df <- data.frame(term = names(term_freq_select), freq = term_freq_select)

# Order the data frame by frequency in descending order
df <- df[order(-df$freq), ]

dtm_matrix<- dtm_matrix[,term_freq >= 5 & term_freq <=8000]
sum(colSums(dtm_matrix)==0)
dim(dtm_matrix)
#dtm_matrix=dtm_matrix/(selected_data$word_count)

Q1=dim(dtm_matrix)[1]/10
Q2=10
dtm_tensor=tensorization(t(dtm_matrix/(selected_data$word_count)),3,Q1=Q1,Q2=Q2,Q3=dim(dtm_matrix)[2])



#######starting analysis
means_M=mean(selected_data$word_count)
K1=3
K2=2
K3=4
TTM=score(dtm_tensor,K1=K1,K2=K2,K3=K3,M=means_M,normalize="Ours",threshold = TRUE,scatterplot = FALSE)
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

tlda=tensor_lda(tensorization(t(dtm_matrix),3,Q1=Q1,Q2=Q2,Q3=dim(dtm_matrix)[2]),K1,K2,K3)

heatmap_matrix(tlda$A1,"Groups","Mode 1",trans="identity")
heatmap_matrix(tlda$A2,"Categories","Mode 2",trans="identity")
heatmap_matrix(tlda$A3,"Topics","Mode 3")
plot_slice(tlda$core,2,"Groups","Topics",TRUE)


matrix=floor(dtm_matrix*(selected_data$word_count))
LDA_results=LDA(dtm_matrix,k=4,control = list(seed = 1234), method = 'VEM')
lda_A=exp(t(LDA_results@beta))
lda_W=t(LDA_results@gamma)
rownames(lda_A) <-colnames(dtm_matrix)
plot_words_per_group((lda_A),words=10) 
heatmap_matrix(as.numeric(lda_A),"Groups","Mode 3")
heatmap_matrix(t(lda_W),"Groups","Mode 1")

# # Create a corpus
# corpus <- Corpus(VectorSource(CNN$Keywords))
# 
# # Preprocess
# corpus <- tm_map(corpus, content_transformer(tolower))
# corpus <- tm_map(corpus, removePunctuation)
# corpus <- tm_map(corpus, removeWords, stopwords("english"))
# 
# # Create a document-term matrix
# dtm <- DocumentTermMatrix(corpus)
# 
# lda_res=LDA(dtm,k=5,method="gibbs")
# W_hat=lda_res@gamma
# df_dict=as.data.frame(dtm$dimnames$Terms)
# A_hat=exp(t(lda_res@beta))

colnames(W_hat)<-categories
CNN_full=cbind(CNN,W_hat)
  
  #######starting analysis
  means_M=mean(selected_data$word_count)
  K1=3
  K2=2
  K3=3
  TTM=score(dtm_tensor,K1=K1,K2=K2,K3=K3,M=means_M,normalize="HOOI",threshold = FALSE,scatterplot = FALSE)
  hatA1=TTM$hatA1
  hatA2=TTM$hatA2
  hatA3=TTM$hatA3
  hatcore=TTM$hatcore
  hatcore_matrix=matrization_tensor(hatcore,3)
  
  heatmap_matrix(TTM$hatA1,"Groups","Mode 1")
  heatmap_matrix(TTM$hatA2,"Categories","Mode 2")
  heatmap_matrix(TTM$hatA3,"Topics","Mode 3")
  hatA3_2=TTM$hatA3
  rownames(hatA3)<-colnames(dtm_matrix)
  plot_words_per_group(hatA3,words=10) #
  plot_slice(TTM$hatcore@data,2,"Groups","Topics",TRUE)
  
  

  
  matrix=floor(dtm_matrix*(selected_data$word_count))
  LDA_results=LDA(dtm_matrix,k=4,control = list(seed = 1234), method = 'VEM')
  lda_A=exp(t(LDA_results@beta))
  lda_W=t(LDA_results@gamma)
  rownames(lda_A) <-colnames(dtm_matrix)
  plot_words_per_group((lda_A),words=10) 
  heatmap_matrix(as.numeric(lda_A),"Groups","Mode 3")
  heatmap_matrix(t(lda_W),"Groups","Mode 1")
  
  # # Create a corpus
# corpus <- Corpus(VectorSource(CNN$Keywords))
# 
# # Preprocess
# corpus <- tm_map(corpus, content_transformer(tolower))
# corpus <- tm_map(corpus, removePunctuation)
# corpus <- tm_map(corpus, removeWords, stopwords("english"))
# 
# # Create a document-term matrix
# dtm <- DocumentTermMatrix(corpus)
# 
# lda_res=LDA(dtm,k=5,method="gibbs")
# W_hat=lda_res@gamma
# df_dict=as.data.frame(dtm$dimnames$Terms)
# A_hat=exp(t(lda_res@beta))

colnames(W_hat)<-categories
CNN_full=cbind(CNN,W_hat)



gua=clean_data(CNN,categories)


full_df <- gua %>%
  left_join(CNN, by = c("YearMon", "Category")) %>%
  mutate(text = ifelse(is.na(text), "Empty", text))

date_seq <- seq(from = as.Date("2018-06-01"), 
                to = as.Date("2022-05-31"), 
                by = "month")
year_month_list <- format(date_seq, "%Y-%m")
year_month_list <- as.vector(year_month_list)

all_dates=rep(year_month_list,each=size)
all_combinations <- expand.grid(YearMon = all_dates, Category = all_sections)

full_df <- all_combinations %>%
  left_join(df_clean, by = c("YearMon", "categories")) %>%
  mutate(text = ifelse(is.na(text), "Empty", text))

clean_data<-function(CNN,categories){
  df_clean<- CNN%>%
    mutate(Year=as.numeric(format(CNN$date,"%Y")),
          YearMon=format(CNN$date,"%Y-%m"),
          Category=tolower(Category))%>%
    filter(Category %in% categories,
           #Section %in% selected_section,
           date >as.Date("2018-06-01"),
           date <as.Date("2022-05-31"))
  
   
  return(df_clean)
}

display_word_weighting <- function(A3, df, words_to_display = 10) {
  
  topic_amt <- ncol(A3)
  word_df <- matrix(0, words_to_display,topic_amt)
  
  for (k in 1:topic_amt) {
    # Get indices of top words
    indices <- sort(A3[,k], decreasing = TRUE, index.return=TRUE)
    
    
    for (i in 1:words_to_display) {
      word_df[i,k] <- paste0(df[indices$ix[i],1], " (",round( A3[indices$ix[i],k], 4), ")")
      
    }
    
  }
  return(word_df)
}



