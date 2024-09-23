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
CNN=CNN_Articles_clean
CNN$date=as.Date(CNN$`Date published`)
library(stringr)
CNN$headlinetext <- stringi::stri_replace_first_regex(CNN$`Headline`, "\\(CNN Business\\).*", "")


##### extract the news related to covid 19
library(dplyr)
covid_related <- grepl("Covid", CNN$`Article text`, ignore.case = TRUE)
contain_COVID=CNN[covid_related, ]
table(contain_COVID$Category)

table(contain_COVID[contain_COVID$Category=="news",]$Section)

news=CNN%>%
  filter(Category=="news")
CNN_data=CNN
keywords <- list(
  sports = c("football", "basketball", "tennis", "soccer", "olympics", "athletics", "marathon", "world cup", "tournament", "league","sport","air"),
  politics = c("election", "government", "senate", "congress", "democracy", "legislation", "policy", "president", "political party", "vote","international","war"),
  #technology = c("technology", "AI", "artificial intelligence", "software", "hardware", "programming", "coding", "science", "machine", "cybersecurity","tech","auto","mobile"),
  business = c("business", "economy", "market", "investment", "finance", "startup", "entrepreneur", "stock", "revenue", "profit","market","commerce","nonprofit"),
  health = c("health", "medicine", "doctor", "hospital", "nursing", "wellness", "disease", "treatment", "surgery", "vaccine","killed","kill")
)
# Revised count_keywords function to work on individual texts
count_keywords <- function(text, keywords) {
  sum(str_count(text, paste0("\\b(", paste(keywords, collapse = "|"), ")\\b")))
}

# Iterate over each category in 'keywords' and apply the count_keywords function to each text
for(category in names(keywords)) {
  CNN_data[[category]] <- sapply(CNN_data$text, count_keywords, keywords = keywords[[category]])
}
CNN_data=as.data.frame(CNN_data)
len=length(categories)
CNN_data$total=rowSums(CNN_data[,(length(names(CNN_data))-(len-1)):length(names(CNN_data))])
CNN_data=CNN_data[CNN_data$total!=0,]
CNN_data$groups <- apply(CNN_data[,(length(names(CNN_data))-(len)):(length(names(CNN_data))-1)], 1, function(x) names(CNN_data[,(length(names(CNN_data))-len):(length(names(CNN_data))-1)])[which.max(x)])
CNN_data$sec_cate<- paste0(CNN_data$Category,",",CNN_data$groups)
CNN_data1=CNN_data
CNN_data=CNN_data1
CNN_data=CNN_data%>%
  filter(total>10,
         groups %in% c("sport","politics","business","health"))
#CNN_full2 =CNN_full%>%

contain_COVID2=CNN_data[CNN_data$Index %in% contain_COVID$Index, ]


###select country
asia_related <- grepl(c("china"), CNN$`Article text`, ignore.case = TRUE)
contain_asia=CNN[asia_related, ]
china_covid<-CNN %>%
  filter(Index %in% contain_COVID$Index,
         Index %in%     contain_asia$Index )

us_related <- grepl(c("america"), CNN$`Article text`, ignore.case = TRUE)
contain_us=CNN[us_related, ]
contain_us_china=contain_us%>%
  filter(Index %in% contain_asia$Index )

us_covid<-CNN %>%
  filter(Index %in% contain_COVID$Index,
         Index %in%     contain_us$Index )
us_china_covid=us_covid %>%
  filter(Index %in% china_covid$Index)%>%
  mutate(categories=paste0(Category,",",Section))%>%
  arrange(Category,Section,date)
only_us_covid=us_covid %>%
  filter(!Index %in% us_china_covid$Index)%>%
  mutate(categories=paste0(Category,",",Section))%>%
  filter(Category %in% c("business","sport","politics","europe"))%>%
  group_by(Category)%>%
  sample_n(20)%>%ungroup()%>%
  arrange(Category,Section,date)
only_china_covid=china_covid %>%
  filter(!Index %in% us_china_covid$Index)%>%
  mutate(categories=paste0(Category,",",Section))%>%
  filter(Category %in% c("business","sport","politics","news"))%>%
  group_by(Category)%>%
  sample_n(20)%>%ungroup()%>%
  arrange(Category,Section,date)



before_covid<- CNN %>%
  filter(Category %in% c("business","politics","sport","health"),
        # date <as.Date("2019-12-31"),
         !Index %in% contain_COVID$Index,
         #Index %in% contain_us$Index,
         #!Index %in% contain_us_china$Index
         )%>%
  group_by(Category)%>%
  sample_n(size=40,replace = FALSE)%>%
  ungroup()%>% arrange(Category,Section,date)

during_covid<-contain_COVID2%>%sample_n(size=160,replace = FALSE)%>%
  arrange(sec_cate,date)%>%
  select(colnames(before_covid))

during_covid<- CNN_data %>%
  filter(sec_cate %in% c("business,business","news,politics","sport,sports","health,health"),
         #date >as.Date("2019-12-31"),
         Index %in% contain_COVID$Index,
         #Index %in% contain_us$Index,
         #!Index %in% contain_us_china$Index
         )%>%
  group_by(Categiry)%>%
  sample_n(size=40,replace = FALSE)%>%
  ungroup()%>% arrange(Category,Section,date)

china_before_covid<- CNN %>%
    filter(
           date <as.Date("2019-12-31"),
           !Index %in% contain_COVID$Index,
           Index %in% contain_asia$Index,
           !Index %in% contain_us_china$Index)%>%
  slice_sample(n=160)%>%
  ungroup()%>% arrange(Category,Section,date)

china_during_covid<- CNN %>%
    filter(
           date >as.Date("2019-12-31"),
           Index %in% contain_COVID$Index,
           Index %in% contain_asia$Index,
           !Index %in% contain_us_china$Index)%>%
  slice_sample(n=160)%>%
  ungroup()%>% arrange(Category,Section,date)

us_china_before_covid<- contain_us_china %>%
  filter(
         date <as.Date("2019-12-31"),
         !Index %in% contain_COVID$Index,
         Index %in% contain_us_china$Index)%>%
  slice_sample(n=160)%>%
  ungroup()%>% arrange(Category,Section,date)

us_china_during_covid<- contain_us_china %>%
  filter(
         date >=as.Date("2019-12-31"),
         Index %in% contain_COVID$Index)%>%
  slice_sample(n=160)%>%
  ungroup()%>% arrange(Category,Section,date)





during_covid1<- contain_COVID %>%
  filter(Category %in% c("business","politices","sport","health"),
         date >as.Date("2019-12-31"),
         Index %in% contain_COVID$Index )%>%
  group_by(Category)%>%
  sample_n(size=25,replace=T)%>%
  ungroup()%>% arrange(Category,Section,date)

during_covid2<-contain_COVID%>%filter(!Index %in% during_covid1$Index,!Category %in% c("sport")) %>%sample_n(size=25,replace=FALSE)
during_covid=as.data.frame(rbind(during_covid1,during_covid2)%>% arrange(Category,Section,date))
  
before_covid<- CNN %>%
  filter(Category %in% c("business","politics","sport","health"),
         #date <as.Date("2019-12-31"),
         !Index %in% contain_COVID$Index )%>%
  group_by(Category)%>%
  sample_n(size=25,replace=FALSE)%>%
  ungroup()%>% arrange(Category,Section,date)


CNN_selected=c()
# Define the number of rows to alternate and total rows in each dataframe
chunk_size <- 5
total_rows <- nrow(during_covid)#nrow(us_before_covid)  # Assuming dfA and dfB have the same number of rows

# Loop through each chunk

for (i in seq(1, total_rows, by = chunk_size)) {
  # Bind 10 rows from dfA
  CNN_selected <- rbind(CNN_selected, before_covid[i:(i + 7 - 1), ])
  # Bind 10 rows from dfB
  CNN_selected <- rbind(CNN_selected, during_covid[i:(i + 3 - 1), ])
}

selected_data=CNN_selected
###clean text
selected_data$`Article text` <- gsub("CNN", " ", selected_data$`Article text`)

selected_data$`Article text` <- gsub("[^A-Za-z0-9]", " ", selected_data$`Article text`)

# Print the cleaned text
#print(cleaned_text)
# Count the number of words in each row of text_column
selected_data$word_count <- sapply(strsplit(selected_data$`Article text`, "\\s+"), function(x) if (is.null(x)) NA else length(x))


corpus <- Corpus(VectorSource(selected_data$`Article text`))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))

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
dtm_matrix=dtm_matrix/(selected_data$word_count)

Q1=120
Q2=10
dtm_tensor=tensorization(t(dtm_matrix),3,Q1=Q1,Q2=Q2,Q3=dim(dtm_matrix)[2])



#######starting analysis
means_M=mean(selected_data$word_count)
K1=4
K2=2
K3=4
TTM=score(dtm_tensor,K1=K1,K2=K2,K3=K3,M=means_M,normalize="Ours",threshold = FALSE)
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
LDA_results=LDA(matrix,k=4,control = list(seed = 1234), method = 'VEM')
lda_A=exp(t(LDA_results@beta))
lda_W=t(LDA_results@gamma)
rownames(lda_A) <-colnames(dtm_matrix)
plot_words_per_group((lda_A),words=10)
heatmap_matrix(as.numeric(lda_A),"Groups","Mode 1")
heatmap_matrix(t(lda_W),"Groups","Mode 1")


###

NTD_result=NTD(dtm_tensor,rank=c(2,2,4))

heatmap_matrix(t(NTD_result$A$A1))

heatmap_matrix(t(NTD_result$A$A2))
heatmap_matrix(t(NTD_result$A$A3))

plot_slice(NTD_result$S@data,1)


###
NTF_result=NTF(dtm_tensor,rank=4)
heatmap_matrix(t(NTF_result$A[[1]]))

heatmap_matrix(t(NTF_result$A[[2]]))
heatmap_matrix(t(NTF_result$A[[3]]))
