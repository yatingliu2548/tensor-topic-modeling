library(readr)
source("D:yatingliu/CODE/tensor-topic-modeling/our_method.R")
CNN_Articels_clean <- read_csv("D:yatingliu/CNN_Articels_clean.csv/CNN_Articels_clean.csv")
CNN=CNN_Articels_clean
CNN$date=as.Date(CNN$`Date published`)
library(stringr)
CNN$headlinetext <- stringi::stri_replace_first_regex(CNN$`Headline`, "\\(CNN Business\\).*", "")

library(dplyr)
covid_related <- grepl("Covid", CNN$`Article text`, ignore.case = TRUE)
contain_COVID=CNN[covid_related, ]
table(contain_COVID$Category)


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
  filter(Category %in% c("business","sport","politics","news"))%>%
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



us_before_covid<- contain_us %>%
  filter(Category %in% c("business","politics","sport","health"),
         date <as.Date("2019-12-31"),
         !Index %in% contain_COVID$Index,
         #Index %in% contain_us$Index,
         !Index %in% contain_us_china$Index)%>%
  group_by(Category)%>%
  sample_n(size=40,replace = FALSE)%>%
  ungroup()%>% arrange(Category,Section,date)

us_during_covid<- contain_us %>%
  filter(Category %in% c("business","politics","sport","health"),
         date >as.Date("2019-12-31"),
         Index %in% contain_COVID$Index,
         #Index %in% contain_us$Index,
         !Index %in% contain_us_china$Index)%>%
  group_by(Category)%>%
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






during_covid<- CNN %>%
  filter(Category %in% c("business","politics","sport","news"),
         date >as.Date("2019-12-31"),
         Index %in% contain_COVID$Index )%>%
  group_by(Category)%>%
  sample_n(size=150,replace=FALSE)%>%
  ungroup()%>% arrange(Category,Section,date)


CNN_selected=c()
# Define the number of rows to alternate and total rows in each dataframe
chunk_size <- 5
total_rows <- nrow(us_before_covid)  # Assuming dfA and dfB have the same number of rows

# Loop through each chunk

for (i in seq(1, total_rows, by = chunk_size)) {
  # Bind 10 rows from dfA
  CNN_selected <- rbind(CNN_selected, us_china_before_covid[i:(i + chunk_size - 1), ])
  # Bind 10 rows from dfB
  CNN_selected <- rbind(CNN_selected, us_china_during_covid[i:(i + chunk_size - 1), ])
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
dim(dtm_matrix)


dtm_tensor=tensorization(t(dtm_matrix/(selected_data$word_count)),3,Q1=96,Q2=10,Q3=dim(dtm_matrix)[2])



#######starting analysis
means_M=mean(selected_data$word_count)
K1=4
K2=2
K3=2
TTM=score(dtm_tensor,K1=K1,K2=K2,K3=K3,M=means_M,normalize="Ours")
hatA1=TTM$hatA1
hatA2=TTM$hatA2
hatA3=TTM$hatA3
hatcore=TTM$hatcore
hatcore_matrix=matrization_tensor(hatcore,3)

heatmap_matrix(TTM$hatA1)
heatmap_matrix(TTM$hatA2)
heatmap_matrix(TTM$hatA3)
hatA3_2=TTM$hatA3
rownames(hatA3)<-colnames(dtm_matrix)
plot_words_per_group(hatA3,words=20) #
plot_slice(TTM$hatcore@data,2)



LDA_results=LDA(dtm_matrix,k=4,control = list(seed = 1234), method = 'VEM')
lda_A=exp(t(LDA_results@beta))
lda_W=t(LDA_results@gamma)
rownames(lda_A)<-colnames(dtm_matrix)
plot_words_per_group(lda_A,words=10) 
heatmap_matrix(lda_A)
heatmap_matrix(t(lda_W))


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