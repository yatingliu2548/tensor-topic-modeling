library(readr)
CNN_Articels_clean <- read_csv("C:/Users/yichugang/Downloads/CNN_Articels_clean.csv/CNN_Articels_clean.csv")
guardian_articles <- read_csv("C:/Users/yichugang/Downloads/archive (1)/guardian_articles.csv")

CNN=CNN_Articels_clean
gua=guardian_articles

CNN$date=as.Date(CNN$`Date published`)
gua$date=as.Date(gua$webPublicationDate)
gua$Category=gua$sectionName
gua$Journal="gua"
CNN$text=paste0(CNN$Headline," ",CNN$`Article text`)
gua$text=paste0(gua$webTitle," ",gua$bodyContent)
CNN$Journal="CNN"
categories=c("news") #sport","politics","business","travel

CNN=clean_data(CNN,categories)
CNN$Keywords =sub("^.*?,\\s*", "", CNN$Keywords )
categories <- c("sports", "politics", "technology","business","health")

keywords <- list(
  sports = c("football", "basketball", "tennis", "soccer", "olympics", "athletics", "marathon", "world cup", "tournament", "league","sport"),
  politics = c("election", "government", "senate", "congress", "democracy", "legislation", "policy", "president", "political party", "vote","international","war"),
  technology = c("technology", "AI", "artificial intelligence", "software", "hardware", "programming", "coding", "science", "machine", "cybersecurity","tech","auto","mobile"),
  business = c("business", "economy", "market", "investment", "finance", "startup", "entrepreneur", "stock", "revenue", "profit","market","commerce"),
  health = c("health", "medicine", "doctor", "hospital", "nursing", "wellness", "disease", "treatment", "surgery", "vaccine","killed","kill","covid")
)
# Revised count_keywords function to work on individual texts
count_keywords <- function(text, keywords) {
  sum(str_count(text, paste0("\\b(", paste(keywords, collapse = "|"), ")\\b")))
}

# Iterate over each category in 'keywords' and apply the count_keywords function to each text
for(category in names(keywords)) {
  CNN_full[[category]] <- sapply(CNN_full$text, count_keywords, keywords = keywords[[category]])
}
CNN_full$total=rowSums(CNN_full[,17:21])
CNN_full=CNN_full[CNN_full$total!=0,]
CNN_full$categories <- apply(CNN_full[,17:21], 1, function(x) names(CNN_full[,17:21])[which.max(x)])
CNN_full$sec_cate<- paste0(CNN_full$Section,",",CNN_full$categories)
CNN_full =CNN_full%>%
  group_by(YearMon,categories) %>%
  sample_n(size = 1, replace = FALSE) %>%
  ungroup()%>%
  arrange(sec_cate,YearMon)

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
