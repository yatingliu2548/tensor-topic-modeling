library(readr)
#source("C:/Users/yichugang/Downloads/Hotel_Reviews.csv")
source("C:/Users/yichugang/Desktop/tensor-topic-modeling/VH_algo.R")
source("C:/Users/yichugang/Desktop/tensor-topic-modeling/run_experiments.R")
source("C:/Users/yichugang/Desktop/tensor-topic-modeling/data_generation.R")
source("C:/Users/yichugang/Desktop/tensor-topic-modeling/our_method.R")
hotel_reviews <- read_csv("C:/Users/yichugang/Downloads/Hotel_Reviews.csv")

df_split <- str_split(hotel_reviews$Tags, pattern = "', '", simplify = TRUE)

# Remove the brackets and extra spaces
df_split <- trimws(gsub("[\\[\\]']", "", df_split))

# Create new dataframe from the split data
new_df <- as.data.frame(df_split)
names(new_df) <- c("Trip_Type", "Group", "Room_Type", "Duration", "Submission_Method","unknown")
hotel_reviews=as.data.frame(cbind(hotel_reviews,new_df))
# Extract numbers from the Duration column
hotel_reviews$Duration_Number <- as.numeric(gsub("[^0-9]", "", hotel_reviews$Duration))
hotel_reviews =subset(hotel_reviews, !is.na(Duration_Number))
hotel_reviews =subset(hotel_reviews, !Group %in% c( "Leisure trip","Business trip" ))
hotel_names=unique(hotel_reviews$Hotel_Name)

K1=5
hotel_names_select=hotel_names[(table(hotel_reviews$Hotel_Name)>500)[hotel_names]]
hotels=sample(hotel_names_select,K1)
hotel_reviews=as.data.frame(hotel_reviews)
reviews <-as.data.frame( hotel_reviews[hotel_reviews$Hotel_Name %in% hotels,])


reviews=reviews %>%
  group_by(Hotel_Name)%>%
  sample_n(size=300,replace=FALSE)%>%ungroup()

data_long <- pivot_longer(reviews, cols = c(Negative_Review, Positive_Review), names_to = "Alttitude", values_to = "Reviews")

Negative_reviews=data_long %>%
  filter(Alttitude %in% c("Negative_Review"))%>%
  #group_by(Hotel_Name)%>%
  #sample_n(size=300,replace = FALSE)%>%
  #ungroup()%>% 
  arrange(Hotel_Name,Group,Trip_Type,Reviewer_Nationality,Review_Date)
  

Positive_reviews=data_long %>%
  filter(Alttitude %in% c("Positive_Review"))%>%
  arrange(Hotel_Name,Group,Trip_Type,Reviewer_Nationality,Review_Date)
table(Positive_reviews$Hotel_Name)

data_selected=c()
# Define the number of rows to alternate and total rows in each dataframe
chunk_size <- 5
total_rows <- nrow(Positive_reviews)#nrow(us_before_covid)  # Assuming dfA and dfB have the same number of rows

# Loop through each chunk

for (i in seq(1, total_rows, by = chunk_size)) {
  # Bind 10 rows from dfA
  data_selected <- rbind(data_selected, Positive_reviews[i:(i + chunk_size - 1), ])
  # Bind 10 rows from dfB
  data_selected <- rbind(data_selected, Negative_reviews[i:(i + chunk_size - 1), ])
}

data_selected$Reviews <- gsub("[^A-Za-z0-9]", " ", data_selected$Reviews )

data_selected$word_count <- sapply(strsplit(data_selected$Reviews, "\\s+"), function(x) if (is.null(x)) NA else length(x))


corpus <- Corpus(VectorSource(data_selected$Reviews))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))

dtm <- DocumentTermMatrix(corpus)
dim(dtm)
dtm_matrix=as.matrix(dtm)
term_freq <- colSums(dtm_matrix)
# Filter terms with frequency >= 5
#term_freq_select <- term_freq[term_freq >= 5 ]
# Create a data frame with term names and their frequencies
df <- data.frame(term = names(term_freq), freq = term_freq)


# Order the data frame by frequency in descending order
df <- df[order(-df$freq), ]
#dtm_matrix<- dtm_matrix[,term_freq >= 5 & term_freq <=8000]
sum(colSums(dtm_matrix)==0)
dim(dtm_matrix)
#dtm_matrix=dtm_matrix/(selected_data$word_count)

Q1=300
Q2=10
dtm_tensor=tensorization(t(dtm_matrix/(data_selected$word_count)),3,Q1=Q1,Q2=Q2,Q3=dim(dtm_matrix)[2])




means_M=mean(data_selected$word_count)
K1=K1
K2=2
K3=4
TTM=score(dtm_tensor,K1=K1,K2=K2,K3=K3,M=means_M,normalize="Ours",threshold = TRUE)
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
