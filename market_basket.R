setwd("~/Documents/tensor-topic-modeling/")
library(tidyverse)
## other methods
library(nnTensor)
library(tidyverse)
library(topicmodels)
library(tidytext)

source("algorithm.R")
source("bayesian.R")
source("NTD.R")
source("tensor_operations.r")
source("run_experiments.R")
product_count_threshold = 10
quantity_threshold = 300
nb_time_point = 10
### Read in the data
transac = read_csv("~/Downloads/market_basket/transaction_data.csv")
product <- read_csv("~/Downloads/market_basket/product.csv")

#### There are a lot of product, but we could group by subcategories (gets rid of brands + quantity)
transac = left_join(transac,
                    product %>% select(PRODUCT_ID, DEPARTMENT, 
                                       COMMODITY_DESC, SUB_COMMODITY_DESC ) )
transac = transac %>%
  group_by(household_key, WEEK_NO, DAY, BASKET_ID, DEPARTMENT, 
           COMMODITY_DESC, SUB_COMMODITY_DESC) %>%
  summarise(QUANTITY = sum(QUANTITY))

### Check how much products appear
test_product = transac %>% 
  group_by(COMMODITY_DESC, SUB_COMMODITY_DESC) %>%
  summarise(count = n())
quantile(test_product$count, probs = seq(0, 1, 0.1))
### Keep only relevant product
test_product_filtered = test_product %>%
  filter(count > product_count_threshold)
test_product_filtered["ID"]= 1:nrow(test_product_filtered)
write_csv(test_product_filtered, "~/Downloads/product_filtered.csv")
transac2 = left_join(transac,
                     test_product_filtered)
transac2 = transac2 %>%
  filter(!is.na(ID))
transac2 = transac2 %>%
  filter(SUB_COMMODITY_DESC != "GASOLINE-REG UNLEADED")
#### some quantities are just too wide, so need to check that the baskets are reasonable

print(quantile(transac2$QUANTITY, 0.97))
quantile(transac2$QUANTITY, probs = seq(0, 1, 0.01))
print(mean(transac2$QUANTITY < quantity_threshold))
transac2 = transac2 %>%
  filter(QUANTITY < quantity_threshold, 
         QUANTITY > 0)
dim(transac2)
transac2 = transac2 %>%
  select(-c("count"))

transac2 = transac2 %>%
  filter(QUANTITY < quantity_threshold, 
         QUANTITY > 0)


#### Aggregate baskets over a week
transac2 = transac2 %>%
  group_by(household_key, WEEK_NO ,DEPARTMENT,        
           COMMODITY_DESC, SUB_COMMODITY_DESC, ID) %>%
  summarise(QUANTITY=sum(QUANTITY))
#### keep baskets that are large enough
baskets = transac2 %>% 
  group_by(household_key, WEEK_NO) %>%
  summarise(total_size = sum(QUANTITY)) 
quantile(baskets$total_size, probs = seq(0, 1, 0.01))

baskets =baskets %>%
  filter(total_size > 10)

transac_filtered = left_join(transac2,
                             baskets)

transac_filtered = transac_filtered %>%
  filter(!is.na(total_size)) %>%
  select(-c("total_size"))

  
#### Check whether households appear enough
transac_filtered_hh = transac_filtered %>% 
  group_by(household_key) %>%
  summarise(count = n_distinct(WEEK_NO))
hist(transac_filtered_hh$count) ### they don't so we

transac_filtered_hh_filtered = transac_filtered_hh %>%
  filter(count > 31)

transac3 =  transac_filtered %>%
  filter(household_key %in%  transac_filtered_hh_filtered$household_key )

data_temp = transac3 %>%
  ungroup() %>% 
  select(household_key, WEEK_NO, ID, QUANTITY)
data_temp = data_temp %>%
  group_by(household_key, WEEK_NO, ID) %>% 
  summarise_all(sum)

market_data = pivot_wider(data_temp,
                    id_cols = c("household_key", "WEEK_NO"),
                    names_from = "ID",
                     values_from = "QUANTITY",
                    values_fill = 0)

prop_nnz= apply(market_data, 2, function(x){mean(x>0)})
quantile(prop_nnz, probs = seq(0, 1, 0.01))

ordered_p= sort(prop_nnz[3:length(prop_nnz)], decreasing = T, index.return=T)
stop_words = names(prop_nnz)[which(prop_nnz>0.1)]
stop_words = stop_words[3:length(stop_words)]
market_data2 <- market_data[, setdiff(names(market_data), stop_words)]
market_data= market_data2
#### Now we need to create the tensor

temp_data = market_data %>%
  filter(WEEK_NO < 52)

nb_time_point = 26
temp_data$bins <- cut(temp_data$WEEK_NO, breaks = nb_time_point, labels = FALSE)

choose_household = temp_data %>%
  ungroup() %>%
  select(-c(WEEK_NO)) %>%
  pivot_longer(cols = -c("household_key", "bins")) %>%
  group_by(household_key, bins) %>%
  summarise(c=sum(value))
### select all household that have at least one item

choose_household=   pivot_wider(choose_household,
                                id_cols = "household_key",
              names_from = "bins",
              values_from = "c",
              values_fill = 0)

selection =which(apply(choose_household[,2:ncol(choose_household)],1,function(x){sum(x==0)})==0)
print(length(selection))

temp_data = temp_data %>%
  filter(household_key %in% choose_household$household_key[selection])

temp_data2 = market_data %>%
  filter(WEEK_NO > 51)

temp_data2$bins <- cut(temp_data2$WEEK_NO, breaks = nb_time_point, labels = FALSE)

choose_household2 = temp_data2 %>%
  ungroup() %>%
  select(-c(WEEK_NO)) %>%
  pivot_longer(cols = -c("household_key", "bins")) %>%
  group_by(household_key, bins) %>%
  summarise(c=sum(value))
### select all household that have at least one item

choose_household2=   pivot_wider(choose_household2,
                                id_cols = "household_key",
                                names_from = "bins",
                                values_from = "c",
                                values_fill = 0)

selection2 =which(apply(choose_household2[,2:ncol(choose_household2)],1,function(x){sum(x==0)})==0)
temp_data2 = temp_data2 %>%
  filter(household_key %in% choose_household2$household_key[selection2])

temp_data2$household_key = temp_data2$household_key + 10000
temp_data = rbind(temp_data,
                  temp_data2)

#### keep only one point per bin
#### keep only one point per bin

data4analysis = temp_data %>%
  ungroup() %>%
  select(-c( "WEEK_NO")) %>%
  group_by(household_key, bins) %>%
  summarise_all(sum) %>%
  ungroup() 

data4analysis = 
  data4analysis %>%
  arrange(household_key, bins) 
STOP
#data4analysis["total"] = apply(data4analysis[,3:ncol(data4analysis)], 1, sum)

D = tensorization(t(as.matrix(data4analysis[, 3:(ncol(data4analysis))])), mode=3,
                  Q1=nrow(data4analysis)/nb_time_point, Q2=nb_time_point, 
                  Q3 = ncol(data4analysis)-2)
matrix_D = (data4analysis %>% 
              ungroup() %>% 
                   select(-c("household_key", "bins"))%>% 
                   as.matrix())

ncol(data4analysis)
R = ncol(data4analysis)-2
i=100; j=9; mean(as.numeric(matrix_D[(i-1) * nb_time_point + j,1:R]) == D@data[i,j,1:R])

D3 = matricization(D, 3)
print(length(which(apply(D3, 1, sum)== 0)))
print(length(which(apply(D3, 2, sum)== 0)))
words = which(apply(D3, 1, sum)> 0)



D3 = D3[words, ]
D_new =  tensorization(D3, mode=3,
                   Q1=nrow(data4analysis)/nb_time_point, Q2=nb_time_point, Q3 = nrow(D3))
R = nrow(D3)
i=1; j=9; mean(as.numeric(D_new@data[i,j,1:R]) == D@data[i,j,1:R])
data=list(D = D_new)

# ## chooose K1, K2, K3
D1 = matricization(D_new, 1)
eigens = svd(D1)$d
# #### choose K1=10
plot(eigens[1:20])
# ####
D2 = matricization(D_new, 2)
eigens2 = svd(D2)$d
plot(eigens2[1:10])
# #### choose K2=2
# ####
# D3 = matricization(D_new, 3)
# eigens3 = svd(D3, nu=200, nv=200)$d
# plot(eigens3[1:30])
# #### choose K1=2

K1 = 4
K2 = 4
K3 = 23
data <- list(D = D_new)
dim(D_new)
length(words)

save.image("~/Downloads/market_basket_data.RData")
STOP
setwd("~/Documents/tensor-topic-modeling/")
library(tidyverse)
## other methods
library(nnTensor)
library(tidyverse)
library(topicmodels)
library(tidytext)

source("algorithm.R")
source("bayesian.R")
source("NTD.R")
source("tensor_operations.r")
source("run_experiments.R")

load("~/Downloads/market_basket_data.RData")
for (K3 in 10:25){
  for (method in c("TTM-HOOI", "TTM-HOSVD",  "TopicScore-HOSVD", "NTD",  "STM", "LDA")){
    M = median(apply(D3, 2, sum))
    results <- run_method(data, K1=K1, K2=K2, K3=K3, M=M, 
                          method=method)
    save(results, file=paste0("~/Downloads/market_basket", 
                              method, "_K1_",
                              K1, "_K2_", K2, "_K3_", K3, ".RData"))
    
    print(paste0("Done with method ", method))
    #print(error)
    #write_csv(error, paste0("/scratch/midway3/cdonnat/tensor-topic-modeling/tensor-topic-modeling/synthetic/results/",result_file, ".csv"))
  }
}

BBBB

load("~/Downloads/market_basketTTM-HOOI_K1_4_K2_4_K3_23.RData")

load("~/Downloads/k23.Rdata")
R = nrow(results$A3)
D3 = matricization(D_new, 3)
R = dim(D3)[1]
Q1 = dim(D_new)[1]
Q2 = dim(D_new)[2] 
lda3 <- LDA(t(D3), k = K3, 
            control = list(seed = 1234), method = 'VEM')
ap_topics3 <- tidy(tm, matrix = "beta")

ap_topics3["word"] = unlist(lapply(1:2163, function(x){rep(x, K3)}))
ap_topics3 = pivot_wider(ap_topics3, id_cols = "word", names_from = "topic", 
                         values_from = "beta")
W3 = tidy(lda3, matrix = "gamma")
W3 <- pivot_wider(W3, id_cols = "document", 
                  names_from = "topic",
                  values_from = "gamma")



A1_df = data.frame(results$A1)
colnames(A1_df) = 1:ncol(A1_df)
A1_df["household_key"] = sort(unique(data4analysis$household_key))
demo <- read_csv("~/Downloads/market_basket/hh_demographic.csv")
A1_df <- left_join(A1_df,
                   demo)
ggplot(A1_df %>% filter(!is.na(HH_COMP_DESC)), 
       aes(x=`1`, y=`2`, colour=HH_COMP_DESC))+
  geom_point()
colnames(ap_topics3)
ap_topics3_wide = pivot_wider(ap_topics3, id_cols = "word", names_from = "topic", 
                         values_from = "beta")


A2_df = data.frame(results$A2)
colnames(A2_df) = 1:ncol(A2_df)


A3_df = data.frame(results$A3)
colnames(A3_df) = 1:ncol(A3_df)

ggplot(A1_df %>% filter(!is.na(INCOME_DESC)), 
       aes(x=`1`, y=`2`, colour=INCOME_DESC))+
  geom_point()


####3 Need to add the demographics + product information
demo <- read_csv("~/Downloads/market_basket/hh_demographic.csv")
unique(demo$MARITAL_STATUS_CODE)
unique(demo$INCOME_DESC)
unique(demo$KID_CATEGORY_DESC)

#### How do we analyse the topics?
ap_topics3


topics <- data.frame(results$A3)
colnames(topics)[1:23] <- 1:K3
topics["ID"] = as.numeric(colnames(data4analysis)[3:(ncol(data4analysis))][words])

topics_filtered = topics %>%
  pivot_longer(cols=-c("ID")
               )
colnames(topics_filtered) <- c("ID", "Topic", "frequency")
topics_filtered = topics_filtered %>%
  group_by(Topic) %>%
  slice_max(order_by = frequency, n = 100)
topics_filtered = left_join(topics_filtered, test_product_filtered)

topics_filtered_w = topics_filtered %>%
  pivot_wider(id_cols ="ID",
              names_from = "Topic",
              values_from = "frequency")



##### find anchors
quantile(abs(apply(topics, 1, sum)- topics[,1]))
for (i in 1:23){
  words_topic_1 = sort(apply(topics %>% select(-c(ID)), 1, sum)- topics[,i], index.return=TRUE)
  words_topic_1_list = test_product_filtered[which(test_product_filtered$ID %in% words_topic_1$ix[1]),]
  print(paste0("Topic ", i))
  print(words_topic_1_list)
  
}



library(wordcloud)
library(RColorBrewer)
# Generate color palette

palette_17_colors <- colorRampPalette(brewer.pal(8, "Set3"))(92)

# Create a mapping of COMMODITY_DESC to colors
commodity_colors <- setNames(palette_17_colors, unique(topics_filtered$COMMODITY_DESC))

# Create the word cloud
for (i in 1:23){
  index = which(topics_filtered$Topic == i)
  w = topics_filtered$COMMODITY_DESC[index]
  freq=topics_filtered$frequency[index] 
  com = commodity_colors[topics_filtered$COMMODITY_DESC[index]]
  wordcloud(
    words = w,        # Words to display
    freq = freq * 10,           # Word size, scaling frequency
    colors = com,  # Color based on commodity
    random.order = FALSE,                          # Words should not be placed randomly
    rot.per = 0.35,                                # Rotation of some words
    scale = c(3, 0.5)                              # Scale the size of the words
  )
  Sys.sleep(10)
}



topics = left_join(topics, test_product_filtered)



unique(topics$COMMODITY_DESC)
unique(product$DEPARTMENT)

library(alto)

lda_varying_params_lists <-  list()
for (k in 1:25) {lda_varying_params_lists[[paste0("k",k)]] <- list(k = k)}

lda_models  <- 
  run_lda_models(
    data = t(D3),
    lda_varying_params_lists = lda_varying_params_lists,
    lda_fixed_params_list = list(method = "VEM"),
    dir = "~/Downloads/",
    reset = FALSE,
    verbose = TRUE
  )

aligned_topics_transport <- 
  align_topics(
    models = lda_models,
    method = "transport") 


plot(aligned_topics_transport)
plot(aligned_topics_transport, color_by = "coherence")
plot(aligned_topics_transport, color_by = "refinement")
compute_number_of_paths(aligned_topics_transport) %>% 
  plot_number_of_paths() + 
  ggtitle("Method: transport")

plot_beta(aligned_topics_transport, models = c("k3", "k18","k23"), threshold = 0.005)

load("~/Downloads/k23.Rdata")

ap_topics3 <- tidy(tm, matrix = "beta")
ap_topics3["word"] = unlist(lapply(1:R, function(x){rep(x, K3)}))
ap_topics3 = pivot_wider(ap_topics3, id_cols = "word", names_from = "topic", 
                         values_from = "beta")
W3 = tidy(lda3, matrix = "gamma")