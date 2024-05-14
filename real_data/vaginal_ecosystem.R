library(reshape2) 
library(reshape) 
library(dplyr)

mae <- readRDS("D:/yatingliu/VMRC-subcommunities-analyses-20230725/results/mae_for_analyses.Rds")
# 
source("D:/yatingliu/VMRC-subcommunities-analyses-20230725/analyses/00_setup.R")
load("D:/yatingliu/VMRC-subcommunities-analyses-20230725/results/gammas.Rdata")
count_assay <- "VM16S_combined"
counts <- assay(mae, count_assay) %>% t()

source("D:/yatingliu/CODE/tensor-topic-modeling/our_method.R")


## data cleaning
nonpreg <-
  colData(mae) %>%
  as.data.frame() %>%
  select(SampleID, Subject, Status, Race,Cohort, Site,Reprod_status,Age,PH,BMI,cycle_nb_m,cycleday,cycle_length,cycleday_fw,cycleday_bw,MC_ok,Bleeding) %>%
  filter(SampleID %in% rownames(counts),
         Status == "Non-pregnant")%>%
  mutate(mense_status= sub("^[^,]*,\\s*", "", Reprod_status ))%>%
  filter(!mense_status %in% c("undefined"),
         MC_ok)%>%
  mutate(expected_status = case_when(
    cycleday <= -17 & cycleday >= -18 ~ "follicular2",
    cycleday <= -1 & cycleday >= -4 ~ "luteal2",
    cycleday <= -5 & cycleday >= -11 ~"luteal1",
    cycleday >= 0 & cycleday <= 4 ~ "mense1",
    cycleday >= 5 & cycleday <= 7 ~"follicular1",
    cycleday <= -12 & cycleday >= -13 ~ "peri-ovulatory1",
    cycleday <= -14 & cycleday >= -16 ~ "peri-ovulatory2"))

length(unique(nonpreg$Subject))
nonpreg$Subject_m=paste0(nonpreg$Subject,",",nonpreg$cycle_nb_m)

summarise_nonpreg=nonpreg%>%group_by(Subject_m,expected_status)%>%summarise(n=length(expected_status))
removed_subject=summarise_nonpreg[summarise_nonpreg$expected_status %in%c("mense1","luteal2","luteal1","follicular1") & summarise_nonpreg$n<2,]$Subject_m  

cases=unique(nonpreg$expected_status)

selected_nonpreg=nonpreg%>% 
  group_by(Subject_m)%>%
  filter(length(unique(expected_status))==length(cases),
         !Subject_m%in%removed_subject)
summarise_nonpreg=selected_nonpreg%>%group_by(Subject_m,expected_status)%>%summarise(n=length(expected_status))


library(dplyr)
library(purrr)

sample_by_status <- function(df) {
  # Conditional logic for sampling
  n_samples <- case_when(
    df$expected_status[1] == "follicular2" ~ 1,
    df$expected_status[1] == "luteal2" ~ 2,
    df$expected_status[1] == "luteal1" ~ 2,
    df$expected_status[1] == "mense1" ~ 2,
    df$expected_status[1] == "follicular1" ~ 2,
    df$expected_status[1] == "peri-ovulatory1" ~ 1,
    df$expected_status[1] == "peri-ovulatory2" ~ 1
  )
  
  if (is.na(n_samples) | n_samples > nrow(df)) {
    return(df)
  } else {
    return(slice_sample(df, n = n_samples, replace = FALSE))
  }
}

selected_nonpreg <- selected_nonpreg %>%
  group_by(Subject_m, expected_status) %>%
  group_split() %>%
  map_dfr(sample_by_status)%>%
  ungroup()%>%
  arrange(Subject_m,cycleday)


summarise_nonpreg=selected_nonpreg%>%group_by(Subject_m,expected_status)%>%summarise(n=length(expected_status))



##into modeling
counts_select=counts[selected_nonpreg$SampleID,] # is document-term matrix
print(dim(counts_select))

Q1=length(unique(selected_nonpreg$Subject_m))
print(Q1)
Q2=11
K1=3
K2=3
K3=5
M_it=rowSums(counts_select)
tensor_counts=tensorization(t(counts_select/M_it),3,Q1=Q1,Q2=11,Q3=dim(counts_select)[2])
means_M=mean(M_it)


score_para=score(tensor_counts,K1=K1,K2=K2,K3=K3,M=means_M,normalize="Ours")
hatA1=score_para$hatA1
hatA2=score_para$hatA2
hatA3=score_para$hatA3
hatcore=score_para$hatcore
rownames(hatA3)=colnames(counts_select)
rownames(hatA1)=unique(selected_nonpreg$Subject_m)
rownames(hatA2)=c("[-18,-17]","[-16,-14]","[-13,-12]","[-11,-8]","[-7,-5]","[-4,-3]","[-2,-1]","[0,2]","[3,4]","[5,6]","[7]")
#colnames(hatA1)=c("Subject_1","Subject_2")
#colnames(hatA2)=c("tg_1","tg_2","tg_3")
#colnames(hatA3)=c("Topic_1","Topic_2","Topic_3","Topic_4","Topic_5","Topic_6","Topic_7","Topic_8","Topic_9")



#####starting analysis
###do regression
library (DirichletReg)


##A1
hatLDA_model_K3=lda_models_full[[K3]]
hatA1_lda=hatLDA_model_K3$gamma
hatA1_lda_df=as.data.frame(hatA1_lda)
hatA1_lda_df$SampleID=rownames(hatA1)
selected_nonpreg_subject_m=selected_nonpreg_subject_m%>%
  left_join(., hatA1_lda_df,by="SampleID")

dr_data=DR_data(selected_nonpreg_subject_m[,23:(23+K3-1)])

dirichlet_model_1 <- DirichReg(dr_data ~ as.factor(Race)  + Age_status+as.factor(mense_status), data = selected_nonpreg_subject_m)
dirichlet_model_1 %>% summary()
dirichlet_model_1 %>% summary()
dms <- dirichlet_model_1 %>% summary()
dms <- dms$coef.mat %>% as.data.frame()
K <- nrow(dms)/8
dms <- 
  dms %>% 
  setNames(c("Estimate","Standard_error","z_value","p_value")) %>% 
  mutate(
    var_group = rep(c("","Race","Race","Age","Age","menstrual status","menstrual status","menstrual status"), K),
    variable = rep(c("Intercept","Hispanic/Latino\n(vs. Black)","White\n(vs. Black)","30s\n(vs. 20s)","40s\n(vs. 20s)","luteal\n(vs. follicular)","menses\n(vs. follicular)","peri-ovulatory\n(vs. follicular)"), K),
    k = rep(paste0("v",1:K2), each = 8),
    sign_level = get_sign_levels(p_value)
  ) %>% 
  as_tibble() 


ggplot(dms %>% filter(var_group != ""),
       aes(x = Estimate, y = variable, col = sign_level)) +
  geom_vline(xintercept = 0, col = "gray") +
  geom_segment(aes(x = Estimate - Standard_error, xend = Estimate + Standard_error,
                   yend = variable)) +
  geom_point() +
  facet_grid(var_group ~ k, scales = "free_y", space = "free_y") +
  scale_color_manual("p-value", breaks = p_val_labels, values = p_val_cols) +
  ylab("") +
  theme(strip.text.y = element_text(angle = 0, hjust = 0),
        strip.background = element_rect(fill = "gray80", color = NA)
  ) +
  xlab("Dirichlet regression coefficient ± sd. error") 

##ours
hatA1_df=as.data.frame(hatA1)
hatA1_df$Subject_m=rownames(hatA1)
selected_nonpreg_subject_m=selected_nonpreg%>%
  mutate(Race_group=ifelse(Race %in% c("White","Hispanic/Latino"), "Other", Race)%>%factor(.,levels =c("Other","Black")),
         Age_status=case_when(Age<30 ~"Young",
                           Age>=30 & Age <40 ~"Middle",
                           Age>40 ~"Old")%>%factor(.,levels =c("Young","Middle","Old")),
         cycle_nb_m=as.factor(cycle_nb_m))

selected_nonpreg_subject_m=selected_nonpreg_subject_m%>%
                left_join(., hatA1_df,by="Subject_m")

dr_data=DR_data(selected_nonpreg_subject_m[,23:(23+K1-1)])

dirichlet_model_1 <- DirichReg(dr_data ~ as.factor(Race)  + Age_status, data = selected_nonpreg_subject_m)
dirichlet_model_1 %>% summary()
dms <- dirichlet_model_1 %>% summary()
dms <- dms$coef.mat %>% as.data.frame()


K <- nrow(dms)/5
dms <- 
  dms %>% 
  setNames(c("Estimate","Standard_error","z_value","p_value")) %>% 
  mutate(
    var_group = rep(c("","Race","Race","Age","Age"), K),
    variable = rep(c("Intercept","Hispanic/Latino\n(vs. Black)","White\n(vs. Black)","30s\n(vs. 20s)","40s\n(vs. 20s)"), K),
    k = rep(paste0("v",1:K1), each = 5),
    sign_level = get_sign_levels(p_value)
  ) %>% 
  as_tibble() 


ggplot(dms %>% filter(var_group != ""),
       aes(x = Estimate, y = variable, col = sign_level)) +
  geom_vline(xintercept = 0, col = "gray") +
  geom_segment(aes(x = Estimate - Standard_error, xend = Estimate + Standard_error,
                   yend = variable)) +
  geom_point() +
  facet_grid(var_group ~ k, scales = "free_y", space = "free_y") +
  scale_color_manual("p-value", breaks = p_val_labels, values = p_val_cols) +
  ylab("") +
  theme(strip.text.y = element_text(angle = 0, hjust = 0),
        strip.background = element_rect(fill = "gray80", color = NA)
  ) +
  xlab("Dirichlet regression coefficient ± sd. error") 

                            
##A2
selected_nonpreg_time=selected_nonpreg%>% mutate(expected_status2=rep(rownames(hatA2),times=Q1))

hatA2_df=hatA2%>% as.data.frame()
hatA2_df$expected_status2 <- rownames(hatA2)

selected_nonpreg_time=selected_nonpreg_time%>%
  left_join(.,hatA2_df,by="expected_status2",relationship
            = "many-to-many")
dr_data=DR_data(selected_nonpreg_time[,22:(22+K2-1)])

# selected_nonpreg_time=selected_nonpreg_time %>%
#   mutate(
#     menses = if_else(mense_status == "menses", 1, 0),  # Assign 'score' if race is Race1, otherwise NA
#     follicular= if_else(mense_status == "follicular", 1, 0),  # Repeat for Race2
#     peri_ovulatory = if_else(mense_status == "peri-ovulatory", 1, 0),  # Repeat for Race3
#     luteal = if_else(mense_status == "luteal", 1, 0),
#     IsBleeding=if_else(Bleeding >0, 1, 0)# Repeat for Race4
#   )
# selected_nonpreg_time=selected_nonpreg_time%>% 
#   group_by(expected_status2)%>%
#   summarise(menses=sum(menses)/24,
#             follicular=sum(follicular)/24,
#             peri_ovulatory=sum(peri_ovulatory)/24,
#             luteal=sum(luteal)/24,
#             IsBleeding=sum(IsBleeding)/24,
#             )
# rownames(selected_nonpreg_time)=selected_nonpreg_time$expected_status2
# selected_nonpreg_time=selected_nonpreg_time[rownames(hatA2),]


dirichlet_model_1 <- DirichReg(dr_data ~as.factor(mense_status), data = selected_nonpreg_time)
dirichlet_model_1 %>% summary()
dms <- dirichlet_model_1 %>% summary()
dms <- dms$coef.mat %>% as.data.frame()
K <- nrow(dms)/4
dms <- 
  dms %>% 
  setNames(c("Estimate","Standard_error","z_value","p_value")) %>% 
  mutate(
    var_group = rep(c("","menstrual status","menstrual status","menstrual status"), K),
    variable = rep(c("Intercept","luteal\n(vs. follicular)","menses\n(vs. follicular)","peri-ovulatory\n(vs. follicular)"), K),
    k = rep(paste0("v",1:K2), each = 4),
    sign_level = get_sign_levels(p_value)
  ) %>% 
  as_tibble() 


ggplot(dms %>% filter(var_group != ""),
       aes(x = Estimate, y = variable, col = sign_level)) +
  geom_vline(xintercept = 0, col = "gray") +
  geom_segment(aes(x = Estimate - Standard_error, xend = Estimate + Standard_error,
                   yend = variable)) +
  geom_point() +
  facet_grid(var_group ~ k, scales = "free_y", space = "free_y") +
  scale_color_manual("p-value", breaks = p_val_labels, values = p_val_cols) +
  ylab("") +
  theme(strip.text.y = element_text(angle = 0, hjust = 0),
        strip.background = element_rect(fill = "gray80", color = NA)
  ) +
  xlab("Dirichlet regression coefficient ± sd. error") 



####### A3

######## vs LDA
Best_K=16
topic_models_ours_full <- run_topic_models(as.matrix(counts_select),1:nrow(counts_select),K1=K1,K2=K2,Q1=Q1,Q2=Q2,list_params=1:Best_K,normalize="Ours")
aligned_topics_transport_ours<- 
  alto::align_topics(
    models = topic_models_ours_full[1:Best_K],
    method = "transport",reg=0.01)
plot(aligned_topics_transport_ours, add_leaves = TRUE, label_topics = TRUE)

lda_varying_params_lists <-  list()
for (k in 1:Best_K) {
  lda_varying_params_lists[[paste0("k",k)]] <- list(k = k)
}
lda_models_full  <- 
  alto::run_lda_models(
    data = counts_select,
    lda_varying_params_lists = lda_varying_params_lists,
    lda_fixed_params_list = list(method = "VEM"),
    dir = "metagenomics_lda_models_full/",
    reset = TRUE,
    verbose = TRUE
  )
aligned_topics_transport_comp <-
  alto::align_topics(
    models = lda_models_full[1:Best_K],
    method = "transport")
plot(aligned_topics_transport_comp, add_leaves = TRUE, label_topics = TRUE)




###hatA3

colnames(hatA3)=paste0("Topic","_", 1:K3)
plot_words_per_group(hatA3,words=5) # running function at the end of this file before running this
#VS LDA
LDA_model_K3=lda_models_full[[K3]]
hatA3_lda=exp(t(LDA_model_K3$beta))
colnames(hatA3_lda)=paste0("Topic","_LDA_", 1:K3)
plot_words_per_group(hatA3_lda,words=5) # returning top-5 words from hatA3



##hatcore
hatcore_3=matrization_tensor(hatcore,3)
plot_slice(hatcore@data,2)
# library(rgl)
library(plotly)
core_tensor=hatcore@data
expand.grid(x = 1:dim(core_tensor)[1], 
            y = 1:dim(core_tensor)[2], 
            z = 1:dim(core_tensor)[3]) -> coords
coords$value <- as.vector(core_tensor)
with(coords, plot3d(x, y, z, col = rainbow(1000)[rank(value)], size = 1))
plot_ly(data = coords, x = ~x, y = ~y, z = ~z, 
        type = 'scatter3d', mode = 'markers',
        marker = list(size = 10,  # Increased size for block-like appearance
                      color = ~value,
                      colorscale = 'Blues',
                      opacity = 0.8)) 

#topic_per_groups
A1_group=paste0("s_",1:K1)
A2_group=paste0("t_",1:K2)
combinations=as.data.frame(expand.grid(A2_group,A1_group))
combinations$categ=paste0(combinations[,1],",",combinations[,2])
colnames(hatcore_3)=combinations$categ
hatcore_df <- melt(hatcore_3)
names(hatcore_df) <- c("RowName", "ColName", "Value")
hatcore_df=separate(hatcore_df, col = ColName, into = c("time_group", "Subject_group"), sep = ",")

# Create the heatmap
ggplot(hatcore_df, aes(x=time_group,y=as.factor(RowName), fill = Value)) +
  geom_tile() + # Use geom_tile() for heatmap
  scale_fill_gradient(low = "cornsilk3", high = "dodgerblue") +
  facet_grid(.~Subject_group, scales="free")+# Color gradient
  labs(title = "Heatmap of Tensor Slice", x = "Menstrual_Group", y = "Species Group", fill = "Value") +
  theme_minimal()

ggplot(hatcore_df%>%filter(RowName%in% c(1,2,3)),
       aes(x=Subject_group, y=Value,group=time_group,color=time_group)) +
  geom_line(linewidth=1) +#
  geom_point(size=1.2)+
  #scale_y_log10() +
  #scale_x_log10() +
  facet_grid(RowName~., scales="free")+
  xlab("Subject_group") +  labs(colour="time_group") + 
  ylab("topic-proportion")



words_prob_given_groups=hatA3%*%hatcore_3

plot_words_per_group(words_prob_given_groups)



#### permutation

res_df <- function(res,match_permuted,k,method,group){
  res_temp = data.frame("min"=min(diag(as.matrix(match_permuted))), 
                        "max" = max(diag(as.matrix(match_permuted))),
                        "mean" = mean(diag(as.matrix(match_permuted))), 
                        "median" = median(diag(as.matrix(match_permuted))), 
                        "q25" = quantile(diag(as.matrix(match_permuted)), 0.25),
                        "q75" = quantile(diag(as.matrix(match_permuted)), 0.75),
                        "k" = k,
                        "method" = method,
                        "res1" = sum(diag(as.matrix(match_permuted))>0.1),
                        "res15" = sum(diag(as.matrix(match_permuted))>0.15),
                        "res2" = sum(diag(as.matrix(match_permuted))>0.2),
                        "res2.5" = sum(diag(as.matrix(match_permuted))>0.25),
                        "res3" = sum(diag(as.matrix(match_permuted))>0.3),
                        "res3.5" = sum(diag(as.matrix(match_permuted))>0.35),
                        "res45" = sum(diag(as.matrix(match_permuted))>0.4),
                        "res4" = sum(diag(as.matrix(match_permuted))>0.45),
                        "res5" = sum(diag(as.matrix(match_permuted))>0.5),
                        "res6" = sum(diag(as.matrix(match_permuted))>0.6),
                        "res7" = sum(diag(as.matrix(match_permuted))>0.7),
                        "res8" = sum(diag(as.matrix(match_permuted))>0.8),
                        "res9" = sum(diag(as.matrix(match_permuted))>0.9),
                        "removed_group" = group,
                  
                        "off_diag" = (sum(match_permuted) - sum(diag(as.matrix(match_permuted))))/(ncol(match_permuted)^2 - ncol(match_permuted)) )
  res = rbind(res, res_temp)
  return(res)
}

group_list=c("mense1","luteal2","luteal1","follicular1")
res=c()
for (group in group_list){
  #train_index = sample(unique(selected_nonpreg$Subject_m),12)
  #train_data=selected_nonpreg[selected_nonpreg$Subject_m %in%train_index,]$SampleID
  #test_data = selected_nonpreg[!selected_nonpreg$Subject_m %in% train_index,]$SampleID
  
train_data=selected_nonpreg[selected_nonpreg$expected_status==group,]$SampleID
test_data = selected_nonpreg[!selected_nonpreg$expected_status==group,]$SampleID

Q11=24
Q12=24
Q21=2
Q22=9
topic_models_ours_full_train <- run_topic_models(as.matrix(counts_select),train_data,K1=K1,K2=1,Q1=Q11,Q2=Q21,list_params=1:Best_K,normalize="Ours")
topic_models_ours_full_test <- run_topic_models(as.matrix(counts_select),test_data,K1=K1,K2=3,Q1=Q12,Q2=Q22,list_params=1:Best_K,normalize="Ours")



lda_models_full_train  <- 
  alto::run_lda_models(
    data = counts_select[train_data,],
    lda_varying_params_lists = lda_varying_params_lists,
    lda_fixed_params_list = list(method = "VEM"),
    dir = "metagenomics_lda_models_full/",
    reset = TRUE,
    verbose = TRUE
  )
lda_models_full_train_test  <- 
  alto::run_lda_models(
    data = counts_select[test_data,],
    lda_varying_params_lists = lda_varying_params_lists,
    lda_fixed_params_list = list(method = "VEM"),
    dir = "metagenomics_lda_models_full/",
    reset = TRUE,
    verbose = TRUE
  )
for (k in 3:Best_K){
  it = 1
  alignment <- align_topics(exp(topic_models_ours_full_train[[paste0("k",k)]]$beta),
                            exp(topic_models_ours_full_test[[paste0("k",k)]]$beta), 
                            dist="cosine", do.plot=FALSE)
  match_permuted <- alignment$match
  res=res_df(res,match_permuted,k,"Ours",group)
  alignment <- align_topics(exp(lda_models_full_train[[paste0("k",k)]]$beta),
                            exp(lda_models_full_train_test[[paste0("k",k)]]$beta), 
                            dist="cosine", do.plot=FALSE)
  match_permuted <- alignment$match
  res=res_df(res,match_permuted,k,"LDA",group)
}
}

#write_csv(as.data.frame(res), "C:/Users/yichugang/Desktop/CODE/tensor-topic-modeling/real_data/vaginal_permute_subject.csv")
res_fi=as.data.frame(res) %>%
  group_by(method, k) %>%
  summarise(min=median(min),
            max=median(max),
            mean=median(mean),
            median=median(median),
            q25=median(q25),
            q75=median(q75))

ggplot(res_fi, 
       aes(x=k, y=median, colour=method))+
  #geom_smooth(alpha=0.2,se = FALSE)+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = q25, ymax = q75), width = 0.1, alpha=0.4) +
  scale_color_manual(values = c( "dodgerblue", "chartreuse2"), breaks = c("Ours","LDA"),
                     labels = c("Ours","LDA"))+
  ylab("Topic Resolution") +
  xlab("K3") 






###some useful functions

library(tidyverse)

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


library(rdist)

normalize_rows <- function(mat) {
  row_norms <- sqrt(rowSums(mat^2))
  return(sweep(mat, 1, row_norms, FUN="/"))
}


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
  return(list("B_permuted"=B_permuted,
              "match" = match_permuted))
}

