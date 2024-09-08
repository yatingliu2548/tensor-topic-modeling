library(reshape2)
library(reshape)
library(dplyr)
library(tidyverse)
library(alto)
library(ggplot2)
theme_set(theme_bw(base_size = 14))

setwd("C:/Users/yichg/yating/tensor-topic-modeling")
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
source("VMRC-subcommunities-analyses-20230725/analyses/00_setup.R")

selected_nonpreg_subject_m= read.csv("real_data/selected_nonpreg_subject_m.csv",row.names = 1)
counts_select=read.csv("real_data/vaginal_microbiota.csv",row.names=1)
selected_nonpreg=selected_nonpreg_subject_m
set.seed(1234)
# ## data cleaning

#
# mae <- readRDS("C:/Users/建新/Downloads/VMRC-subcommunities-analyses-20230725/results/mae_for_analyses.Rds")
# #
# source("C:/Users/建新/Downloads/VMRC-subcommunities-analyses-20230725/analyses/00_setup.R")
# load("C:/Users/建新/Downloads/VMRC-subcommunities-analyses-20230725/results/gammas.Rdata")
# count_assay <- "VM16S_combined"
# counts <- assay(mae, count_assay) %>% t()
# # nonpreg <-
#   colData(mae) %>%
#   as.data.frame() %>%
#   select(SampleID, Subject, Status, Race,Cohort, Site,Reprod_status,Age,PH,BMI,cycle_nb_m,cycleday,cycle_length,cycleday_fw,cycleday_bw,MC_ok,Bleeding) %>%
#   filter(SampleID %in% rownames(counts),
#          Status == "Non-pregnant")%>%
#   mutate(mense_status= sub("^[^,]*,\\s*", "", Reprod_status ))%>%
#   filter(!mense_status %in% c("undefined"),
#          MC_ok)%>%
#   mutate(expected_status = case_when(
#     cycleday <= -17 & cycleday >= -18 ~ "follicular2",
#     cycleday <= -1 & cycleday >= -4 ~ "luteal2",
#     cycleday <= -5 & cycleday >= -11 ~"luteal1",
#     cycleday >= 0 & cycleday <= 4 ~ "mense1",
#     cycleday >= 5 & cycleday <= 7 ~"follicular1",
#     cycleday <= -12 & cycleday >= -13 ~ "peri-ovulatory1",
#     cycleday <= -14 & cycleday >= -16 ~ "peri-ovulatory2"))
#
# length(unique(nonpreg$Subject))
# nonpreg$Race[nonpreg$Race == "Black"] <- "African American"
# nonpreg$Subject_m=paste0(nonpreg$Subject,",",nonpreg$cycle_nb_m)
#
# summarise_nonpreg=nonpreg%>%group_by(Subject_m,expected_status)%>%summarise(n=length(expected_status))
# removed_subject=summarise_nonpreg[summarise_nonpreg$expected_status %in%c("mense1","luteal2","luteal1","follicular1") & summarise_nonpreg$n<2,]$Subject_m
#
# cases=unique(nonpreg$expected_status)
#
# selected_nonpreg=nonpreg%>%
#   group_by(Subject_m)%>%
#   filter(length(unique(expected_status))==length(cases),
#          !Subject_m%in%removed_subject)
# summarise_nonpreg=selected_nonpreg%>%group_by(Subject_m,expected_status)%>%summarise(n=length(expected_status))
#
#
# library(dplyr)
# library(purrr)
#
# sample_by_status <- function(df) {
#   # Conditional logic for sampling
#   n_samples <- case_when(
#     df$expected_status[1] == "follicular2" ~ 1,
#     df$expected_status[1] == "luteal2" ~ 2,
#     df$expected_status[1] == "luteal1" ~ 2,
#     df$expected_status[1] == "mense1" ~ 2,
#     df$expected_status[1] == "follicular1" ~ 2,
#     df$expected_status[1] == "peri-ovulatory1" ~ 1,
#     df$expected_status[1] == "peri-ovulatory2" ~ 1
#   )
#
#   if (is.na(n_samples) | n_samples > nrow(df)) {
#     return(df)
#   } else {
#     return(slice_sample(df, n = n_samples, replace = FALSE))
#   }
# }
#
# selected_nonpreg <- selected_nonpreg %>%
#   group_by(Subject_m, expected_status) %>%
#   group_split() %>%
#   map_dfr(sample_by_status)%>%
#   ungroup()%>%
#   arrange(Subject_m,cycleday)
#
#
# summarise_nonpreg=selected_nonpreg%>%group_by(Subject_m)%>%summarise(n=length(Race))
#
# selected_nonpreg_subject_m=selected_nonpreg%>%
#   mutate(Race_group=ifelse(Race %in% c("White","Hispanic/Latino"), "Other", Race)%>%factor(.,levels =c("Other","African African")),
#          Age_status=case_when(Age<30 ~"Young",
#                            Age>=30 & Age <40 ~"Middle",
#                            Age>40 ~"Old")%>%factor(.,levels =c("Young","Middle","Old")),
#          cycle_nb_m=as.factor(cycle_nb_m))%>%
#   mutate(expected_status2=rep(c("[-18,-17]","[-16,-14]","[-13,-12]","[-11,-8]","[-7,-5]","[-4,-3]","[-2,-1]","[0,2]","[3,4]","[5,6]","[7]"),times=Q1))
#counts_select=counts[selected_nonpreg$SampleID,] # is document-term matrix
#write.csv(counts_select, file = "vaginal_microbiota.csv", row.names = T)

##into modeling

print(dim(counts_select))

Q1=length(unique(selected_nonpreg$Subject_m))
print(Q1)
Q2=11
K1=3
K2=4
K3=9
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


Best_K=9
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
plot(aligned_topics_transport_comp, add_leaves = TRUE, label_topics = TRUE,min_feature_prop=0.1)


#post_lda
plda_full <- run_pLDA_models(as.matrix(counts_select),1:nrow(counts_select),K1=K1,K2=K2,Q1=Q1,Q2=Q2,list_params=1:Best_K)
aligned_topics_transport_postLDA<-
  alto::align_topics(
    models = plda_full[1:Best_K],
    method = "transport",reg=0.01)
plot(aligned_topics_transport_postLDA, add_leaves = TRUE, label_topics = TRUE)



#SLDA

SLDA_full <- run_SLDA_models(as.matrix(counts_select),rownames(counts_select),data=selected_nonpreg,Q1=Q1,Q2=Q2,list_params=1:Best_K,max_em=5)
aligned_topics_transport_SLDA<-
  alto::align_topics(
    models = SLDA_full[which( map_int(SLDA_full, ~class(.) == "list")==1)],
    method = "transport",reg=0.1)
plot(aligned_topics_transport_SLDA, add_leaves = TRUE, label_topics = TRUE,min_feature_prop=0.001)

###Tensor LDA
tensorlda_full <- run_tensorLDA_models(as.matrix(counts_select),1:nrow(counts_select),K1=K1,K2=K2,Q1=Q1,Q2=Q2,list_params=1:Best_K)
aligned_topics_transport_tensorLDA<-
  alto::align_topics(
    models = tensorlda_full[1:Best_K],
    method = "transport",reg=0.01)
plot(aligned_topics_transport_tensorLDA, add_leaves = TRUE, label_topics = TRUE)


#####starting analysis
library (DirichletReg)
plot_dirichlet<-function(W,Topic_K=9,word="v",by="SampleID",relationship= "many-to-many"){
  #W is p\times k

  W_df=as.data.frame(W)
  if (by=="SampleID"){
    rownames(W)=rownames(counts_select)
  }else if(by=="Subject_m"){
    rownames(W)=unique(selected_nonpreg$Subject_m)
  }else if(by=="expected_status2"){
    rownames(W)=c("[-18,-17]","[-16,-14]","[-13,-12]","[-11,-8]","[-7,-5]","[-4,-3]","[-2,-1]","[0,2]","[3,4]","[5,6]","[7]")
  }
  W_df$ID=rownames(W)
  colnames(W_df)[colnames(W_df) == "ID"] <- by

  W_df=selected_nonpreg_subject_m%>%
    left_join(., W_df,by=by,relationship=relationship)

  dr_data=DR_data(W_df[,(ncol(selected_nonpreg_subject_m)+1):(ncol(W_df))])

  K3=Topic_K
  dirichlet_model_1 <- DirichReg(dr_data ~ as.factor(Race)  + Age_status+as.factor(mense_status), data = selected_nonpreg_subject_m)
  dirichlet_model_1 %>% summary()
  #dirichlet_model_1 %>% summary()
  dms <- dirichlet_model_1 %>% summary()
  dms <- dms$coef.mat %>% as.data.frame()
  K <- nrow(dms)/8
  dms <-
    dms %>%
    setNames(c("Estimate","Standard_error","z_value","p_value")) %>%
    mutate(
      var_group = rep(c("","Race","Race","Age","Age","Menstrual\nstatus","Menstrual\nstatus","Menstrual\nstatus"), K),
      variable = rep(c("Intercept","Hispanic/Latino\n(vs. African American)","White\n(vs. African American)","30s\n(vs. 20s)","40s\n(vs. 20s)","luteal\n(vs. follicular)","menses\n(vs. follicular)","peri-ovulatory\n(vs. follicular)"), K),
      k = rep(paste0(word,1:K3), each = 8),
      sign_level = get_sign_levels(p_value)
    ) %>%
    as_tibble()


  plot=ggplot(dms %>% filter(var_group != ""),
              aes(x = Estimate, y = variable, col = sign_level)) +
    geom_vline(xintercept = 0, col = "gray") +
    geom_segment(aes(x = Estimate - Standard_error, xend = Estimate + Standard_error,
                     yend = variable)) +
    geom_point() +
    facet_grid(var_group ~ k, scales = "free_y", space = "free_y") +
    scale_color_manual("p-value", breaks = p_val_labels, values = p_val_cols) +
    ylab("") +
    theme(strip.text.y = element_text(angle = 0, hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.background = element_rect(fill = "gray80", color = NA)
    ) +

    xlab("Dirichlet regression coefficient ± sd. error")
  return(plot)
}

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

topic_corre<- function(data,W,mode=1){
  df_onehot
}

##LDA
K3=9
Q1=24
Q2=11
model=lda_models_full[[K3]]
W_lda=model$gamma
plot_dirichlet(W_lda,Topic_K=K3,word="v")
W1_LDA=matrization_tensor(tensorization(t(W_lda),3,Q1,Q2,dim(W_lda)[2]),1)
A1_LDA=spectral_clustering(W1_LDA %*% t(W1_LDA),K=K1,mix=T)
plot_dirichlet(A1_LDA,Topic_K=K1,by="Subject_m",word="g")
heatmap_matrix(A1_LDA,"Groups","Subject_m")
W2_LDA=matrization_tensor(tensorization(t(W_lda),3,Q1,Q2,dim(W_lda)[2]),2)
A2_LDA=spectral_clustering(W2_LDA %*% t(W2_LDA),K=K2,mix=T)
heatmap_matrix2(A2_LDA,"Classes","time")
plot_dirichlet(A2_LDA,Topic_K=K2,by="expected_status2",word="t")
G_LDA=compute_G_from_WA(kronecker(A1_LDA,A2_LDA),W_lda)
plot_slice(tensorization(t(G_LDA),3,K1,K2,dim(G_LDA)[2])@data,2,"Groups of Subject","Topics",yes=T)

#SLDA
model=SLDA_full[[K3-2]]
W_slda=model$gamma
dim(W_slda)
rownames(W_slda)=rownames(counts_select)
plot_dirichlet(W_slda,Topic_K=K3,word="v")
W1_SLDA=matrization_tensor(tensorization(t(W_slda),3,Q1,Q2,dim(W_slda)[2]),1)
A1_SLDA=spectral_clustering(W1_SLDA %*% t(W1_SLDA),K=K1,mix=T)
plot_dirichlet(A1_SLDA,Topic_K=K1,by="Subject_m",word="g")
heatmap_matrix(A1_SLDA,"Groups","Subject_m")
W2_SLDA=matrization_tensor(tensorization(t(W_slda),3,Q1,Q2,dim(W_slda)[2]),2)
A2_SLDA=spectral_clustering(W2_SLDA %*% t(W2_SLDA),K=K2,mix=T)
plot_dirichlet(A2_SLDA,Topic_K=K2,by="expected_status2",word="t")
heatmap_matrix(A2_SLDA,"Classes","time")
G_SLDA=compute_G_from_WA(kronecker(A1_SLDA,A2_SLDA),W_slda)
plot_slice(tensorization(t(G_SLDA),3,K1,K2,dim(G_SLDA)[2])@data,2,"Groups of Subject","Topics",yes=T)


#tensorlda
active_index=which(apply(counts_select, 2, sum)>0)
data_active = t( counts_select[, active_index])
tlda_9=tensor_lda(tensorization(data_active,3,Q1=Q1,Q2=11,Q3=dim(data_active)[1]),K1,K2,K3=K3)
#rownames(W_tlda)=rownames(counts_select)
plot_dirichlet(tensorlda_full[[K3]]$gamma,Topic_K=K3,word="v")
plot_dirichlet(tlda_9$A1,Topic_K=K1,by="Subject_m",word="g")
heatmap_matrix(tlda_9$A1,"Groups","Subject_m")
plot_dirichlet(tlda_9$A2,Topic_K=K2,by="expected_status2",word="t")
heatmap_matrix(tlda_9$A2,"Classes","time")
plot_slice(tlda_9$core,2,"Groups of Subject","Topics",yes=T)

#postlda
model=post_lda(tensorization(t(counts_select),3,Q1=Q1,Q2=Q2,Q3=dim(counts_select)[2])@data,K1,K2,K3=K3)
#rownames(W_tlda)=rownames(counts_select)
plot_dirichlet(plda_full[[K3]]$gamma,Topic_K=K3,word="v")
plot_dirichlet(model$A1,Topic_K=K1,by="Subject_m",word="g")
heatmap_matrix(model$A1,"Groups","Subject_m")
plot_dirichlet(model$A2,Topic_K=K2,by="expected_status2",word="t")
heatmap_matrix(model$A2,"Classes","time")
plot_slice(model$core,2,"Groups of Subject","Topics",yes=T)


#Ours
model=topic_models_ours_full[[K3]]
W_ours=model$gamma
plot_dirichlet(W_ours,Topic_K=K3,word="v")
score_para=score(tensor_counts,K1=K1,K2=K2,K3=K3,M=means_M,normalize="Ours")
plot_dirichlet(score_para$hatA1,Topic_K=K1,by="Subject_m",word="g")
heatmap_matrix(score_para$hatA1,"Groups","Subject_m")
plot_dirichlet(score_para$hatA2,Topic_K=K2,by="expected_status2",word="t")
heatmap_matrix(score_para$hatA2,"Classes","time")
plot_slice(score_para$hatcore@data,2,"Groups of Subject","Topics",yes=T)


##A1
# hatLDA_model_K3=lda_models_full[[K3]]
# hatA1_lda=hatLDA_model_K3$gamma
# hatA1_lda_df=as.data.frame(hatA1_lda)
# hatA1_lda_df$SampleID=rownames(hatA1)
# selected_nonpreg_subject_m=selected_nonpreg_subject_m%>%
#   left_join(., hatA1_lda_df,by="SampleID")
#
# dr_data=DR_data(selected_nonpreg_subject_m[,23:(23+K3-1)])
#
# dirichlet_model_1 <- DirichReg(dr_data ~ as.factor(Race)  + Age_status+as.factor(mense_status), data = selected_nonpreg_subject_m)
# dirichlet_model_1 %>% summary()
# dirichlet_model_1 %>% summary()
# dms <- dirichlet_model_1 %>% summary()
# dms <- dms$coef.mat %>% as.data.frame()
# K <- nrow(dms)/8
# dms <-
#   dms %>%
#   setNames(c("Estimate","Standard_error","z_value","p_value")) %>%
#   mutate(
#     var_group = rep(c("","Race","Race","Age","Age","menstrual status","menstrual status","menstrual status"), K),
#     variable = rep(c("Intercept","Hispanic/Latino\n(vs. Black)","White\n(vs. Black)","30s\n(vs. 20s)","40s\n(vs. 20s)","luteal\n(vs. follicular)","menses\n(vs. follicular)","peri-ovulatory\n(vs. follicular)"), K),
#     k = rep(paste0("v",1:K2), each = 8),
#     sign_level = get_sign_levels(p_value)
#   ) %>%
#   as_tibble()
#
#
# ggplot(dms %>% filter(var_group != ""),
#        aes(x = Estimate, y = variable, col = sign_level)) +
#   geom_vline(xintercept = 0, col = "gray") +
#   geom_segment(aes(x = Estimate - Standard_error, xend = Estimate + Standard_error,
#                    yend = variable)) +
#   geom_point() +
#   facet_grid(var_group ~ k, scales = "free_y", space = "free_y") +
#   scale_color_manual("p-value", breaks = p_val_labels, values = p_val_cols) +
#   ylab("") +
#   theme(strip.text.y = element_text(angle = 0, hjust = 0),
#         strip.background = element_rect(fill = "gray80", color = NA)
#   ) +
#   xlab("Dirichlet regression coefficient ± sd. error")
#
# ##ours
# A1=as.data.frame(hatA1)#hatA1_df$Subject_m=rownames(hatA1)
#
# #A1=as.data.frame(A1_SLDA)
# A1$Subject_m=rownames(hatA1)
#
# df_A1=selected_nonpreg_subject_m%>%
#                 left_join(., A1,by="Subject_m")
#
# dr_data=DR_data(df_A1[,(ncol(selected_nonpreg_subject_m)+1):ncol(df_A1)])
#
# dirichlet_model_1 <- DirichReg(dr_data ~ as.factor(Race)  + Age_status, data = df_A1)
# dirichlet_model_1 %>% summary()
# dms <- dirichlet_model_1 %>% summary()
# dms <- dms$coef.mat %>% as.data.frame()
#
#
# K <- nrow(dms)/5
# dms <-
#   dms %>%
#   setNames(c("Estimate","Standard_error","z_value","p_value")) %>%
#   mutate(
#     var_group = rep(c("","Race","Race","Age","Age"), K),
#     variable = rep(c("Intercept","Hispanic/Latino\n(vs. Black)","White\n(vs. Black)","30s\n(vs. 20s)","40s\n(vs. 20s)"), K),
#     k = rep(paste0("v",1:K1), each = 5),
#     sign_level = get_sign_levels(p_value)
#   ) %>%
#   as_tibble()
#
#
# ggplot(dms %>% filter(var_group != ""),
#        aes(x = Estimate, y = variable, col = sign_level)) +
#   geom_vline(xintercept = 0, col = "gray") +
#   geom_segment(aes(x = Estimate - Standard_error, xend = Estimate + Standard_error,
#                    yend = variable)) +
#   geom_point() +
#   facet_grid(var_group ~ k, scales = "free_y", space = "free_y") +
#   scale_color_manual("p-value", breaks = p_val_labels, values = p_val_cols) +
#   ylab("") +
#   theme(strip.text.y = element_text(angle = 0, hjust = 0),
#         strip.background = element_rect(fill = "gray80", color = NA)
#   ) +
#   xlab("Dirichlet regression coefficient ± sd. error")
#
#
# ##A2
# selected_nonpreg_time=selected_nonpreg%>% mutate(expected_status2=rep(rownames(hatA2),times=Q1))
#
# hatA2_df=hatA2%>% as.data.frame()
# hatA2_df$expected_status2 <- rownames(hatA2)
#
# selected_nonpreg_time=selected_nonpreg_time%>%
#   left_join(.,hatA2_df,by="expected_status2",relationship
#             = "many-to-many")
# dr_data=DR_data(selected_nonpreg_time[,22:(22+K2-1)])
#
#
#
# dirichlet_model_1 <- DirichReg(dr_data ~as.factor(mense_status), data = selected_nonpreg_time)
# dirichlet_model_1 %>% summary()
# dms <- dirichlet_model_1 %>% summary()
# dms <- dms$coef.mat %>% as.data.frame()
# K <- nrow(dms)/4
# dms <-
#   dms %>%
#   setNames(c("Estimate","Standard_error","z_value","p_value")) %>%
#   mutate(
#     var_group = rep(c("","menstrual status","menstrual status","menstrual status"), K),
#     variable = rep(c("Intercept","luteal\n(vs. follicular)","menses\n(vs. follicular)","peri-ovulatory\n(vs. follicular)"), K),
#     k = rep(paste0("v",1:K2), each = 4),
#     sign_level = get_sign_levels(p_value)
#   ) %>%
#   as_tibble()
#
#
# ggplot(dms %>% filter(var_group != ""),
#        aes(x = Estimate, y = variable, col = sign_level)) +
#   geom_vline(xintercept = 0, col = "gray") +
#   geom_segment(aes(x = Estimate - Standard_error, xend = Estimate + Standard_error,
#                    yend = variable)) +
#   geom_point() +
#   facet_grid(var_group ~ k, scales = "free_y", space = "free_y") +
#   scale_color_manual("p-value", breaks = p_val_labels, values = p_val_cols) +
#   ylab("") +
#   theme(strip.text.y = element_text(angle = 0, hjust = 0),
#         strip.background = element_rect(fill = "gray80", color = NA)
#   ) +
#   xlab("Dirichlet regression coefficient ± sd. error")
#


####### A3

######## vs LDA



################# VS structural LDA


#out_bio=convert_dtm_to_stm_format(counts_select)

K3=9
poliblogPrevFit <- stm(documents = Matrix(counts_select, sparse = TRUE),
                       K = 5, prevalence =~ mense_status*Age+mense_status*Race,
                       max.em.its = 20,
                       data = selected_nonpreg%>%mutate(SampleID=as.integer(rownames(selected_nonpreg))),
                       init.type = "Spectral")

findingk <- searchK(documents=convert_dtm_to_stm_format(as.matrix(as.data.frame((counts_select))))$documents,vocab=poliblogPrevFit$vocab, K = c(3:20),
                                        prevalence =~mense_status*Age+mense_status*Race, data = selected_nonpreg, verbose=FALSE, max.em.its = 20)



plot(findingk)
# plot(poliblogPrevFit)
# prep <- estimateEffect(c(9) ~  mense_status*Race, poliblogPrevFit,
#                        metadata = selected_nonpreg, uncertainty = "None")
#
# plot(prep, covariate = "Age", model = poliblogPrevFit,
#      method = "continuous", xlab = "Age", moderator = "mense_status",
#      moderator.value = "mense", linecol = "blue", ylim = c(0, .12),
#      printlegend = F)

#W_SLDA=lda_models_full[[K3]]$gamma

#
# W_SLDA=poliblogPrevFit$theta
# A_SLDA=exp(poliblogPrevFit$beta$logbeta[[1]])
# colnames(A_SLDA)=poliblogPrevFit$vocab
#
# W_Slda_df=as.data.frame(W_SLDA)
# W_Slda_df$SampleID=rownames(counts_select)
# slda_df=selected_nonpreg_subject_m%>%
#   left_join(., W_Slda_df,by="SampleID")
#
# dr_data=DR_data(slda_df[,(ncol(selected_nonpreg_subject_m)+1):(ncol(slda_df))])
#
# K3=9
# dirichlet_model_1 <- DirichReg(dr_data ~ as.factor(Race)  + Age_status+as.factor(mense_status), data = selected_nonpreg_subject_m)
# dirichlet_model_1 %>% summary()
# #dirichlet_model_1 %>% summary()
# dms <- dirichlet_model_1 %>% summary()
# dms <- dms$coef.mat %>% as.data.frame()
# K <- nrow(dms)/8
# dms <-
#   dms %>%
#   setNames(c("Estimate","Standard_error","z_value","p_value")) %>%
#   mutate(
#     var_group = rep(c("","Race","Race","Age","Age","Menstrual\nstatus","Menstrual\nstatus","Menstrual\nstatus"), K),
#     variable = rep(c("Intercept","Hispanic/Latino\n(vs. African American)","White\n(vs. African American)","30s\n(vs. 20s)","40s\n(vs. 20s)","luteal\n(vs. follicular)","menses\n(vs. follicular)","peri-ovulatory\n(vs. follicular)"), K),
#     k = rep(paste0("v",1:K3), each = 8),
#     sign_level = get_sign_levels(p_value)
#   ) %>%
#   as_tibble()
#
#
# ggplot(dms %>% filter(var_group != ""),
#        aes(x = Estimate, y = variable, col = sign_level)) +
#   geom_vline(xintercept = 0, col = "gray") +
#   geom_segment(aes(x = Estimate - Standard_error, xend = Estimate + Standard_error,
#                    yend = variable)) +
#   geom_point() +
#   facet_grid(var_group ~ k, scales = "free_y", space = "free_y") +
#   scale_color_manual("p-value", breaks = p_val_labels, values = p_val_cols) +
#   ylab("") +
#   theme(strip.text.y = element_text(angle = 0.5, hjust = 0),
#         strip.background = element_rect(fill = "gray80", color = NA)
#   ) +
#   xlab("Dirichlet regression coefficient ± sd. error")
#



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
group_list=seq(1:20)
res=c()
Best_K=16
lda_varying_params_lists <-  list()
for (k in 1:Best_K) {
  lda_varying_params_lists[[paste0("k",k)]] <- list(k = k)
}
for (group in group_list){
  train_index = sample(unique(selected_nonpreg$Subject_m),12)
  train_data=selected_nonpreg[selected_nonpreg$Subject_m %in%train_index,]$SampleID
  test_data = selected_nonpreg[!selected_nonpreg$Subject_m %in% train_index,]$SampleID

#train_data=selected_nonpreg[selected_nonpreg$expected_status==group,]$SampleID
#test_data = selected_nonpreg[!selected_nonpreg$expected_status==group,]$SampleID

Q11=12
Q12=12
Q21=11
Q22=11
topic_models_ours_full_train <- run_topic_models(as.matrix(counts_select),train_data,K1=3,K2=4,Q1=Q11,Q2=Q21,list_params=1:Best_K,normalize="Ours")
topic_models_ours_full_test <- run_topic_models(as.matrix(counts_select),test_data,K1=3,K2=4,Q1=Q12,Q2=Q22,list_params=1:Best_K,normalize="Ours")

tensorlda_full_train <- run_tensorLDA_models(as.matrix(counts_select),train_data,K1=K1,K2=K2,Q1=Q11,Q2=Q21,list_params=1:Best_K)
tensorlda_full_test <- run_tensorLDA_models(as.matrix(counts_select),test_data,K1=K1,K2=K2,Q1=Q12,Q2=Q22,list_params=1:Best_K)

SLDA_full_train <- run_SLDA_models(as.matrix(counts_select),train_data,data=selected_nonpreg,Q1=Q11,Q2=Q21,list_params=3:Best_K)
SLDA_full_test <- run_SLDA_models(as.matrix(counts_select),test_data,data=selected_nonpreg,Q1=Q12,Q2=Q22,list_params=3:Best_K)

lda_models_full_train  <-tryCatch(
  alto::run_lda_models(
    data = counts_select[train_data,],
    lda_varying_params_lists = lda_varying_params_lists,
    lda_fixed_params_list = list(method = "VEM"),
    dir = "metagenomics_lda_models_full/",
    reset = TRUE,
    verbose = TRUE
  ),
  error = function(err) {
    # Code to handle the error (e.g., print an error message, log the error, etc.)
    cat("Error occurred while running lda:", conditionMessage(err), "\n")
    # Return a default value or NULL to continue with the rest of the code
    return(NULL)
  })

lda_models_full_train_test  <-tryCatch(
  alto::run_lda_models(
    data = counts_select[test_data,],
    lda_varying_params_lists = lda_varying_params_lists,
    lda_fixed_params_list = list(method = "VEM"),
    dir = "metagenomics_lda_models_full/",
    reset = TRUE,
    verbose = TRUE
  ),
  error = function(err) {
    # Code to handle the error (e.g., print an error message, log the error, etc.)
    cat("Error occurred while running lda:", conditionMessage(err), "\n")
    # Return a default value or NULL to continue with the rest of the code
    return(NULL)
  })


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
  if(paste0("k",k)%in% names(SLDA_full_train) & paste0("k",k) %in% names(SLDA_full_test)){
    alignment <- align_topics(exp(SLDA_full_train[[paste0("k",k)]]$beta),
                              exp(SLDA_full_test[[paste0("k",k)]]$beta),
                              dist="cosine", do.plot=FALSE)
    match_permuted <- alignment$match
    res=res_df(res,match_permuted,k,"SLDA",group)
  }
  if(paste0("k",k)%in% names(tensorlda_full_train) & paste0("k",k) %in% names(tensorlda_full_test)){
    alignment <- align_topics(exp(tensorlda_full_train[[paste0("k",k)]]$beta),
                              exp(tensorlda_full_test[[paste0("k",k)]]$beta),
                              dist="cosine", do.plot=FALSE)
    match_permuted <- alignment$match
    res=res_df(res,match_permuted,k,"tLDA",group)
  }

}
}
 res1_4=res

write_csv(as.data.frame(res), "C:/Users/建新/Desktop/tensor-topic-modeling/tensor-topic-modeling/real_data/vaginal_permute_subject_1_10.csv")
res_fi=as.data.frame(res) %>%
  group_by(method, k) %>%
  summarise(min=median(min),
            max=median(max),
            mean=median(mean),
            median=median(median),
            q25=median(q25),
            q75=median(q75))

ggplot(res_fi,
       aes(x=k, y=mean, colour=method))+
  #geom_smooth(alpha=0.2,se = FALSE)+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = q25, ymax = q75), width = 0.1, alpha=0.4) +
  scale_color_manual(values = c( "dodgerblue", "chartreuse2"), breaks = c("Ours","LDA"),
                     labels = c("Ours","LDA"))+
  ylab("Topic Resolution") +
  xlab("K3")





