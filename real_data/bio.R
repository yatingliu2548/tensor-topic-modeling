mae <- readRDS("C:/Users/yichugang/Downloads/VMRC-subcommunities-analyses-20230725/results/mae_for_analyses.Rds")
# 
source("C:/Users/yichugang/Downloads/VMRC-subcommunities-analyses-20230725/analyses/00_setup.R")
load("C:/Users/yichugang/Downloads/VMRC-subcommunities-analyses-20230725/results/gammas.Rdata")
count_assay <- "VM16S_combined"
counts <- assay(mae, count_assay) %>% t()

si <- colData(mae) %>% as.data.frame()
number_of_cycles <-
  si %>%
  filter(SampleID %in% gammas$d) %>%
  filter(!is.na(cycleday)) %>%
  group_by(Subject, cycle_nb_m) %>%
  summarize(n_days = n(), range_cycleday = range(cycleday) %>% diff, .groups = "drop") %>%
  filter(n_days >= 10, range_cycleday >= 18) %>%
  select(Subject, cycle_nb_m) %>%
  distinct() %>%
  group_by(Subject) %>%
  summarize(n_cycles = n())


df <-
  colData(mae) %>%
  as.data.frame() %>%
  select(SampleID, Subject, Status, Race, Site,Reprod_status,Age,PH,BMI,cycleday) %>%
  filter(SampleID %in% rownames(counts))
# clean data
nonpreg=df%>% filter(Status=="Non-pregnant")
nonpreg$Reprod_status <- sub("^[^,]*,\\s*", "", nonpreg$Reprod_status )
nonpreg=nonpreg%>%
  filter(Reprod_status%in% c("menses","luteal","follicular","peri-ovulatory"))%>%
  arrange(Reprod_status,Subject)

dim_select=nonpreg%>%
  group_by(Reprod_status,Subject)%>%
  count(Reprod_status,Subject)#%>%group_by(Reprod_status)%>%summarise(min=min(n))

Subject_outlier=dim_select%>% filter(n<4)
nonpreg=nonpreg[!nonpreg$Subject%in% Subject_outlier$Subject,]
nonpreg=nonpreg%>%
  group_by(Reprod_status,Subject) %>%
  sample_n(size = 4, replace = FALSE) %>%
  ungroup()%>%
  arrange(Reprod_status,Subject)
####################################################################
nonpreg <- read_csv("nonpreg.csv")
nonpreg$week=as.integer(sub(".*W(\\d+)D.*", "\\1",nonpreg$SampleID))
dim_select=nonpreg%>%
    group_by(Subject)%>%
     count(Subject)#
remove_id=dim_select[dim_select$n<16,]$Subject
nonpreg=nonpreg%>%filter(!Subject %in% remove_id)%>%arrange(Subject,Reprod_status,week)
subcounts <- read_csv("subcounts.csv")
sub_counts=as.matrix(subcounts[, -1])
rownames(sub_counts) <- subcounts$...1
sub_counts=sub_counts[nonpreg$SampleID,]

total_M=rowSums(sub_counts)
means_M=mean(total_M)
#sub_counts=sub_counts/total_M
nonpreg_mean=nonpreg%>%
  group_by(Subject)%>%
  summarise(age_mean=mean(Age),
            PH_mean=mean(PH))
tensor_counts =tensorization(t(sub_counts/total_M),3,Q1=21,Q2=16,Q3=dim(sub_counts)[2])

score_para=score(tensor_counts,K1=3,K2=4,K3=9,M=means_M,normalize="Ours")
hatA1=score_para$hatA1
hatA2=score_para$hatA2
hatA3=score_para$hatA3
rownames(hatA3)=colnames(sub_counts)
rownames(hatA1)=unique(nonpreg$Subject)
colnames(hatA1)=c("White","Hispanic/Latino","Black")
rownames(hatA2)=rep(c("follicular","luteal","menses","peri-ovulatory"),each=4)
colnames(hatA2)=c(-14,0,7,-10) # -14 is peri-ovu, 7 is folli, -10 
  #c(-1,1,0)# -1 is before mense(peri and luteal)
#colnames(hatA2)=c("non-mense","menses")
hatcore=score_para$hatcore
hatcore_3=matrization_tensor(hatcore,3)
#hat_core_3
# Creating a list to store combinations
combinations=as.data.frame(expand.grid(colnames(hatA2),colnames(hatA1)))
combinations$categ=paste0(combinations[,1],",",combinations[,2])
colnames(hatcore_3)=combinations$categ

hatcore_df <- melt(hatcore_3)
names(hatcore_df) <- c("RowName", "ColName", "Value")
hatcore_df=separate(hatcore_df, col = ColName, into = c("mense_status", "Race"), sep = ",")
gamma_ours=get_gammas_from_alignment(alignment = aligned_topics_transport_ours,m = best_K
)


labels=as_labeller(c(#`8` = "8=I",`9`="9=III",`4`="4=II",`2`="2=V"#,
                     #`1`="1=IV-CO",`3`="3=IV-CI"#,
                     `5`="5=IV-A",`7`="7=IV-B.a",`6`="6=IV-B.b"
                     ))
ggplot(hatcore_df%>%filter(RowName%in% c(5,6,7)),
       aes(x=as.numeric(mense_status), y=(Value),group=Race,color=Race)) +
  geom_line(linewidth=1) +#
  geom_point(size=1.2)+
  #scale_y_log10() +
  #scale_x_log10() +
  facet_grid(RowName~., scales="free",labeller=as_labeller(labels))+
  xlab("Menstrual cycle type") +  labs(colour="Race") + 
  ylab("topic-proportion")



plot_composition_consecutive_cycles(df_topics_consecutive_cycles)

topic_person=matrization_tensor(hatcore,3)%*%kronecker(t(hatA1),t(hatA2))
colnames(topic_person)=rownames(sub_counts)


library(rgl)
library(plotly)
core_tensor=hatcore@data
expand.grid(x = 1:dim(core_tensor)[1], 
            y = 1:dim(core_tensor)[2], 
            z = 1:dim(core_tensor)[3]) -> coords
coords$value <- as.vector(core_tensor@data)

with(coords, plot3d(x, y, z, col = rainbow(1000)[rank(value)], size = 1))
plot_ly(data = coords, x = ~x, y = ~y, z = ~z, 
        type = 'scatter3d', mode = 'markers',
        marker = list(size = 10,  # Increased size for block-like appearance
                      color = ~value,
                      colorscale = 'Blues',
                      opacity = 0.8))  




lda_varying_params_lists <-  list()
for (k in 1:9) {
  lda_varying_params_lists[[paste0("k",k)]] <- list(k = k)
}
lda_models_full  <- 
    run_lda_models(
       data = sub_counts,
       lda_varying_params_lists = lda_varying_params_lists,
       lda_fixed_params_list = list(method = "VEM"),
       dir = "metagenomics_lda_models_full/",
       reset = TRUE,
       verbose = TRUE
   )

K1=3
K2=4
Q1=21
Q2=16
topic_models_ours_full <- run_topic_models(as.matrix(sub_counts),1:nrow(sub_counts),K1=K1,K2=K2,Q1=Q1,Q2=Q2,list_params=1:9,normalize="Ours")

aligned_topics_transport_ours<- 
    align_topics(
      models = topic_models_ours_full[1:9],
      method = "transport")
plot(aligned_topics_transport_ours, add_leaves = TRUE, label_topics = TRUE)
ttr=aligned_topics_transport_ours@topics
aligned_topics_transport_ours@topics <- 
  aligned_topics_transport_ours@topics %>% 
  mutate(
    k_label = k_label 
    %>% factor(., levels = levels(k_label) %>% sort)
  ) 
path_to_topics_our <- 
  aligned_topics_transport_ours@topics %>% 
  filter(m == str_c("k", best_K)) %>% 
  select(path, k_label) %>% 
  arrange(path)
path_to_topics=path_to_topics%>% arrange(k_label)
path_to_topics_our$k_label2=path_to_topics$k_label
path_to_topics_our=path_to_topics_our%>%arrange(k_label)
label1=c(1,2,3,4,5,1,2,3,4,5,6,7,8,9)
label2=c(1,2,3,4,5,1,2,3,4,5,6,7,8,9)
label_mapping <- setNames(path_to_topics_our$k_label2, path_to_topics_our$k_label)

alto::plot_beta(
  aligned_topics_transport_ours, 
  models = c(coarse_K, best_K), 
  x_axis = "label",
  color_by = "path",
  threshold = 0.005
) #+scale_color_manual(values = get_topic_colors(path_to_topics$k_label)) +
  guides(col = "none")#+
  scale_x_discrete(labels = label_mapping)




aligned_topics_transport_comp <-
    align_topics(
      models = lda_models_full[1:9],
      method = "transport")
plot(aligned_topics_transport_comp, add_leaves = TRUE, label_topics = TRUE)
