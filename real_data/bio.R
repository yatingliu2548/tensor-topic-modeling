# mae <- readRDS("../results/mae_for_analyses.Rds")
# 
# count_assay <- "VM16S_combined"
# counts <- assay(mae, count_assay) %>% t() 
# 
# si <- colData(mae) %>% as.data.frame()
# number_of_cycles <- 
#   si %>% 
#   filter(SampleID %in% gammas$d) %>% 
#   filter(!is.na(cycleday)) %>% 
#   group_by(Subject, cycle_nb_m) %>% 
#   summarize(n_days = n(), range_cycleday = range(cycleday) %>% diff, .groups = "drop") %>% 
#   filter(n_days >= 10, range_cycleday >= 18) %>% 
#   select(Subject, cycle_nb_m) %>% 
#   distinct() %>% 
#   group_by(Subject) %>% 
#   summarize(n_cycles = n())
# 
# 
# df <- 
#   colData(mae) %>% 
#   as.data.frame() %>% 
#   select(SampleID, Subject, Status, Race, Site,Reprod_status,Age,PH,BMI) %>% 
#   filter(SampleID %in% rownames(counts))
# ## clean data
# nonpreg=df%>% filter(Status=="Non-pregnant")
# nonpreg$Reprod_status <- sub("^[^,]*,\\s*", "", nonpreg$Reprod_status )
# nonpreg=nonpreg%>%
#   filter(Reprod_status%in% c("menses","luteal","follicular","peri-ovulatory"))%>%
#   arrange(Reprod_status,Subject)
# 
# dim_select=nonpreg%>%
#   group_by(Reprod_status,Subject)%>%
#   count(Reprod_status,Subject)#%>%group_by(Reprod_status)%>%summarise(min=min(n))
# 
# Subject_outlier=dim_select%>% filter(n<4)
# nonpreg=nonpreg[!nonpreg$Subject%in% Subject_outlier$Subject,]
# nonpreg=nonpreg%>%
#   group_by(Reprod_status,Subject) %>%
#   sample_n(size = 4, replace = FALSE) %>%
#   ungroup()%>%
#   arrange(Reprod_status,Subject)

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
sub_counts=sub_counts/total_M

tensor_counts =tensorization(t(sub_counts),3,Q1=21,Q2=16,Q3=dim(sub_counts)[2])

score_para=score(tensor_counts,K1=3,K2=4,K3=5,M=means_M,normalize="Ours")
hatA1=score_para$hatA1
hatA2=score_para$hatA2
hatA3=score_para$hatA3
hatcore=score_para$hatcore
