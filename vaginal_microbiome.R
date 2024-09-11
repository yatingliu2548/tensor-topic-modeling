library(reshape2)
library(reshape)
library(dplyr)

setwd("~/Documents/tensor-topic-modeling/")
source("algorithm.R")
source("analysis_function.R")
source("bayesian.R")

selected_nonpreg_subject_m= read_csv("selected_nonpreg_subject_m.csv")
counts_select=read_csv("vaginal_microbiota.csv")

set.seed(1234)

mae <- readRDS("~/Downloads/VMRC-subcommunities-analyses-20230725 2/results/mae_for_analyses.Rds")

count_assay <- "VM16S_combined"
counts <- assay(mae, count_assay) %>% t() 
df <- 
  colData(mae) %>% 
  as.data.frame() %>% 
  select(SampleID, Subject, Status) %>% 
  filter(SampleID %in% rownames(counts))


df %>% 
  group_by(Status) %>% 
  summarize(
    n_samples = n(),
    n_subjects = length(unique(Subject))
  ) %>% 
  kable(
    ., format = "latex", booktab = TRUE, linesep = "",
    caption = "Number of samples per reproductive status on which topic models were fitted"
  ) %>%
  kable_styling(latex_options = "HOLD_position")


mc_data <- 
  colData(mae) %>% 
  as.data.frame() %>% 
  filter(Status != "Pregnant", !is.na(cycleday), MC_ok) %>% 
  dplyr::select(SampleID, Subject, cycle_nb_m, cycleday) %>% 
  mutate(d = SampleID)

test = mc_data %>% group_by(SampleID, Subject)

df_viz <- 
  df %>% 
  group_by(Subject, cycle_nb_m, k) %>% 
  mutate(has_k = any(g > 0.05)) %>% 
  ungroup() %>% 
  filter(has_k) 

nonpreg <-
  colData(mae) %>%
  as.data.frame() %>%
  #select(SampleID, Subject, Status, Race, Cohort, Site, Reprod_status, Age, PH, BMI, 
  #       cycle_nb_m, cycleday, cycle_length, cycleday_fw, cycleday_bw, MC_ok,Bleeding) %>%
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


