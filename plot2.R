library(tidyverse)
library(stringr)


# Set your directory where files are located
directory <- "~/Documents/tensor-topic-modeling/synthetic/results_final/results_final"

# List all files in the directory
files <- list.files(path = directory, pattern = "exp_2.*\\.csv", full.names = TRUE)
res <- c()
for (file in files){
  file_name <- str_extract(file, "[^/]+\\.csv$")  # Extract the file name ending with .csv
  file_name <- str_remove(file_name, "\\.csv$")  # Remove the .csv extension
  
  split_numbers <- str_split(file_name, "[-_]", simplify = TRUE)
  # Split the numbers by - and _
  temp_df <- read_csv(file)
  temp_df["K1_check"] = as.numeric(split_numbers[2])
  temp_df["K2_check"] = as.numeric(split_numbers[3])
  temp_df["K3_check"] = as.numeric(split_numbers[4])
  temp_df["sparse"] = as.logical(split_numbers[5])
  temp_df["exp"] = as.numeric(split_numbers[6])
  temp_df["seed"] = as.numeric(split_numbers[7])
  res <- rbind(res, temp_df)
}

res["K3"] = res$K

res_summary <- res %>% 
  group_by(Q1, Q2, R, M, K1, K2, K, mode, method, sparse) %>%
  summarise(error_mean = mean(error),
            error_q25 = quantile(error, 0.25),
            error_q50 = quantile(error, 0.5),
            error_q75 = quantile(error, 0.75),
            time_mean = mean(time),
            counts = n(),
            time_q25 = quantile(time, 0.25),
            time_q50 = quantile(time, 0.5),
            time_q75 = quantile(time, 0.75))

unique(res$K1)
unique(res$K2)
unique(res$R)
unique(res$K3)
unique(res$Q2)
unique(res$Q1)
unique(res$M)
unique(res_summary$K)

res_summary <- res_summary %>% 
  mutate(M_title = paste0("M = ", M),
         R_title = paste0("R = ", R),
         K3_title = paste0("K3 = ", K3),
         method_name = ifelse(method == "bayesian", "Tensor LDA",
                              ifelse(method == "LDA", "Hybrid LDA",
                                     method)),
         Q1Q2_title = paste0("Q1 = ", Q1, "\n",
                             "Q2 = ", Q2))
res_summary$R_title = factor(res_summary$R_title ,
                             levels = c("R = 100", "R = 500", "R = 1000",
                                        "R = 5000","R = 10000",
                                        "R = 20000","R = 30000","R = 50000"))


res_summary$Q1Q2_title = factor(res_summary$Q1Q2_title ,
                                levels = c( "Q1 = 5\nQ2 = 5",
                                            "Q1 = 10\nQ2 = 10",
                                            "Q1 = 15\nQ2 = 15" , 
                                            "Q1 = 30\nQ2 = 30",
                                            "Q1 = 50\nQ2 = 50",
                                            "Q1 = 70\nQ2 = 70",
                                            "Q1 = 100\nQ2 = 100"))

test1 = res_summary%>% filter(mode == "A1")
test2 = res_summary%>% filter(mode == "core")


legend_order <- c( "Tensor LDA" , "Hybrid LDA",
                   "STM" ,
                   "NTD" ,
                   "TopicScore-HOSVD",
                   "TTM-HOSVD",
                   "TTM-HOOI" )
my_colors <- c( "black", "skyblue", "chartreuse4", "orange", "grey", "purple", "magenta")

labels_n <- c( "Tensor LDA" , "Hybrid LDA",
               "STM" ,
               "NTD" ,
               "TopicScore-HOSVD",
               "TTM-HOSVD",
               "TTM-HOOI" )
theme_set(theme_bw(base_size = 18))



unique(res_summary$method)
unique(res_summary$M)

ggplot(res_summary%>% filter(mode == "core", R %in% c(500), Q1<51), aes(x = M, y = error_q50,
                                                               colour = method_name)) + 
  geom_line(position = position_dodge(width = 0.2), linewidth=0.6) + 
  geom_point(position = position_dodge(width = 0.2), size=2) + 
  geom_errorbar(aes(ymin = error_q25, ymax = error_q75), 
                position = position_dodge(width = 0.2), size = 0.6) + 
  facet_grid( R_title ~ Q1Q2_title, scales="free", 
             labeller = as_labeller(c("R = 1000" = "R = 1,000",
                                      "R = 10000" = "R = 10,000",
                                      "R = 500" = "R = 500",
                                      "R = 5000" = "R = 5,000",
                                      "R = 30000" = "R = 30,000",
                                      "Q1 = 10\nQ2 = 10" =  "N1 = 10\nN2 = 10",
                                      "Q1 = 5\nQ2 = 5" =    "N1 = 5\nN2 = 5",
                                      "Q1 = 10\nQ2 = 10"=    "N1 = 10\nN2 = 10",
                                      "Q1 = 15\nQ2 = 15" = "N1 = 15\nN2 = 15" , 
                                      "Q1 = 30\nQ2 = 30"=   "N1 = 30\nN2 = 30",
                                      "Q1 = 50\nQ2 = 50"="N1 = 50\nN2 = 50",
                                      "Q1 = 70\nQ2 = 70"="N1 = 70\nN2 = 70",
                                      "Q1 = 100\nQ2 = 100"=    "N1 = 100\nN2 = 100"))) + 
  ylab("Core : l1 error")+ 
  scale_y_log10() + 
  scale_x_log10() + 
  scale_color_manual(values = my_colors, breaks = legend_order,
                     labels = labels_n) +
  labs(colour = "Method") + 
  theme_bw()


ggplot(res_summary%>% filter(mode == "A3", Q1 %in% c(5, 15, 30, 50), M==10000), aes(x = R, y = error_q50,
                                                               colour = method_name)) + 
  geom_line(position = position_dodge(width = 0.2), linewidth=0.6) + 
  geom_point(position = position_dodge(width = 0.2), size=2) + 
  geom_errorbar(aes(ymin = error_q25, ymax = error_q75), 
                position = position_dodge(width = 0.2), size = 0.6) + 
  facet_grid(Q1Q2_title ~ M_title, scales="free", 
             labeller = as_labeller(c("R = 1000" = "R = 1,000",
                                      "R = 10000" = "R = 10,000",
                                      "R = 500" = "R = 500",
                                      "R = 5000" = "R = 5,000",
                                      "R = 30000" = "R = 30,000",
                                      "M = 1000" = "M = 1,000",
                                      "Q1 = 10\nQ2 = 10" =  "N1 = 10\nN2 = 10",
                                      "Q1 = 5\nQ2 = 5" =    "N1 = 5\nN2 = 5",
                                      "Q1 = 10\nQ2 = 10"=    "N1 = 10\nN2 = 10",
                                      "Q1 = 15\nQ2 = 15" = "N1 = 15\nN2 = 15" , 
                                      "Q1 = 30\nQ2 = 30"=   "N1 = 30\nN2 = 30",
                                      "Q1 = 50\nQ2 = 50"="N1 = 50\nN2 = 50",
                                      "Q1 = 70\nQ2 = 70"="N1 = 70\nN2 = 70",
                                      "Q1 = 100\nQ2 = 100"=    "N1 = 100\nN2 = 100"))) + 
  ylab("A3 : l1 error")+ 
  scale_y_log10() + 
  scale_x_log10() + 
  scale_color_manual(values = my_colors, breaks = legend_order,
                     labels = labels_n) +
  labs(colour = "Method") + 
  theme_bw()

ggplot(res_summary%>% filter(mode == "A3", R<6000), aes(x = M, y = error_q50,
                                                colour = method_name)) + 
  geom_line(position = position_dodge(width = 0.2), linewidth=0.6) + 
  geom_point(position = position_dodge(width = 0.2), size=2) + 
  geom_errorbar(aes(ymin = error_q25, ymax = error_q75), 
                position = position_dodge(width = 0.2), size = 0.6) + 
  facet_grid(Q1Q2_title ~ R_title, scales="free", 
             labeller = as_labeller(c("R = 1000" = "R = 1,000",
                                      "R = 10000" = "R = 10,000",
                                      "R = 500" = "R = 500",
                                      "R = 5000" = "R = 5,000",
                                      "R = 30000" = "R = 30,000",
                                      "Q1 = 10\nQ2 = 10" =  "N1 = 10\nN2 = 10",
                                      "Q1 = 5\nQ2 = 5" =    "N1 = 5\nN2 = 5",
                                      "Q1 = 10\nQ2 = 10"=    "N1 = 10\nN2 = 10",
                                      "Q1 = 15\nQ2 = 15" = "N1 = 15\nN2 = 15" , 
                                      "Q1 = 30\nQ2 = 30"=   "N1 = 30\nN2 = 30",
                                      "Q1 = 50\nQ2 = 50"="N1 = 50\nN2 = 50",
                                      "Q1 = 70\nQ2 = 70"="N1 = 70\nN2 = 70",
                                      "Q1 = 100\nQ2 = 100"=    "N1 = 100\nN2 = 100"))) + 
  ylab("Core : l1 error")+ 
  scale_y_log10() + 
  scale_x_log10() + 
  scale_color_manual(values = my_colors, breaks = legend_order,
                     labels = labels_n) +
  labs(colour = "Method") + 
  theme_bw()


res_summary = res_summary %>% 
  mutate(mode_title =paste0(mode, " : l1 error"))
ggplot(res_summary%>% filter( M==1000), aes(x = R, y = error_q50,
                                                                                            colour = method_name)) + 
  geom_line(position = position_dodge(width = 0.2), linewidth=0.6) + 
  geom_point(position = position_dodge(width = 0.2), size=2) + 
  geom_errorbar(aes(ymin = error_q25, ymax = error_q75), 
                position = position_dodge(width = 0.2), size = 0.6) + 
  facet_grid(mode~Q1Q2_title , scales="free") + 
  ylab("l1 error")+ 
  scale_y_log10() + 
  scale_x_log10() + 
  scale_color_manual(values = my_colors, breaks = legend_order,
                     labels = labels_n) +
  labs(colour = "Method") + 
  theme_bw()


ggplot(res_summary%>% filter( R==1000, Q1==50), aes(x = M, y = error_q50,
                                            colour = method_name)) + 
  geom_line(position = position_dodge(width = 0.2), linewidth=0.6) + 
  geom_point(position = position_dodge(width = 0.2), size=2) + 
  geom_errorbar(aes(ymin = error_q25, ymax = error_q75), 
                position = position_dodge(width = 0.2), size = 0.6) + 
  facet_grid(mode~Q1Q2_title , scales="free") + 
  ylab("l1 error")+ 
  scale_y_log10() + 
  scale_x_log10() + 
  scale_color_manual(values = my_colors, breaks = legend_order,
                     labels = labels_n) +
  labs(colour = "Method") + 
  theme_bw()

ggplot(res_summary%>% filter( M==10000), aes(x = Q1, y = error_q50,
                                                    colour = method_name)) + 
  geom_line(position = position_dodge(width = 0.2), linewidth=0.6) + 
  geom_point(position = position_dodge(width = 0.2), size=2) + 
  geom_errorbar(aes(ymin = error_q25, ymax = error_q75), 
                position = position_dodge(width = 0.2), size = 0.6) + 
  facet_grid(mode~R_title , scales="free") + 
  ylab("l1 error")+ 
  scale_y_log10() + 
  scale_x_log10() + 
  scale_color_manual(values = my_colors, breaks = legend_order,
                     labels = labels_n) +
  labs(colour = "Method") + 
  theme_bw()


ggplot(res_summary%>% filter( M==10000), aes(x = R, y = error_q50,
                                             colour = method_name)) + 
  geom_line(position = position_dodge(width = 0.2), linewidth=0.6) + 
  geom_point(position = position_dodge(width = 0.2), size=2) + 
  geom_errorbar(aes(ymin = error_q25, ymax = error_q75), 
                position = position_dodge(width = 0.2), size = 0.6) + 
  facet_grid(mode~Q1Q2_title , scales="free") + 
  ylab("l1 error")+ 
  scale_y_log10() + 
  scale_x_log10() + 
  scale_color_manual(values = my_colors, breaks = legend_order,
                     labels = labels_n) +
  labs(colour = "Method") + 
  theme_bw()


ggplot(res_summary%>% filter(mode == "A1", M==10000), aes(x = R, y = time_q50,
                                             colour = method_name)) + 
  geom_line(position = position_dodge(width = 0.2), linewidth=0.6) + 
  geom_point(position = position_dodge(width = 0.2), size=2) + 
  geom_errorbar(aes(ymin = time_q25, ymax = time_q75), 
                position = position_dodge(width = 0.2), size = 0.6) + 
  facet_grid(.~Q1Q2_title , scales="free") + 
  ylab("Time (s)")+ 
  scale_y_log10() + 
  scale_x_log10() + 
  scale_color_manual(values = my_colors, breaks = legend_order,
                     labels = labels_n) +
  labs(colour = "Method") + 
  theme_bw()




