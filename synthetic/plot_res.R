library(tidyverse)
library(stringr)


# Set your directory where files are located
directory <- "~/Documents/tensor-topic-modeling/synthetic/results"

# List all files in the directory
files <- list.files(path = directory, pattern = "*.csv", full.names = TRUE)
res <- c()
for (file in files){
  
  extracted_numbers <- str_extract(file, "[0-9\\-_]+(?=\\.csv)")
  # Split the numbers by - and _
  split_numbers <- unlist(strsplit(extracted_numbers, "[-_]"))
  temp_df <- read_csv(file)
  temp_df["K1"] = as.numeric(split_numbers[2])
  temp_df["K2"] = as.numeric(split_numbers[3])
  temp_df["K3"] = as.numeric(split_numbers[4])
  temp_df["exp"] = as.numeric(split_numbers[5])
  temp_df["seed"] = as.numeric(split_numbers[6])
  res <- rbind(res, temp_df)
}



res_summary <- res %>% 
  group_by(Q1, Q2, R, M, K1, K2, K3, mode, method) %>%
  summarise(error_mean = mean(error),
            error_q25 = quantile(error, 0.25),
            error_q50 = quantile(error, 0.5),
            error_q75 = quantile(error, 0.75),
            time_mean = mean(time),
            time_q25 = quantile(time, 0.25),
            time_q50 = quantile(time, 0.5),
            time_q75 = quantile(time, 0.75))
unique(res$K1)
unique(res$K2)
unique(res$R)
unique(res$K3)
unique(res$Q2)

no_go <- which((res_summary$Q1 == 100) & (res_summary$Q2 == 30))

res_summary <- res_summary %>% 
  mutate(M_title = paste0("M = ", M),
         R_title = paste0("R = ", R),
         method_name = ifelse(method == "bayesian", "Tensor LDA",
                              ifelse(method == "LDA", "Hybrid LDA",
                                     method)),
         Q1Q2_title = paste0("Q1 = ", Q1, "\n",
                             "Q2 = ", Q2))
res_summary$R_title = factor(res_summary$R_title ,
                             levels = c("R = 100", "R = 500", "R = 1000"))

ggplot(res_summary[-no_go,] %>% filter(mode == "A1",
                       K2 == 2, Q1 < 2000), aes(x = M, y = error_q50,
                                          colour = method_name)) + 
  geom_line(position = position_dodge(width = 0.15), linewidth=0.6) + 
  geom_point(position = position_dodge(width = 0.15), size=2) + 
  geom_errorbar(aes(ymin = error_q25, ymax = error_q75), 
                position = position_dodge(width = 0.15), size = 0.6) + 
  facet_grid(Q1Q2_title ~ R_title, scales="free") + 
  ylab("Mode 1 : l1 error")+ 
  scale_y_log10() + 
  scale_x_log10() + 
  labs(colour = "Method") + 
  theme_bw()

ggplot(res_summary[-no_go,] %>% filter(mode == "A2",
                                       K2 == 2, Q1 < 2000), aes(x = M, y = error_q50,
                                                                colour = method_name)) + 
  geom_line(position = position_dodge(width = 0.15), linewidth=0.6) + 
  geom_point(position = position_dodge(width = 0.15), size=2) + 
  geom_errorbar(aes(ymin = error_q25, ymax = error_q75), 
                position = position_dodge(width = 0.15), size = 0.6) + 
  facet_grid(Q1Q2_title ~ R_title, scales="free") + 
  ylab("Mode 2 : l1 error")+ 
  scale_y_log10() + 
  scale_x_log10() + 
  labs(colour = "Method") + 
  theme_bw()


ggplot(res_summary[-no_go,] %>% filter(mode == "A3",
                                       K2 == 2, Q1 < 2000), aes(x = M, y = error_q50,
                                                                colour = method_name)) + 
  geom_line(position = position_dodge(width = 0.15), linewidth=0.6) + 
  geom_point(position = position_dodge(width = 0.15), size=2) + 
  geom_errorbar(aes(ymin = error_q25, ymax = error_q75), 
                position = position_dodge(width = 0.15), size = 0.6) + 
  facet_grid(Q1Q2_title ~ R_title, scales="free") + 
  ylab("Mode 3 : l1 error")+ 
  scale_y_log10() + 
  scale_x_log10() + 
  labs(colour = "Method") + 
  theme_bw()

ggplot(res_summary[-no_go,] %>% filter(mode == "core",
                                       K2 == 2, Q1 < 2000), aes(x = M, y = error_q50,
                                                                colour = method_name)) + 
  geom_line(position = position_dodge(width = 0.15), linewidth=0.6) + 
  geom_point(position = position_dodge(width = 0.15), size=2) + 
  geom_errorbar(aes(ymin = error_q25, ymax = error_q75), 
                position = position_dodge(width = 0.15), size = 0.6) + 
  facet_grid(Q1Q2_title ~ R_title, scales="free") + 
  ylab("Core : l1 error")+ 
  scale_y_log10() + 
  scale_x_log10() + 
  labs(colour = "Method") + 
  theme_bw()


ggplot(res_summary[-no_go,] %>% filter(mode == "A2",
                                       K2 == 2, Q1 < 2000), aes(x = M, y = error_q50,
                                                                colour = method)) + 
  geom_line(position = position_dodge(width = 0.5), linewidth=0.6) + 
  geom_point(position = position_dodge(width = 0.5), size=2) + 
  geom_errorbar(aes(ymin = error_q25, ymax = error_q75), 
                position = position_dodge(width = 0.5), size = 0.6) + 
  facet_grid(Q1Q2_title ~ R_title, scales="free") + 
  ylab("Mode 1: l1 Error")+ 
  scale_y_log10() + 
  scale_x_log10() + 
  labs(colour = "Method") + 
  theme_bw()


ggplot(res_summary[-no_go,] %>% filter(mode == "core",
                                       K2 == 2, Q1 < 2000), aes(x = M, y = time_q50,
                                                                colour = method_name)) + 
  geom_line(position = position_dodge(width = 0.15), linewidth=0.6) + 
  geom_point(position = position_dodge(width = 0.15), size=2) + 
  geom_errorbar(aes(ymin = time_q25, ymax = time_q75), 
                position = position_dodge(width = 0.15), size = 0.6) + 
  facet_grid(Q1Q2_title ~ R_title, scales="free") + 
  ylab("Time (s)")+ 
  scale_y_log10() + 
  scale_x_log10() + 
  labs(colour = "Method") + 
  theme_bw()
