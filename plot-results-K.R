library(tidyverse)
library(stringr)


# Set your directory where files are located
directory <- "~/Documents/tensor-topic-modeling/synthetic/results"

# List all files in the directory
files <- list.files(path = directory, pattern = "^exp_K.*-234.*\\.csv$", full.names = TRUE)


res <- c()
for (file in files){
  file_name <- str_extract(file, "[^/]+\\.csv$")  # Extract the file name ending with .csv
  file_name <- str_remove(file_name, "\\.csv$")  # Remove the .csv extension
  
  split_numbers <- str_split(file_name, "[-_]", simplify = TRUE)
  # Split the numbers by - and _
  temp_df <- read_csv(file)
  temp_df["Q1_check"] = as.numeric(split_numbers[3])
  temp_df["Q2_check"] = as.numeric(split_numbers[3])
  temp_df["R_check"] = as.numeric(split_numbers[4])
  temp_df["sparse"] = as.logical(split_numbers[5])
  temp_df["exp"] = as.numeric(split_numbers[6])
  temp_df["seed"] = as.numeric(split_numbers[7])
  res <- rbind(res, temp_df)
}

a1_res = res[which(res$mode == "A1"),]
a3_res = res[which(res$mode == "A3"),]


unique(a1_res$K2)
unique(a3_res$K2)

unique(a1_res$K)
unique(a3_res$K)

a1_summary <- a1_res %>% 
  dplyr::group_by(Q1, Q2, R, M, K, mode, method, sparse) %>%
  dplyr::summarise(error_mean = mean(error),
                   error_q25 = quantile(error, 0.25),
                   error_q50 = quantile(error, 0.5),
                   error_q75 = quantile(error, 0.75),
                   time_mean = mean(time),
                   counts = n(),
                   time_q25 = quantile(time, 0.25),
                   time_q50 = quantile(time, 0.5),
                   time_q75 = quantile(time, 0.75)
                   )

a3_summary <- a3_res %>% 
  dplyr::group_by(Q1, Q2, R, M, K, mode, method, sparse) %>%
  dplyr::summarise(error_mean = mean(error),
                   error_q25 = quantile(error, 0.25),
                   error_q50 = quantile(error, 0.5),
                   error_q75 = quantile(error, 0.75),
                   time_mean = mean(time),
                   counts = n(),
                   time_q25 = quantile(time, 0.25),
                   time_q50 = quantile(time, 0.5),
                   time_q75 = quantile(time, 0.75))

res_summary <- res %>% 
  dplyr::group_by(Q1, Q2, R, M, K, mode, method, sparse) %>%
  dplyr::summarise(error_mean = mean(error),
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
unique(res$K)

unique(res$Q2)
unique(res$Q1)
unique(res$M)
unique(res$method)

length(res_summary$mode == "A1")
length(res_summary$mode == "A3")
a1_res = res_summary %>% filter(mode == "A1")
res_summary <- res_summary %>% 
  mutate(M_title = paste0("M = ", M),
         R_title = paste0("R = ", R),
         K3_title = paste0("K3 = ", K),
         method_name = ifelse(method == "bayesian", "Tensor LDA",
                              ifelse(method == "LDA", "Hybrid LDA",
                                     method)),
         Q1Q2_title = paste0("Q1 = ", Q1, "\n",
                             "Q2 = ", Q2))
res_summary$R_title = factor(res_summary$R_title ,
                             levels = c("R = 100", "R = 500", "R = 1000",
                                        "R = 5000","R = 10000",
                                        "R = 20000","R = 50000"))


unique(res_summary$Q1)
unique(res_summary$Q2)
unique(res_summary$M)
unique(res_summary$method)


ggplot(res_summary%>% filter(mode == "core", K==3), aes(x = M, y = error_mean,
                                           colour = method_name)) + 
  geom_line(position = position_dodge(width = 0.15), linewidth=0.6) + 
  geom_point(position = position_dodge(width = 0.15), size=2) + 
  #geom_errorbar(aes(ymin = error_q25, ymax = error_q75), 
  #              position = position_dodge(width = 0.15), size = 0.6) + 
  facet_grid(Q1Q2_title ~  R_title, scales="free") + 
  ylab("Mode 1 : l1 error")+ 
  #scale_y_log10() + 
  scale_x_log10() + 
  labs(colour = "Method") + 
  theme_bw()



ggplot(res_summary%>% filter(mode == "core",
                             K2 == 2), aes(x = R, y = error_q50,
                                           colour = method_name)) + 
  geom_line(position = position_dodge(width = 0.15), linewidth=0.6) + 
  geom_point(position = position_dodge(width = 0.15), size=2) + 
  geom_errorbar(aes(ymin = error_q25, ymax = error_q75), 
                position = position_dodge(width = 0.15), size = 0.6) + 
  facet_grid(K3_title ~ M_title, scales="free") + 
  ylab("Mode 1 : l1 error")+ 
  scale_y_log10() + 
  scale_x_log10() + 
  labs(colour = "Method") + 
  theme_bw()


ggplot(res_summary %>% filter(mode == "core",
                              K2 == 2, Q1 < 2000), aes(x = M, y = error_q50,
                                                       colour = method_name)) + 
  geom_line(position = position_dodge(width = 0.15), linewidth=0.6) + 
  geom_point(position = position_dodge(width = 0.15), size=2) + 
  geom_errorbar(aes(ymin = error_q25, ymax = error_q75), 
                position = position_dodge(width = 0.15), size = 0.6) + 
  facet_grid(K3_title ~ R_title, scales="free") + 
  ylab("Mode 2 : l1 error")+ 
  scale_y_log10() + 
  scale_x_log10() + 
  labs(colour = "Method") + 
  theme_bw()

unique(res_summary$K1)
ggplot(res_summary %>% filter(mode == "core",
                              K2 == 2, 
                              Q1 < 2000), aes(x = R, y = error_q50,
                                              colour = method_name)) + 
  geom_line(position = position_dodge(width = 0.15), linewidth=0.6) + 
  geom_point(position = position_dodge(width = 0.15), size=2) + 
  geom_errorbar(aes(ymin = error_q25, ymax = error_q75), 
                position = position_dodge(width = 0.15), size = 0.6) + 
  facet_grid(K3_title ~ M_title, scales="free") + 
  ylab("Mode 3 : l1 error")+ 
  scale_y_log10() + 
  scale_x_log10() + 
  labs(colour = "Method") + 
  theme_bw()

ggplot(res_summary %>% filter(mode == "core",
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


ggplot(res_summary%>% filter(mode == "core",
                             K2 == 2, Q1 < 2000), aes(x = M, y = time_q50,
                                                      colour = method_name)) + 
  geom_line(position = position_dodge(width = 0.15), linewidth=0.6) + 
  geom_point(position = position_dodge(width = 0.15), size=2) + 
  geom_errorbar(aes(ymin = time_q25, ymax = time_q75), 
                position = position_dodge(width = 0.15), size = 0.6) + 
  facet_grid(K3_title ~ R_title, scales="free") + 
  ylab("Time (s)")+ 
  scale_y_log10() + 
  scale_x_log10() + 
  labs(colour = "Method") + 
  theme_bw()
