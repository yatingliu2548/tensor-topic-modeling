## Real-data pipeline for the vaginal microbiome experiment.
## This script keeps only the tensor topic model workflow.

suppressPackageStartupMessages({
  library(dplyr)
})

get_repo_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1])))
  } else {
    script_dir <- getwd()
  }
  normalizePath(file.path(script_dir, ".."), mustWork = TRUE)
}

repo_root <- get_repo_root()
real_data_dir <- file.path(repo_root, "real_data")

required_functions <- c("score", "tensorization", "heatmap_matrix", "plot_slice")
missing_functions <- required_functions[!vapply(required_functions, exists, logical(1), mode = "function")]
if (length(missing_functions) > 0) {
  stop(
    "Missing required functions. Load methods/vh_methods/load_vh_methods.R and methods/score_based_methods.R before running this script."
  )
}

prepare_vaginal_data <- function(
  metadata_path = file.path(real_data_dir, "selected_nonpreg_subject_m.csv"),
  counts_path = file.path(real_data_dir, "vaginal_microbiota.csv")
) {
  metadata <- read.csv(metadata_path, check.names = FALSE, stringsAsFactors = FALSE)
  if (names(metadata)[1] %in% c("", "X", "...1")) {
    metadata <- metadata[, -1, drop = FALSE]
  }

  counts_df <- read.csv(counts_path, check.names = FALSE, stringsAsFactors = FALSE)
  sample_ids <- counts_df[[1]]
  counts_matrix <- as.matrix(counts_df[, -1, drop = FALSE])
  storage.mode(counts_matrix) <- "numeric"
  rownames(counts_matrix) <- sample_ids

  required_cols <- c("SampleID", "Subject_m", "expected_status2")
  missing_cols <- setdiff(required_cols, names(metadata))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "Missing required metadata columns: %s",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  metadata <- metadata %>%
    filter(SampleID %in% rownames(counts_matrix)) %>%
    mutate(expected_status2 = factor(expected_status2, levels = unique(expected_status2))) %>%
    arrange(Subject_m, expected_status2)

  counts_matrix <- counts_matrix[metadata$SampleID, , drop = FALSE]

  if (any(is.na(counts_matrix))) {
    stop("Count matrix contains missing values after alignment.")
  }

  list(metadata = metadata, counts_matrix = counts_matrix)
}

top_words_by_topic <- function(topic_word_matrix, top_n = 15) {
  if (is.null(rownames(topic_word_matrix))) {
    rownames(topic_word_matrix) <- paste0("token_", seq_len(nrow(topic_word_matrix)))
  }
  topic_labels <- colnames(topic_word_matrix)
  if (is.null(topic_labels)) {
    topic_labels <- paste0("topic_", seq_len(ncol(topic_word_matrix)))
  }

  rows <- lapply(seq_len(ncol(topic_word_matrix)), function(i) {
    ord <- order(topic_word_matrix[, i], decreasing = TRUE)
    ord <- ord[seq_len(min(top_n, length(ord)))]
    data.frame(
      topic = topic_labels[i],
      token = rownames(topic_word_matrix)[ord],
      weight = topic_word_matrix[ord, i],
      row.names = NULL
    )
  })

  do.call(rbind, rows)
}

run_vaginal_real_data <- function(
  K1 = 3,
  K2 = 4,
  K3 = 9,
  normalize_method = "Ours",
  threshold = FALSE,
  plot_results = TRUE
) {
  prepared <- prepare_vaginal_data()
  metadata <- prepared$metadata
  counts_matrix <- prepared$counts_matrix

  Q1 <- length(unique(metadata$Subject_m))
  Q2 <- length(unique(metadata$expected_status2))
  Q3 <- ncol(counts_matrix)

  if (nrow(counts_matrix) != Q1 * Q2) {
    stop(
      sprintf(
        "Tensor shape mismatch: nrow(counts)=%d but Q1*Q2=%d.",
        nrow(counts_matrix),
        Q1 * Q2
      )
    )
  }

  total_reads <- rowSums(counts_matrix)
  total_reads[total_reads == 0] <- 1
  means_M <- mean(total_reads)

  normalized_counts <- sweep(counts_matrix, 1, total_reads, "/")
  tensor_counts <- tensorization(
    t(normalized_counts),
    mode = 3,
    Q1 = Q1,
    Q2 = Q2,
    Q3 = Q3
  )

  fit <- score(
    tensor_counts,
    K1 = K1,
    K2 = K2,
    K3 = K3,
    M = means_M,
    normalize = normalize_method,
    threshold = threshold
  )

  fit$hatA1 <- as.matrix(fit$hatA1)
  fit$hatA2 <- as.matrix(fit$hatA2)
  fit$hatA3 <- as.matrix(fit$hatA3)
  rownames(fit$hatA1) <- unique(metadata$Subject_m)
  rownames(fit$hatA2) <- unique(as.character(metadata$expected_status2))
  rownames(fit$hatA3) <- colnames(counts_matrix)
  colnames(fit$hatA3) <- paste0("topic_", seq_len(ncol(fit$hatA3)))

  fit$top_words <- top_words_by_topic(fit$hatA3, top_n = 15)
  fit$metadata <- metadata

  if (plot_results) {
    heatmap_matrix(fit$hatA1, xlab = "Latent Group", ylab = "Subject")
    heatmap_matrix(fit$hatA2, xlab = "Latent Group", ylab = "Cycle Status")
    heatmap_matrix(fit$hatA3, xlab = "Latent Group", ylab = "Taxon")
    plot_slice(
      fit$hatcore@data,
      k = 2,
      xlab = "Subject Group",
      ylab = "Taxon Topic",
      yes = TRUE
    )
  }

  fit
}

if (sys.nframe() == 0) {
  set.seed(1234)
  model_fit <- run_vaginal_real_data()
  print(dim(model_fit$hatA1))
  print(dim(model_fit$hatA2))
  print(dim(model_fit$hatA3))
  print(head(model_fit$top_words, 20))
}
