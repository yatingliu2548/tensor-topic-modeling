#!/usr/bin/env Rscript

find_repo_root <- function(start = getwd()) {
  cur <- normalizePath(start, mustWork = TRUE)
  repeat {
    if (
      file.exists(file.path(cur, "methods", "methods_core.R")) &&
      file.exists(file.path(cur, "synthetic", "Y3.csv"))
    ) {
      return(cur)
    }
    parent <- dirname(cur)
    if (identical(parent, cur)) {
      stop("Cannot find repository root from ", start)
    }
    cur <- parent
  }
}

parse_args <- function(args) {
  out <- list()
  for (arg in args) {
    kv <- strsplit(arg, "=", fixed = TRUE)[[1]]
    if (length(kv) == 2) {
      out[[kv[1]]] <- kv[2]
    }
  }
  out
}

`%||%` <- function(x, y) if (is.null(x) || !nzchar(as.character(x))) y else x

as_bool <- function(x) {
  tolower(as.character(x)) %in% c("1", "true", "t", "yes", "y")
}

normalize_cols <- function(mat) {
  totals <- colSums(mat)
  if (any(totals <= 0)) {
    stop("Every document/cell column in Y3 must have positive total count.")
  }
  sweep(mat, 2, totals, "/")
}

read_numeric_matrix <- function(path) {
  df <- read.csv(path, check.names = FALSE)
  first_col_numeric <- suppressWarnings(!any(is.na(as.numeric(df[[1]]))))
  if (ncol(df) > 1 && !first_col_numeric) {
    rownames(df) <- df[[1]]
    df <- df[-1]
  }
  mat <- as.matrix(df)
  storage.mode(mat) <- "numeric"
  if (anyNA(mat)) {
    stop("Input matrix contains non-numeric values after reading: ", path)
  }
  mat
}

save_heatmap <- function(mat, path, title, xlab = "Component", ylab = "Index") {
  mat <- as.matrix(mat)
  df <- expand.grid(row = seq_len(nrow(mat)), col = seq_len(ncol(mat)))
  df$value <- as.vector(mat)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = factor(col), y = row, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(option = "H") +
    ggplot2::labs(title = title, x = xlab, y = ylab, fill = "") +
    ggplot2::theme_minimal(base_size = 11)
  ggplot2::ggsave(path, p, width = 7, height = 5, dpi = 180)
  invisible(path)
}

save_core_facets <- function(core, path) {
  arr <- core@data
  df <- expand.grid(
    mode1_component = seq_len(dim(arr)[1]),
    mode2_component = seq_len(dim(arr)[2]),
    topic_component = seq_len(dim(arr)[3])
  )
  df$value <- as.vector(arr)
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = factor(mode2_component), y = factor(mode1_component), fill = value)
  ) +
    ggplot2::geom_tile() +
    ggplot2::facet_wrap(~topic_component, nrow = 1) +
    ggplot2::scale_fill_viridis_c(option = "H") +
    ggplot2::labs(
      title = "Estimated core tensor slices",
      x = "Mode-2 component",
      y = "Mode-1 component",
      fill = ""
    ) +
    ggplot2::theme_minimal(base_size = 11)
  ggplot2::ggsave(path, p, width = 10, height = 4, dpi = 180)
  invisible(path)
}

save_top_tokens_plot <- function(A3, token_names, top_n, path) {
  rows <- do.call(rbind, lapply(seq_len(ncol(A3)), function(topic) {
    ord <- order(A3[, topic], decreasing = TRUE)
    ord <- ord[seq_len(min(top_n, length(ord)))]
    data.frame(
      topic = paste0("Topic ", topic),
      token = token_names[ord],
      weight = A3[ord, topic],
      rank = seq_along(ord),
      stringsAsFactors = FALSE
    )
  }))
  rows$token_topic <- paste(rows$topic, rows$token, sep = " | ")
  rows$token_topic <- factor(rows$token_topic, levels = rev(unique(rows$token_topic)))
  p <- ggplot2::ggplot(rows, ggplot2::aes(x = weight, y = token_topic)) +
    ggplot2::geom_col(fill = "#2f7f7b") +
    ggplot2::facet_wrap(~topic, scales = "free_y") +
    ggplot2::scale_y_discrete(labels = function(x) sub("^Topic [0-9]+ \\| ", "", x)) +
    ggplot2::labs(title = "Top tokens by estimated topic", x = "A3 weight", y = "") +
    ggplot2::theme_minimal(base_size = 11)
  ggplot2::ggsave(path, p, width = 10, height = 7, dpi = 180)
  rows[c("topic", "token", "weight", "rank")]
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
repo_root <- find_repo_root()

source(file.path(repo_root, "methods", "methods_core.R"))
ensure_benchmark_sources()
source(file.path(repo_root, "methods", "analysis_function.R"))
source(file.path(repo_root, "methods", "algorithms", "ttm_hosvd.R"))

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("This example requires ggplot2 to save plots.")
}

Q1 <- as.integer(args$Q1 %||% 30)
Q2 <- as.integer(args$Q2 %||% 10)
K1 <- as.integer(args$K1 %||% 2)
K2 <- as.integer(args$K2 %||% 2)
K3 <- as.integer(args$K3 %||% 4)
top_n <- as.integer(args$top_n %||% 10)
vh_method <- as.character(args$vh_method %||% "SP")
output_dir <- args$output_dir %||% file.path(repo_root, "results", "ttm_hosvd_example")
run_identifiability <- as_bool(args$identifiability %||% "false")

y3_path <- args$Y3 %||% file.path(repo_root, "synthetic", "Y3.csv")
truth_d3_path <- args$D_truth %||% ""

identifiability_truth <- NULL
identifiability_meta <- NULL

if (run_identifiability) {
  source(file.path(repo_root, "methods", "identifiability_data_generation.R"))
  R <- as.integer(args$R %||% 500)
  epsilon <- as.numeric(args$epsilon %||% 0.05)
  M_half <- as.integer(args$M_half %||% 250)
  seed <- as.integer(args$seed %||% 123)

  generated <- generate_identifiability_split_tensor(
    Q1 = Q1,
    Q2 = Q2,
    R = R,
    K1 = K1,
    K2 = K2,
    K3 = K3,
    epsilon = epsilon,
    M_half = M_half,
    seed = seed
  )

  Y3 <- matrization_tensor(generated$truth$Y, 3)
  token_names <- paste0("token_", generated$truth$vocab)
  identifiability_truth <- generated$truth
  identifiability_meta <- generated$meta
  truth_d3_path <- ""
} else {
  Y3 <- read_numeric_matrix(y3_path)
  if (ncol(Y3) != Q1 * Q2) {
    stop(sprintf("Expected Q1*Q2=%d columns, got %d.", Q1 * Q2, ncol(Y3)))
  }

  token_names <- rownames(Y3)
  if (is.null(token_names) || any(!nzchar(token_names))) {
    token_names <- paste0("token_", seq_len(nrow(Y3)))
  }
}

column_totals <- colSums(Y3)
M_reference <- as.integer(stats::median(column_totals))
empirical_D3 <- normalize_cols(Y3)
R <- nrow(Y3)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

empirical_probability_tensor <- tensorization(empirical_D3, mode = 3, Q1 = Q1, Q2 = Q2, Q3 = R)
data <- if (run_identifiability) {
  list(Y = identifiability_truth$Y)
} else {
  list(D = empirical_probability_tensor)
}

if (run_identifiability) {
  cat("Running TTM-HOSVD identifiability example\n")
  cat(sprintf("epsilon=%g; M_half=%d\n", identifiability_meta$epsilon, identifiability_meta$M_half))
} else {
  cat("Running TTM-HOSVD on observed count data\n")
}
cat(sprintf("Y3 dimensions: %d tokens x %d tensor cells\n", R, ncol(Y3)))
cat(sprintf("Tensor dimensions: Q1=%d Q2=%d R=%d\n", Q1, Q2, R))
cat(sprintf("Column count totals: min=%g median=%g max=%g\n", min(column_totals), stats::median(column_totals), max(column_totals)))
cat(sprintf("Ranks: K1=%d K2=%d K3=%d; VH=%s\n", K1, K2, K3, vh_method))
cat(sprintf("Output directory: %s\n", output_dir))

fit <- fit_ttm_hosvd_method(
  data = data,
  K1 = K1,
  K2 = K2,
  K3 = K3,
  M = M_reference,
  vh_method = vh_method,
  return_diagnostics = TRUE
)

if (!is.null(fit$error)) {
  stop("TTM-HOSVD failed: ", fit$error)
}

fitted_tensor <- tensor_create(fit$core, fit$A1, fit$A2, fit$A3)
empirical_residual_l1 <- l1_error(fitted_tensor@data, empirical_probability_tensor@data) /
  sum(abs(empirical_probability_tensor@data))
fitted_D3 <- matrization_tensor(fitted_tensor, 3)
residual_D3 <- empirical_D3 - fitted_D3
core_mode3 <- matrization_tensor(fit$core, 3)

cat(sprintf("Elapsed seconds: %.3f\n", fit$time))
cat(sprintf("Estimated A1: %d x %d\n", nrow(fit$A1), ncol(fit$A1)))
cat(sprintf("Estimated A2: %d x %d\n", nrow(fit$A2), ncol(fit$A2)))
cat(sprintf("Estimated A3: %d x %d\n", nrow(fit$A3), ncol(fit$A3)))
cat(sprintf("Estimated core tensor: %s\n", paste(dim(fit$core), collapse = " x ")))
cat(sprintf("Relative L1 residual vs empirical distribution: %.6f\n", empirical_residual_l1))

write.csv(fit$A1, file.path(output_dir, "estimated_A1.csv"), row.names = FALSE)
write.csv(fit$A2, file.path(output_dir, "estimated_A2.csv"), row.names = FALSE)
write.csv(fit$A3, file.path(output_dir, "estimated_A3.csv"), row.names = token_names)
write.csv(core_mode3, file.path(output_dir, "estimated_core_mode3.csv"), row.names = FALSE)
write.csv(fitted_D3, file.path(output_dir, "fitted_D3.csv"), row.names = token_names)
write.csv(residual_D3, file.path(output_dir, "residual_D3.csv"), row.names = token_names)

if (run_identifiability) {
  identifiability_errors <- evaluate_method_result(fit, identifiability_truth)
  scalar_meta <- identifiability_meta[vapply(identifiability_meta, length, integer(1)) == 1]
  write.csv(identifiability_errors, file.path(output_dir, "identifiability_errors.csv"), row.names = FALSE)
  write.csv(
    data.frame(name = names(scalar_meta), value = unlist(scalar_meta), row.names = NULL),
    file.path(output_dir, "identifiability_metadata.csv"),
    row.names = FALSE
  )
  cat("Identifiability recovery errors:\n")
  print(identifiability_errors[, c("mode", "error")], row.names = FALSE)
}

mode_assignments <- data.frame(
  mode1_index = seq_len(nrow(fit$A1)),
  mode1_component = max.col(fit$A1),
  stringsAsFactors = FALSE
)
write.csv(mode_assignments, file.path(output_dir, "mode1_assignments.csv"), row.names = FALSE)

mode2_assignments <- data.frame(
  mode2_index = seq_len(nrow(fit$A2)),
  mode2_component = max.col(fit$A2),
  stringsAsFactors = FALSE
)
write.csv(mode2_assignments, file.path(output_dir, "mode2_assignments.csv"), row.names = FALSE)

top_tokens <- save_top_tokens_plot(
  fit$A3,
  token_names = token_names,
  top_n = top_n,
  path = file.path(output_dir, "top_tokens_by_topic.png")
)
write.csv(top_tokens, file.path(output_dir, "top_tokens_by_topic.csv"), row.names = FALSE)

plot_files <- c(
  save_heatmap(fit$A1, file.path(output_dir, "estimated_A1_heatmap.png"), "Estimated mode-1 factors", xlab = "Mode-1 component"),
  save_heatmap(fit$A2, file.path(output_dir, "estimated_A2_heatmap.png"), "Estimated mode-2 factors", xlab = "Mode-2 component"),
  save_heatmap(fit$A3, file.path(output_dir, "estimated_A3_heatmap.png"), "Estimated topic-token factors", xlab = "Topic component", ylab = "Token"),
  save_core_facets(fit$core, file.path(output_dir, "estimated_core_slices.png")),
  save_heatmap(fitted_D3, file.path(output_dir, "fitted_distribution_heatmap.png"), "Fitted token distribution", xlab = "Tensor cell", ylab = "Token"),
  save_heatmap(residual_D3, file.path(output_dir, "residual_heatmap.png"), "Empirical minus fitted distribution", xlab = "Tensor cell", ylab = "Token"),
  file.path(output_dir, "top_tokens_by_topic.png")
)

if (nzchar(truth_d3_path)) {
  truth_D3 <- read_numeric_matrix(truth_d3_path)
  if (!identical(dim(truth_D3), dim(empirical_D3))) {
    stop("D_truth must have the same dimensions as Y3.")
  }
  truth_tensor <- tensorization(truth_D3, mode = 3, Q1 = Q1, Q2 = Q2, Q3 = R)
  truth_relative_l1 <- l1_error(fitted_tensor@data, truth_tensor@data) / sum(abs(truth_tensor@data))
  cat(sprintf("Optional synthetic diagnostic: relative L1 vs D_truth = %.6f\n", truth_relative_l1))
}

cat("Saved tables and plots in:\n")
cat(sprintf("  %s\n", output_dir))
cat("Key plots:\n")
cat(paste0("  ", plot_files, collapse = "\n"), "\n")
