#!/usr/bin/env Rscript

# Fit TTM-HOSVD to the curated arXiv author-year-word corpus and save the
# estimated author factors, year factors, topic-word factors, core tensor, and
# README figures. The script can either build the corpus directly from the
# Kaggle arXiv metadata JSONL file or reuse CSV files prepared by
# real_data/arxiv_prepare_corpus.py.

suppressPackageStartupMessages({
  library(jsonlite)
  library(ggplot2)
  library(tm)
  library(wordcloud)
})

find_repo_root <- function(start = getwd()) {
  # Walk upward until the repository layout is found, so the script can be run
  # from the repo root or from a subdirectory.
  cur <- normalizePath(start, mustWork = TRUE)
  repeat {
    if (
      file.exists(file.path(cur, "methods", "methods_core.R")) &&
        dir.exists(file.path(cur, "real_data"))
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
  # Parse key=value command-line options without adding extra dependencies.
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
  # Interpret common command-line truthy values.
  tolower(as.character(x)) %in% c("1", "true", "t", "yes", "y")
}

normalize_text_key <- function(x) {
  # Create a lowercase ASCII key for author-name matching.
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

drop_middle_initials <- function(x) {
  # Remove single-letter initials before comparing author names.
  trimws(gsub("\\s+", " ", gsub("\\b[a-z]\\b", " ", x)))
}

first_last_key <- function(x) {
  # Use only the first and last token as a fallback author-name key.
  tokens <- unlist(strsplit(normalize_text_key(x), "\\s+"), use.names = FALSE)
  if (length(tokens) == 0) {
    return("")
  }
  paste(tokens[1], tokens[length(tokens)])
}

# Selected authors and reference cluster labels from the paper's arXiv example.
author_table <- data.frame(
  author = c(
    "Arora, Sanjeev",
    "Barber, Rina Foygel",
    "Belkin, Mikhail",
    "Bengio, Yoshua",
    "Candes, Emmanuel",
    "Fan, Jianqing",
    "Jordan, Michael I.",
    "Jurafsky, Dan",
    "LeCun, Yann",
    "Leskovec, Jure",
    "Levina, Elizaveta",
    "Ma, Tengyu",
    "Manning, Christopher D.",
    "Meinshausen, Nicolai",
    "Montanari, Andrea",
    "Smith, Noah A.",
    "Tibshirani, Ryan",
    "Wainwright, Martin",
    "Yu, Bin",
    "Zhu, Ji",
    "Zou, James"
  ),
  match_name = c(
    "Sanjeev Arora",
    "Rina Foygel Barber",
    "Mikhail Belkin",
    "Yoshua Bengio",
    "Emmanuel Candes",
    "Jianqing Fan",
    "Michael I Jordan",
    "Dan Jurafsky",
    "Yann LeCun",
    "Jure Leskovec",
    "Elizaveta Levina",
    "Tengyu Ma",
    "Christopher D Manning",
    "Nicolai Meinshausen",
    "Andrea Montanari",
    "Noah A Smith",
    "Ryan Tibshirani",
    "Martin Wainwright",
    "Bin Yu",
    "Ji Zhu",
    "James Zou"
  ),
  stringsAsFactors = FALSE
)
author_table$key <- normalize_text_key(author_table$match_name)
author_table$key_no_initials <- drop_middle_initials(author_table$key)
author_table$first_last_key <- vapply(author_table$match_name, first_last_key, character(1))
author_table$cluster_statistical_estimation <- author_table$author %in% c(
  "Barber, Rina Foygel", "Belkin, Mikhail", "Candes, Emmanuel",
  "Fan, Jianqing", "Jordan, Michael I.", "Levina, Elizaveta",
  "Meinshausen, Nicolai", "Montanari, Andrea", "Tibshirani, Ryan",
  "Wainwright, Martin", "Yu, Bin", "Zhu, Ji"
)
author_table$cluster_network_data <- author_table$author %in% c(
  "Jordan, Michael I.", "Leskovec, Jure", "Levina, Elizaveta",
  "Meinshausen, Nicolai", "Montanari, Andrea", "Yu, Bin",
  "Zhu, Ji", "Zou, James"
)
author_table$cluster_theoretical_ml <- author_table$author %in% c(
  "Arora, Sanjeev", "Belkin, Mikhail", "Jordan, Michael I.",
  "Ma, Tengyu", "Montanari, Andrea", "Wainwright, Martin"
)
author_table$cluster_nlp_neural_networks <- author_table$author %in% c(
  "Bengio, Yoshua", "Jurafsky, Dan", "LeCun, Yann",
  "Leskovec, Jure", "Manning, Christopher D.", "Smith, Noah A."
)
author_table$cluster_explainable_stats_ml_applications <- author_table$author %in% c(
  "Yu, Bin", "Zhu, Ji", "Zou, James"
)

extract_year <- function(paper) {
  # Prefer the first arXiv version date, then fall back to update_date.
  if (!is.null(paper$versions) && length(paper$versions) > 0) {
    created <- paper$versions[[1]]$created
    if (!is.null(created) && nzchar(created)) {
      year <- regmatches(created, regexpr("[12][0-9]{3}", created))
      if (length(year) == 1 && nzchar(year)) {
        return(as.integer(year))
      }
    }
  }
  if (!is.null(paper$update_date) && nzchar(paper$update_date)) {
    return(as.integer(substr(paper$update_date, 1, 4)))
  }
  NA_integer_
}

paper_author_keys <- function(authors_parsed) {
  # Convert arXiv's parsed author fields into normalized comparison keys.
  if (is.null(authors_parsed) || length(authors_parsed) == 0) {
    return(character())
  }
  vapply(authors_parsed, function(author) {
    last <- author[[1]] %||% ""
    first <- author[[2]] %||% ""
    suffix <- author[[3]] %||% ""
    normalize_text_key(paste(first, suffix, last))
  }, character(1))
}

match_selected_authors <- function(paper_keys, authors) {
  # Match exact normalized names, names without initials, and first/last names.
  paper_no_initials <- drop_middle_initials(paper_keys)
  paper_first_last <- vapply(paper_keys, first_last_key, character(1))
  matched <- authors$author[
    authors$key %in% paper_keys |
      authors$key_no_initials %in% paper_no_initials |
      authors$first_last_key %in% paper_first_last
  ]
  unique(matched)
}

find_metadata_path <- function(repo_root, requested_path = "") {
  # Try common local locations before asking the user to pass metadata=...
  candidates <- c(
    requested_path,
    file.path(repo_root, "real_data", "arxiv-metadata-oai-snapshot.json"),
    file.path(dirname(repo_root), "arxiv-metadata-oai-snapshot.json"),
    file.path(dirname(repo_root), "data", "arxiv-metadata-oai-snapshot.json"),
    file.path(dirname(repo_root), "data", "arxiv", "arxiv-metadata-oai-snapshot.json")
  )
  candidates <- candidates[nzchar(candidates)]
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) {
    stop(
      "Cannot find arXiv metadata JSON. Pass metadata=/path/to/arxiv-metadata-oai-snapshot.json ",
      "from https://www.kaggle.com/datasets/Cornell-University/arxiv."
    )
  }
  normalizePath(existing[1], mustWork = TRUE)
}

collect_author_papers <- function(metadata_path, authors, start_year, end_year, max_records = Inf) {
  # Stream the large JSONL file and keep only records that match selected authors.
  con <- file(metadata_path, open = "r")
  on.exit(close(con), add = TRUE)

  rows <- list()
  n_rows <- 0L
  n_seen <- 0L
  repeat {
    line <- readLines(con, n = 1, warn = FALSE)
    if (length(line) == 0) {
      break
    }
    n_seen <- n_seen + 1L
    if (is.finite(max_records) && n_seen > max_records) {
      break
    }
    if (n_seen %% 100000 == 0) {
      message("Scanned ", n_seen, " records; matched ", n_rows, " author-paper rows")
    }

    paper <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(paper)) {
      next
    }
    year <- extract_year(paper)
    if (is.na(year) || year < start_year || year > end_year) {
      next
    }

    keys <- paper_author_keys(paper$authors_parsed)
    matched <- match_selected_authors(keys, authors)
    if (length(matched) == 0) {
      next
    }

    categories <- paper$categories %||% ""
    for (author in matched) {
      n_rows <- n_rows + 1L
      rows[[n_rows]] <- data.frame(
        selected_author = author,
        id = paper$id %||% "",
        year = year,
        title = gsub("\\s+", " ", paper$title %||% ""),
        abstract = gsub("\\s+", " ", paper$abstract %||% ""),
        categories = categories,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    stop("No papers matched the selected author list in the requested year range.")
  }
  do.call(rbind, rows)
}

sample_author_year_papers <- function(papers, authors, years, per_year, seed) {
  # Sample up to per_year papers per author-year; use a +/- 2 year window when
  # an exact year has too few papers.
  set.seed(seed)
  selected <- list()
  used_ids <- character()
  n_selected <- 0L

  for (author in authors$author) {
    author_papers <- papers[papers$selected_author == author, , drop = FALSE]
    for (year in years) {
      exact_year <- author_papers[
        author_papers$year == year & !author_papers$id %in% used_ids,
        ,
        drop = FALSE
      ]
      chosen <- exact_year[0, , drop = FALSE]
      if (nrow(exact_year) > 0) {
        n_take <- min(per_year, nrow(exact_year))
        chosen <- exact_year[sample(seq_len(nrow(exact_year)), n_take), , drop = FALSE]
      }

      if (nrow(chosen) < per_year) {
        window_years <- (year - 2):(year + 2)
        window_papers <- author_papers[
          author_papers$year %in% window_years &
            author_papers$year != year &
            !author_papers$id %in% c(used_ids, chosen$id),
          ,
          drop = FALSE
        ]
        if (nrow(window_papers) > 0) {
          n_extra <- min(per_year - nrow(chosen), nrow(window_papers))
          extra <- window_papers[sample(seq_len(nrow(window_papers)), n_extra), , drop = FALSE]
          chosen <- rbind(chosen, extra)
        }
      }

      if (nrow(chosen) == 0) {
        next
      }

      chosen$target_year <- year
      chosen$sample_window <- ifelse(chosen$year == year, "same_year", "within_2_years")
      used_ids <- union(used_ids, chosen$id)

      n_selected <- n_selected + 1L
      selected[[n_selected]] <- chosen
    }
  }

  if (length(selected) == 0) {
    stop("Sampling produced no papers.")
  }
  do.call(rbind, selected)
}

tokenize_abstract <- function(text) {
  # Tokenize abstracts with simple lowercasing, punctuation removal, and stopwords.
  text <- iconv(text, from = "", to = "ASCII//TRANSLIT")
  text <- tolower(text)
  text <- gsub("https?://\\S+", " ", text)
  text <- gsub("[^a-z ]+", " ", text)
  tokens <- unlist(strsplit(text, "\\s+"), use.names = FALSE)
  tokens <- tokens[nchar(tokens) >= 3 & nchar(tokens) <= 25]
  stop_words <- unique(c(
    tm::stopwords("en"),
    "using", "use", "used", "paper", "papers", "study", "studies",
    "result", "results", "show", "shows", "based", "propose", "proposed",
    "provide", "new", "also", "one", "two", "via", "approach"
  ))
  tokens[!tokens %in% stop_words]
}

build_count_tensor <- function(selected_papers, authors, years, max_vocab, min_total_count) {
  # Build a vocabulary-by-author-year count matrix used as mode-3 matricization.
  cells <- expand.grid(author = authors$author, year = years, stringsAsFactors = FALSE)
  cells$cell <- paste(cells$author, cells$year, sep = "__")

  token_by_cell <- vector("list", nrow(cells))
  names(token_by_cell) <- cells$cell
  global_counts <- integer()

  for (i in seq_len(nrow(selected_papers))) {
    cell <- paste(selected_papers$selected_author[i], selected_papers$target_year[i], sep = "__")
    tokens <- tokenize_abstract(selected_papers$abstract[i])
    if (length(tokens) == 0) {
      next
    }
    tab <- table(tokens)
    token_by_cell[[cell]] <- c(token_by_cell[[cell]], tokens)
    current_counts <- global_counts[names(tab)]
    current_counts[is.na(current_counts)] <- 0L
    global_counts[names(tab)] <- current_counts + as.integer(tab)
  }
  global_counts[is.na(global_counts)] <- 0L
  global_counts <- sort(global_counts, decreasing = TRUE)
  vocab <- names(global_counts[global_counts >= min_total_count])
  vocab <- head(vocab, max_vocab)
  if (length(vocab) == 0) {
    stop("Vocabulary is empty; lower min_total_count or check the metadata file.")
  }

  Y3 <- matrix(0, nrow = length(vocab), ncol = nrow(cells), dimnames = list(vocab, cells$cell))
  for (cell in names(token_by_cell)) {
    tokens <- token_by_cell[[cell]]
    if (length(tokens) == 0) {
      next
    }
    tab <- table(tokens[tokens %in% vocab])
    Y3[names(tab), cell] <- as.integer(tab)
  }

  empty_cells <- colSums(Y3) == 0
  if (any(empty_cells)) {
    Y3[, empty_cells] <- 1L
  }

  list(Y3 = Y3, cells = cells, vocabulary = data.frame(token = vocab, count = as.integer(global_counts[vocab])), empty_cells = cells[empty_cells, ])
}

normalize_cols <- function(mat) {
  # Convert each author-year word-count column into an empirical distribution.
  totals <- colSums(mat)
  totals[totals <= 0] <- 1
  sweep(mat, 2, totals, "/")
}

save_heatmap <- function(mat, path, title, xlab, ylab) {
  # Save a compact heatmap for the estimated factor matrices.
  df <- expand.grid(row = seq_len(nrow(mat)), col = seq_len(ncol(mat)))
  df$value <- as.vector(mat)
  df$row_label <- rownames(mat)[df$row]
  p <- ggplot(df, aes(x = factor(col), y = factor(row, levels = rev(seq_len(nrow(mat)))), fill = value)) +
    geom_tile() +
    scale_y_discrete(labels = rev(rownames(mat))) +
    scale_fill_viridis_c(option = "H") +
    labs(title = title, x = xlab, y = ylab, fill = "") +
    theme_minimal(base_size = 10) +
    theme(axis.text.y = element_text(size = 6))
  ggsave(path, p, width = 8, height = max(4, min(12, nrow(mat) * 0.22)), dpi = 180)
  invisible(path)
}

top_words_by_topic <- function(A3, top_n = 25) {
  # Extract the highest-weight vocabulary terms for each estimated topic.
  rows <- lapply(seq_len(ncol(A3)), function(topic) {
    ord <- order(A3[, topic], decreasing = TRUE)
    ord <- ord[seq_len(min(top_n, length(ord)))]
    data.frame(
      topic = paste0("Topic ", topic),
      word = rownames(A3)[ord],
      weight = A3[ord, topic],
      rank = seq_along(ord),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

save_topic_wordclouds <- function(A3, path, top_n = 80) {
  # Save one word cloud per topic using the estimated topic-word factor matrix.
  png(path, width = 1800, height = 480, res = 160)
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    dev.off()
  }, add = TRUE)
  par(mfrow = c(1, ncol(A3)), mar = c(0, 0, 3, 0))
  colors <- c("#234f9c", "#8c6d1f", "#c43b6a", "#2c8c5a", "#8a4f1d", "#222222")
  for (topic in seq_len(ncol(A3))) {
    ord <- order(A3[, topic], decreasing = TRUE)
    ord <- ord[seq_len(min(top_n, length(ord)))]
    wordcloud(
      words = rownames(A3)[ord],
      freq = A3[ord, topic],
      random.order = FALSE,
      rot.per = 0.08,
      colors = colors,
      scale = c(3.2, 0.45),
      max.words = length(ord)
    )
    title(paste("Topic", topic), line = 0.5)
  }
  invisible(path)
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
repo_root <- find_repo_root()
source(file.path(repo_root, "methods", "methods_core.R"))
ensure_benchmark_sources()
source(file.path(repo_root, "methods", "algorithms", "ttm_hosvd.R"))

svd_backend <- args$svd_backend %||% "rsvd"
if (identical(svd_backend, "rsvd")) {
  if (!requireNamespace("rsvd", quietly = TRUE)) {
    stop("svd_backend=rsvd requires the R package 'rsvd'.")
  }
  svds <- function(A, k) {
    fit <- rsvd::rsvd(A, k = k)
    list(u = fit$u, d = fit$d, v = fit$v)
  }
} else if (identical(svd_backend, "base")) {
  svds <- function(A, k) {
    k_eff <- min(as.integer(k), nrow(A), ncol(A))
    fit <- svd(A, nu = k_eff, nv = k_eff)
    list(u = fit$u[, seq_len(k_eff), drop = FALSE], d = fit$d[seq_len(k_eff)], v = fit$v[, seq_len(k_eff), drop = FALSE])
  }
}

prepared_dir <- args$prepared_dir %||% ""
metadata_path <- if (nzchar(prepared_dir)) "" else find_metadata_path(repo_root, args$metadata %||% "")
output_dir <- args$output_dir %||% file.path(repo_root, "results", "arxiv_ttm_hosvd")
start_year <- as.integer(args$start_year %||% 2005)
end_year <- as.integer(args$end_year %||% 2024)
per_year <- as.integer(args$per_year %||% 3)
K1 <- as.integer(args$K1 %||% 5)
K2 <- as.integer(args$K2 %||% 3)
K3 <- as.integer(args$K3 %||% 5)
max_vocab <- as.integer(args$max_vocab %||% 5000)
min_total_count <- as.integer(args$min_total_count %||% 5)
top_n <- as.integer(args$top_n %||% 25)
seed <- as.integer(args$seed %||% 1234)
vh_method <- as.character(args$vh_method %||% "SP")
run_model <- as_bool(args$run_model %||% "true")
max_records <- as.numeric(args$max_records %||% Inf)

years <- seq.int(start_year, end_year)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (nzchar(prepared_dir)) {
  prepared_dir <- normalizePath(prepared_dir, mustWork = TRUE)
  cat("Loading prepared arXiv corpus\n")
  cat(sprintf("Prepared directory: %s\n", prepared_dir))
  author_table <- read.csv(file.path(prepared_dir, "arxiv_authors.csv"), check.names = FALSE, stringsAsFactors = FALSE)
  cells <- read.csv(file.path(prepared_dir, "tensor_cells.csv"), check.names = FALSE, stringsAsFactors = FALSE)
  years <- sort(unique(as.integer(cells$year)))
  Y3 <- as.matrix(read.csv(file.path(prepared_dir, "Y3_author_year_word_counts.csv"), row.names = 1, check.names = FALSE))
  storage.mode(Y3) <- "numeric"
  counts <- list(
    Y3 = Y3,
    cells = cells,
    vocabulary = read.csv(file.path(prepared_dir, "vocabulary.csv"), check.names = FALSE, stringsAsFactors = FALSE),
    empty_cells = read.csv(file.path(prepared_dir, "empty_cells.csv"), check.names = FALSE, stringsAsFactors = FALSE)
  )
  selected_papers <- read.csv(file.path(prepared_dir, "selected_author_year_papers.csv"), check.names = FALSE, stringsAsFactors = FALSE)
} else {
  cat("Building curated arXiv corpus\n")
  cat(sprintf("Metadata: %s\n", metadata_path))
  cat(sprintf("Authors: %d; years: %d-%d; papers per author-year: up to %d\n", nrow(author_table), start_year, end_year, per_year))

  matched_papers <- collect_author_papers(metadata_path, author_table, start_year, end_year, max_records)
  selected_papers <- sample_author_year_papers(matched_papers, author_table, years, per_year, seed)
  counts <- build_count_tensor(selected_papers, author_table, years, max_vocab, min_total_count)

  write.csv(author_table, file.path(output_dir, "arxiv_authors.csv"), row.names = FALSE)
  write.csv(matched_papers, file.path(output_dir, "matched_author_papers.csv"), row.names = FALSE)
  write.csv(selected_papers, file.path(output_dir, "selected_author_year_papers.csv"), row.names = FALSE)
  write.csv(counts$cells, file.path(output_dir, "tensor_cells.csv"), row.names = FALSE)
  write.csv(counts$vocabulary, file.path(output_dir, "vocabulary.csv"), row.names = FALSE)
  write.csv(counts$empty_cells, file.path(output_dir, "empty_cells_filled_uniformly.csv"), row.names = FALSE)
  write.csv(counts$Y3, file.path(output_dir, "Y3_author_year_word_counts.csv"))
}

cat(sprintf("Selected %d author-paper rows across %d unique arXiv records\n", nrow(selected_papers), length(unique(selected_papers$id))))
cat(sprintf("Tensor dimensions: authors=%d years=%d vocabulary=%d\n", nrow(author_table), length(years), nrow(counts$Y3)))

if (!run_model) {
  cat("run_model=false, stopping after data construction.\n")
  quit(save = "no", status = 0)
}

D3 <- normalize_cols(counts$Y3)
data <- list(D = tensorization(D3, mode = 3, Q1 = nrow(author_table), Q2 = length(years), Q3 = nrow(D3)))
M_reference <- as.integer(stats::median(colSums(counts$Y3)))

cat("Running TTM-HOSVD\n")
fit <- fit_ttm_hosvd_method(
  data = data,
  K1 = K1,
  K2 = K2,
  K3 = K3,
  M = M_reference,
  seed = seed,
  vh_method = vh_method,
  return_diagnostics = TRUE
)
if (!is.null(fit$error)) {
  stop("TTM-HOSVD failed: ", fit$error)
}

rownames(fit$A1) <- author_table$author
rownames(fit$A2) <- as.character(years)
rownames(fit$A3) <- rownames(counts$Y3)
colnames(fit$A1) <- paste0("Author cluster ", seq_len(ncol(fit$A1)))
colnames(fit$A2) <- paste0("Year cluster ", seq_len(ncol(fit$A2)))
colnames(fit$A3) <- paste0("Topic ", seq_len(ncol(fit$A3)))

top_words <- top_words_by_topic(fit$A3, top_n = top_n)
core_mode3 <- matrization_tensor(fit$core, 3)

write.csv(fit$A1, file.path(output_dir, "estimated_author_factors_A1.csv"))
write.csv(fit$A2, file.path(output_dir, "estimated_year_factors_A2.csv"))
write.csv(fit$A3, file.path(output_dir, "estimated_topic_word_factors_A3.csv"))
write.csv(core_mode3, file.path(output_dir, "estimated_core_mode3.csv"), row.names = FALSE)
write.csv(top_words, file.path(output_dir, "top_words_by_topic.csv"), row.names = FALSE)

save_heatmap(fit$A1, file.path(output_dir, "author_factor_heatmap.png"), "Estimated author factors", "Author cluster", "Author")
save_heatmap(fit$A2, file.path(output_dir, "year_factor_heatmap.png"), "Estimated year factors", "Year cluster", "Year")
save_topic_wordclouds(fit$A3, file.path(output_dir, "topic_wordclouds.png"), top_n = 80)

cat(sprintf("Elapsed seconds: %.3f\n", fit$time))
cat("Saved arXiv TTM-HOSVD outputs in:\n")
cat(sprintf("  %s\n", output_dir))
cat("Key files:\n")
cat(sprintf("  %s\n", file.path(output_dir, "top_words_by_topic.csv")))
cat(sprintf("  %s\n", file.path(output_dir, "topic_wordclouds.png")))
cat(sprintf("  %s\n", file.path(output_dir, "author_factor_heatmap.png")))
