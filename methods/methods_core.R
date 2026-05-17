METHODS_CORE_FILE <- tryCatch(normalizePath(sys.frame(1)$ofile, mustWork = TRUE), error = function(e) "")

find_ttm_repo_root <- function(start = getwd()) {
  cur <- normalizePath(start, mustWork = TRUE)
  repeat {
    has_methods_dir <- dir.exists(file.path(cur, "methods")) ||
      dir.exists(file.path(cur, "benchmarks", "methods"))
    if (
      file.exists(file.path(cur, "methods", "score_based_methods.R")) &&
      dir.exists(file.path(cur, "methods", "vh_methods")) &&
      has_methods_dir
    ) {
      return(cur)
    }
    parent <- dirname(cur)
    if (identical(parent, cur)) {
      stop("Cannot find repository root containing methods/score_based_methods.R")
    }
    cur <- parent
  }
}

resolve_methods_dir <- function(repo_root) {
  modern_path <- file.path(repo_root, "methods")
  legacy_path <- file.path(repo_root, "benchmarks", "methods")
  if (dir.exists(modern_path)) {
    return(modern_path)
  }
  if (dir.exists(legacy_path)) {
    return(legacy_path)
  }
  stop("Cannot find methods directory under repo root")
}

ensure_benchmark_sources <- local({
  initialized <- FALSE
  function() {
    if (initialized) {
      return(invisible(TRUE))
    }
    repo_root <- find_ttm_repo_root()
    methods_dir <- resolve_methods_dir(repo_root)
    source(file.path(methods_dir, "vh_methods", "load_vh_methods.R"), local = globalenv())
    source(file.path(methods_dir, "score_based_methods.R"), local = globalenv())
    source(file.path(methods_dir, "data_generation.R"), local = globalenv())
    initialized <<- TRUE
    invisible(TRUE)
  }
})

normalize_rows_safe <- function(mat) {
  rs <- rowSums(mat)
  rs[rs == 0] <- 1
  sweep(mat, 1, rs, "/")
}

normalize_cols_safe <- function(mat) {
  cs <- colSums(mat)
  cs[cs == 0] <- 1
  sweep(mat, 2, cs, "/")
}

get_tensor_dims <- function(data) {
  if (!is.null(data$Y)) {
    d <- dim(data$Y@data)
  } else if (!is.null(data$D)) {
    d <- dim(data$D@data)
  } else {
    stop("data must contain Y or D as rTensor tensor")
  }
  list(Q1 = d[1], Q2 = d[2], R = d[3])
}

extract_y3_counts <- function(data, M) {
  if (!is.null(data$Y)) {
    return(matrization_tensor(data$Y, 3))
  }
  if (!is.null(data$D)) {
    return(round(pmax(matrization_tensor(data$D, 3) * M, 0)))
  }
  stop("data must contain Y or D")
}

extract_empirical_d_tensor <- function(data, M) {
  if (!is.null(data$Y)) {
    return(data$Y / M)
  }
  if (!is.null(data$D)) {
    return(data$D)
  }
  stop("data must contain Y or D")
}

build_a12_core_from_doc_topics <- function(W_doc_topic, Q1, Q2, K1, K2) {
  K3 <- ncol(W_doc_topic)
  lda_W <- t(W_doc_topic)

  W1 <- matrization_tensor(tensorization(lda_W, 3, Q1, Q2, K3), 1)
  A1 <- spectral_clustering(W1 %*% t(W1), K = K1, mix = TRUE)

  W2 <- matrization_tensor(tensorization(lda_W, 3, Q1, Q2, K3), 2)
  A2 <- spectral_clustering(W2 %*% t(W2), K = K2, mix = TRUE)

  G_mat <- compute_G_from_WA(kronecker(A1, A2), W_doc_topic)
  core <- tensorization(t(G_mat), 3, K1, K2, ncol(G_mat))

  list(A1 = A1, A2 = A2, core = core)
}

method_result <- function(method, A1 = NULL, A2 = NULL, A3 = NULL, core = NULL, time = NA_real_, error = NULL, meta = NULL) {
  list(
    method = method,
    A1 = A1,
    A2 = A2,
    A3 = A3,
    core = core,
    time = as.numeric(time),
    error = error,
    meta = meta
  )
}

compute_core_l1_error <- function(hat_core, true_core, perm1, perm2, perm3) {
  hatG3 <- matrization_tensor(hat_core, 3)
  G3 <- matrization_tensor(true_core, 3)
  G3_perm <- t(perm3) %*% G3 %*% kronecker(perm1, perm2)
  l1_error(G3_perm, hatG3)
}

evaluate_method_result <- function(result, truth) {
  rows <- data.frame(
    method = result$method,
    mode = c("A1", "A2", "A3", "core"),
    error = NA_real_,
    time = as.numeric(result$time),
    error_msg = if (is.null(result$error)) NA_character_ else as.character(result$error),
    stringsAsFactors = FALSE
  )

  if (is.null(result$A1) || is.null(result$A2) || is.null(result$A3) || is.null(result$core)) {
    return(rows)
  }

  p1 <- matrix_lp_distance(result$A1, truth$A1, lp = 1)
  p2 <- matrix_lp_distance(result$A2, truth$A2, lp = 1)
  p3 <- matrix_lp_distance(result$A3, truth$A3, lp = 1)
  core_err <- compute_core_l1_error(result$core, truth$G, p1$permutation, p2$permutation, p3$permutation)

  rows$error <- c(p1$error, p2$error, p3$error, core_err)
  rows$time <- result$time
  rows$error_msg <- if (is.null(result$error)) NA_character_ else as.character(result$error)
  rows
}

ensure_method_registry <- local({
  initialized <- FALSE
  function() {
    if (initialized) {
      return(invisible(TRUE))
    }

    if (nzchar(METHODS_CORE_FILE)) {
      methods_dir <- dirname(METHODS_CORE_FILE)
    } else {
      repo_root <- find_ttm_repo_root()
      methods_dir <- resolve_methods_dir(repo_root)
    }

    source(file.path(methods_dir, "algorithms", "lda.R"), local = globalenv())
    source(file.path(methods_dir, "algorithms", "stm.R"), local = globalenv())
    source(file.path(methods_dir, "algorithms", "ntd.R"), local = globalenv())
    source(file.path(methods_dir, "algorithms", "topicscore_hosvd.R"), local = globalenv())
    source(file.path(methods_dir, "algorithms", "bayesian.R"), local = globalenv())
    source(file.path(methods_dir, "algorithms", "ttm_hosvd.R"), local = globalenv())
    source(file.path(methods_dir, "algorithms", "ttm_hooi.R"), local = globalenv())
    source(file.path(methods_dir, "algorithms", "ttm_method.R"), local = globalenv())

    initialized <<- TRUE
    invisible(TRUE)
  }
})

get_method_registry <- function() {
  ensure_method_registry()
  list(
    "bayesian" = fit_bayesian_method,
    "LDA" = fit_lda_method,
    "STM" = fit_stm_method,
    "NTD" = fit_ntd_method,
    "TopicScore-HOSVD" = fit_topicscore_hosvd_method,
    "TTM-HOSVD" = fit_ttm_hosvd_method,
    "TTM-HOOI" = fit_ttm_hooi_method,
    "tensor_lda" = fit_bayesian_method,
    "hybrid_lda" = fit_lda_method
  )
}

run_benchmark_method <- function(method, data, K1, K2, K3, M, ...) {
  registry <- get_method_registry()
  if (!method %in% names(registry)) {
    stop(sprintf("Unknown method: %s", method))
  }
  registry[[method]](data = data, K1 = K1, K2 = K2, K3 = K3, M = M, ...)
}
