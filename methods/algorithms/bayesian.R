# Method: Tensor-LDA (Bayesian) Baseline
# Fits the tensor topic model in Stan; when rstan/model file is unavailable,
# it falls back to the Hybrid-LDA path for operational robustness.
get_tensor_lda_stan_model <- local({
  cache <- NULL
  cache_path <- NULL
  function(stan_file) {
    if (is.null(cache) || !identical(cache_path, stan_file)) {
      cache <<- rstan::stan_model(file = stan_file, model_name = "tensor_lda")
      cache_path <<- stan_file
    }
    cache
  }
})

fit_bayesian_method <- function(
  data,
  K1,
  K2,
  K3,
  M,
  seed = 1234,
  ...
) {
  ensure_benchmark_sources()
  dots <- list(...)
  use_vb <- if (!is.null(dots$use_vb)) as.logical(dots$use_vb) else TRUE
  vb_iter <- if (!is.null(dots$vb_iter)) as.integer(dots$vb_iter) else 5000L
  vb_output_samples <- if (!is.null(dots$vb_output_samples)) as.integer(dots$vb_output_samples) else 500L

  if (!requireNamespace("rstan", quietly = TRUE)) {
    warning("rstan is unavailable; falling back to LDA mapping for bayesian method", call. = FALSE)
    out <- fit_lda_method(data, K1, K2, K3, M, seed = seed, method_label = "bayesian")
    out$meta <- c(out$meta, list(fallback = "LDA (rstan missing)"))
    return(out)
  }

  repo_root <- find_ttm_repo_root()
  methods_dir <- resolve_methods_dir(repo_root)
  stan_file <- file.path(methods_dir, "algorithms", "lda_model.stan")
  if (!file.exists(stan_file)) {
    warning("lda_model.stan is missing; falling back to LDA mapping for bayesian method", call. = FALSE)
    out <- fit_lda_method(data, K1, K2, K3, M, seed = seed, method_label = "bayesian")
    out$meta <- c(out$meta, list(fallback = "LDA (stan model missing)"))
    return(out)
  }

  dims <- get_tensor_dims(data)
  Y3 <- extract_y3_counts(data, M)

  reviewer_ids <- rep(seq_len(dims$Q1), each = dims$Q2)
  paper_ids <- rep(seq_len(dims$Q2), times = dims$Q1)

  data_list <- list(
    N1 = dims$Q1,
    N2 = dims$Q2,
    V = dims$R,
    K = K3,
    K1 = K1,
    K2 = K2,
    D = dims$Q1 * dims$Q2,
    X = t(Y3),
    reviewer_ids = reviewer_ids,
    paper_ids = paper_ids
  )

  elapsed <- system.time({
    fit <- tryCatch({
      set.seed(seed)
      model <- get_tensor_lda_stan_model(stan_file)
      if (isTRUE(use_vb)) {
        rstan::vb(
          object = model,
          data = data_list,
          iter = vb_iter,
          output_samples = vb_output_samples,
          seed = seed,
          refresh = 0
        )
      } else {
        rstan::sampling(
          object = model,
          data = data_list,
          iter = 2000,
          chains = 4,
          seed = seed,
          refresh = 0
        )
      }
    }, error = function(e) e)
  })["elapsed"]

  if (inherits(fit, "error")) {
    return(method_result("bayesian", time = elapsed, error = conditionMessage(fit)))
  }

  pars <- rstan::extract(fit, permuted = TRUE)

  reviewer_type_mean <- apply(pars$reviewer_type, c(2, 3), mean)
  paper_category_mean <- apply(pars$paper_category, c(2, 3), mean)
  phi_mean <- t(apply(pars$phi, c(2, 3), mean))
  core_mean <- apply(pars$core, c(2, 3), mean)
  core_mean <- matrix(core_mean, nrow = K1 * K2, ncol = K3)

  core_arr <- array(0, dim = c(K1, K2, K3))
  for (k1 in seq_len(K1)) {
    for (k2 in seq_len(K2)) {
      core_arr[k1, k2, ] <- core_mean[(k1 - 1) * K2 + k2, ]
    }
  }

  method_result(
    "bayesian",
    A1 = normalize_rows_safe(reviewer_type_mean),
    A2 = normalize_rows_safe(paper_category_mean),
    A3 = normalize_cols_safe(phi_mean),
    core = as.tensor(core_arr),
    time = elapsed,
    meta = list(mapping = "Tensor-LDA (Bayesian)", use_vb = use_vb)
  )
}
