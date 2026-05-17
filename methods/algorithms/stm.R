# Method: STM Baseline
# Fits Structural Topic Model with reviewer/paper covariates.
# If STM is unavailable or fitting fails, it returns no estimate.
fit_stm_method <- function(
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
  max_em_its <- if (!is.null(dots$max_em_its)) as.integer(dots$max_em_its) else 50L

  if (!requireNamespace("stm", quietly = TRUE) || !requireNamespace("Matrix", quietly = TRUE)) {
    return(method_result("STM", time = NA_real_, error = "STM dependencies not available"))
  }

  dims <- get_tensor_dims(data)
  Y3 <- extract_y3_counts(data, M)

  docs <- Matrix::Matrix(as.matrix(t(Y3)), sparse = TRUE)
  colnames(docs) <- as.character(seq_len(dims$R))
  meta <- data.frame(
    reviewer = factor(rep(seq_len(dims$Q1), each = dims$Q2)),
    paper = factor(rep(seq_len(dims$Q2), times = dims$Q1))
  )

  elapsed <- system.time({
    fit <- tryCatch(
      stm::stm(
        documents = docs,
        K = K3,
        prevalence = ~ reviewer + paper,
        data = meta,
        init.type = "Spectral",
        max.em.its = max_em_its,
        seed = seed
      ),
      error = function(e) e
    )
  })["elapsed"]

  if (inherits(fit, "error")) {
    return(method_result("STM", time = elapsed, error = conditionMessage(fit)))
  }

  A3 <- t(exp(fit$beta$logbeta[[1]]))
  A3 <- normalize_cols_safe(A3)

  W_doc_topic <- fit$theta
  parts <- build_a12_core_from_doc_topics(W_doc_topic, dims$Q1, dims$Q2, K1, K2)

  method_result(
    method = "STM",
    A1 = parts$A1,
    A2 = parts$A2,
    A3 = A3,
    core = parts$core,
    time = elapsed
  )
}
