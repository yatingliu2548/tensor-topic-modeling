# Method: Hybrid-LDA Baseline
# Fits LDA on mode-3 documents, then recovers A1/A2 with spectral clustering
# on document-topic structure, and estimates the core via regression.
fit_lda_method <- function(
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
  max_iter <- if (!is.null(dots$max_iter)) as.integer(dots$max_iter) else 1000L
  method_label <- if (!is.null(dots$method_label)) as.character(dots$method_label) else "LDA"

  if (!requireNamespace("topicmodels", quietly = TRUE)) {
    return(method_result(method_label, time = NA_real_, error = "Package 'topicmodels' is required"))
  }

  dims <- get_tensor_dims(data)
  Y3 <- extract_y3_counts(data, M)
  lda_control <- list(
    seed = seed,
    verbose = 0L,
    em = list(iter.max = max_iter, tol = 1e-4)
  )

  elapsed <- system.time({
    fit <- tryCatch(
      topicmodels::LDA(
        t(Y3),
        k = K3,
        control = lda_control,
        method = "VEM"
      ),
      error = function(e) e
    )
  })["elapsed"]

  if (inherits(fit, "error")) {
    return(method_result(method_label, time = elapsed, error = conditionMessage(fit)))
  }

  A3 <- exp(t(fit@beta))
  A3 <- normalize_cols_safe(A3)

  W_doc_topic <- fit@gamma
  parts <- build_a12_core_from_doc_topics(W_doc_topic, dims$Q1, dims$Q2, K1, K2)

  method_result(
    method = method_label,
    A1 = parts$A1,
    A2 = parts$A2,
    A3 = A3,
    core = parts$core,
    time = elapsed,
    meta = list(mapping = "Hybrid-LDA")
  )
}

fit_hybrid_lda_method <- fit_lda_method
