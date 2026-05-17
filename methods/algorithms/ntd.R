# Method: NTD Baseline
# Runs nonnegative Tucker decomposition and post-processes factors/core to
# enforce nonnegativity and simplex-style normalization.
fit_ntd_method <- function(
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
  num_iter <- if (!is.null(dots$num_iter)) as.integer(dots$num_iter) else 10L
  algorithm <- if (!is.null(dots$algorithm)) as.character(dots$algorithm) else "KL"
  init <- if (!is.null(dots$init)) as.character(dots$init) else "NMF"

  if (!requireNamespace("nnTensor", quietly = TRUE)) {
    return(method_result("NTD", time = NA_real_, error = "Package 'nnTensor' is required"))
  }

  empirical_D <- extract_empirical_d_tensor(data, M)

  elapsed <- system.time({
    fit <- tryCatch(
      nnTensor::NTD(empirical_D, rank = c(K1, K2, K3), algorithm = algorithm, init = init, num.iter = num_iter),
      error = function(e) e
    )
  })["elapsed"]

  if (inherits(fit, "error")) {
    return(method_result("NTD", time = elapsed, error = conditionMessage(fit)))
  }

  A1 <- t(fit$A$A1)
  A1 <- pmax(A1, 0)
  A1 <- normalize_rows_safe(A1)

  A2 <- t(fit$A$A2)
  A2 <- pmax(A2, 0)
  A2 <- normalize_rows_safe(A2)

  A3 <- t(fit$A$A3)
  A3 <- pmax(A3, 0)
  A3 <- normalize_cols_safe(A3)

  core <- fit$S
  G3 <- matrization_tensor(core, 3)
  G3 <- pmax(G3, 0)
  denom <- colSums(G3)
  denom[denom == 0] <- 1
  G3 <- t(apply(G3, 1, function(x) x / denom))
  G3[is.na(G3)] <- 0
  core <- tensorization(G3, 3, dim(core)[1], dim(core)[2], dim(core)[3])

  method_result("NTD", A1 = A1, A2 = A2, A3 = A3, core = core, time = elapsed)
}
