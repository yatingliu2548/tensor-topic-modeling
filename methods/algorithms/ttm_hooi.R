# Method: TTM-HOOI (Our Method)
# Uses HOOI factor updates inside the same TTM pipeline.
fit_ttm_hooi_method <- function(
  data,
  K1,
  K2,
  K3,
  M,
  seed = 1234,
  vh_method = "SP",
  K0 = NULL,
  m = NULL,
  threshold = FALSE,
  return_diagnostics = FALSE,
  ...
) {
  ensure_benchmark_sources()

  empirical_D <- extract_empirical_d_tensor(data, M)

  elapsed <- system.time({
    set.seed(seed)
    fit <- tryCatch(
      score(
        empirical_D,
        K1 = K1,
        K2 = K2,
        K3 = K3,
        M = M,
        normalize = "HOOI",
        VHMethod = vh_method,
        K0 = K0,
        m = m,
        threshold = threshold,
        as.sparse = FALSE,
        return_diagnostics = return_diagnostics
      ),
      error = function(e) e
    )
  })["elapsed"]

  if (inherits(fit, "error")) {
    return(method_result("TTM-HOOI", time = elapsed, error = conditionMessage(fit)))
  }

  method_result(
    "TTM-HOOI",
    A1 = fit$hatA1,
    A2 = fit$hatA2,
    A3 = fit$hatA3,
    core = fit$hatcore,
    time = elapsed,
    meta = list(vh_method = vh_method, diagnostics = fit$diagnostics)
  )
}
