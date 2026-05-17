# Method Dispatcher: TTM Family
# Convenience wrapper that routes to TTM-HOSVD, TTM-HOOI, or TopicScore-HOSVD.
fit_ttm_method <- function(
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
  variant = c("TTM-HOSVD", "TTM-HOOI", "TopicScore-HOSVD"),
  ...
) {
  variant <- match.arg(variant)

  if (variant == "TTM-HOSVD") {
    return(fit_ttm_hosvd_method(data, K1, K2, K3, M, seed = seed, vh_method = vh_method, K0 = K0, m = m, threshold = threshold, return_diagnostics = return_diagnostics))
  }
  if (variant == "TTM-HOOI") {
    return(fit_ttm_hooi_method(data, K1, K2, K3, M, seed = seed, vh_method = vh_method, K0 = K0, m = m, threshold = threshold, return_diagnostics = return_diagnostics))
  }

  fit_topicscore_hosvd_method(data, K1, K2, K3, M, seed = seed, vh_method = vh_method, K0 = K0, m = m, threshold = threshold, return_diagnostics = return_diagnostics)
}
