vertices_est_SP <- function(R, m = NULL, K = NULL) {
  if (is.null(K) || !is.finite(K)) {
    K <- dim(R)[2] + 1
  }
  K <- as.integer(K)
  if (is.null(m) || !is.finite(m)) {
    m <- ceiling(10 * K)
  }
  m <- max(K, as.integer(m))
  distinct_count <- nrow(unique(as.data.frame(R)))
  max_center_count <- min(nrow(R) - 1L, distinct_count)
  if (max_center_count < K) {
    stop(
      sprintf(
        "SVS-SP requires at least %d usable cluster centers after normalization; found only %d.",
        K,
        max_center_count
      )
    )
  }
  center_count <- max(K, min(m, max_center_count))

  obj <- stats::kmeans(R, center_count, iter.max = K * 100, nstart = max(10L, K * 10L))
  theta <- as.matrix(obj$centers)
  successiveProj(theta, K)
}
