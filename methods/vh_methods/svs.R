vertices_est <- function(R, K0 = NULL, m = NULL, K = NULL) {
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
        "SVS requires at least %d usable cluster centers after normalization; found only %d.",
        K,
        max_center_count
      )
    )
  }

  center_count <- max(K, min(m, max_center_count))
  if (is.null(K0) || !is.finite(K0)) {
    K0 <- ceiling(1.5 * K)
  }
  K0 <- max(K, min(as.integer(K0), center_count))

  obj <- stats::kmeans(
    R,
    centers = center_count,
    iter.max = K * 100,
    nstart = max(10L, K * 10L)
  )

  theta <- as.matrix(obj$centers)
  theta_original <- theta

  inner <- theta %*% t(theta)
  distance <- diag(inner) %*% t(rep(1, length(diag(inner)))) + rep(1, length(diag(inner))) %*% t(diag(inner)) - 2 * inner
  top2 <- which(distance == max(distance), arr.ind = TRUE)[1, ]
  theta0 <- as.matrix(theta[top2, , drop = FALSE])
  theta <- as.matrix(theta[-top2, , drop = FALSE])

  if (K0 > 2) {
    for (k0 in 3:K0) {
      inner <- theta %*% t(theta)
      distance <- matrix(1, nrow(theta0), 1) %*% t(diag(inner)) - 2 * theta0 %*% t(theta)
      ave_dist <- colMeans(distance)
      index <- which(ave_dist == max(ave_dist))[1]
      theta0 <- rbind(theta0, theta[index, , drop = FALSE])
      theta <- as.matrix(theta[-index, , drop = FALSE])
    }
    theta <- theta0
  }

  if (K0 <= K) {
    return(list(V = theta, theta = theta_original))
  }

  comb <- utils::combn(seq_len(K0), K)
  max_values <- rep(0, dim(comb)[2])
  for (i in seq_len(dim(comb)[2])) {
    for (j in seq_len(K0)) {
      max_values[i] <- max(simplex_dist(as.matrix(theta[j, ]), as.matrix(theta[comb[, i], ])), max_values[i])
    }
  }
  min_index <- which(max_values == min(max_values))
  new_theta <- theta[comb[, min_index[1]], ]

  list(V = new_theta, theta = theta_original)
}
