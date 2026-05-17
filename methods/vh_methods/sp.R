successiveProj <- function(R, K) {
  n <- dim(R)[1]

  Y <- cbind(rep(1, n), R)
  indexSet <- c()
  valid_indices <- seq_len(dim(Y)[1])

  while (length(indexSet) < K && dim(Y)[1] > 0) {
    l2Norms <- apply(Y, 1, function(x) sqrt(sum(x^2)))
    index <- which(l2Norms == max(l2Norms))

    if (length(index) > 1) {
      r <- Matrix::rankMatrix(R[valid_indices[index], ])[1]
      if (r < length(index)) {
        chosen <- sample(index, 1)
        u <- Y[chosen, ] / sqrt(sum(Y[chosen, ]^2))
        indexSet <- c(indexSet, valid_indices[chosen])
        Y <- Y[-setdiff(index, c(chosen)), ]
        valid_indices <- setdiff(valid_indices, valid_indices[setdiff(index, c(chosen))])
        Y <- t(apply(Y, 1, function(x) x - (x %*% t(u)) %*% u))
      } else {
        u <- Y[index, ] / sqrt(sum(Y[index, ]^2))
        indexSet <- c(indexSet, valid_indices[index])
        Y <- t(apply(Y, 1, function(x) x - (x %*% t(u)) %*% u))
      }
    } else {
      indexSet <- c(indexSet, valid_indices[index])
      u <- Y[index, ] / sqrt(sum(Y[index, ]^2))
      Y <- t(apply(Y, 1, function(x) x - sum(x * u) * u))
    }
  }

  if (length(indexSet) > K) {
    indexSet <- sample(indexSet, K)
  }

  list(V = R[indexSet, ], indexSet = indexSet)
}
