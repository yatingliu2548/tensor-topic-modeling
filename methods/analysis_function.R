## Shared analysis and evaluation helpers used across methods and VH backends.

solve_lsap_safe <- function(cost_matrix) {
  if (requireNamespace("clue", quietly = TRUE)) {
    return(as.integer(clue::solve_LSAP(cost_matrix)))
  }

  n <- nrow(cost_matrix)
  if (n != ncol(cost_matrix)) {
    stop("cost_matrix must be square when clue is unavailable")
  }
  if (n > 9) {
    stop("clue package is required for assignment problems larger than 9x9")
  }

  perms <- combinat::permn(seq_len(n))
  best_cost <- Inf
  best_perm <- NULL
  for (p in perms) {
    perm <- as.integer(p)
    cur_cost <- sum(cost_matrix[cbind(seq_len(n), perm)])
    if (cur_cost < best_cost) {
      best_cost <- cur_cost
      best_perm <- perm
    }
  }
  best_perm
}

replaceWithLeastPositive <- function(vec) {
  vec[vec <= 0] <- min(vec[vec > 0])
  vec
}

nearPD_safe <- function(mat) {
  mat <- (mat + t(mat)) / 2
  eigenObj <- eigen(mat)
  values <- replaceWithLeastPositive(eigenObj$values)
  vectors <- eigenObj$vectors
  vectors %*% diag(values) %*% t(vectors)
}

l2_error <- function(A, B) {
  sqrt(sum((A - B)^2))
}

l1_error <- function(A, B) {
  sum(abs(A - B))
}

simplex_dist <- function(theta, V) {
  VV <- cbind(diag(rep(1, dim(V)[1] - 1)), -rep(1, dim(V)[1] - 1)) %*% V
  D <- VV %*% t(VV)
  d <- VV %*% (theta - V[dim(V)[1], ])

  A <- cbind(diag(rep(1, dim(V)[1] - 1)), -rep(1, dim(V)[1] - 1))
  b0 <- c(rep(0, dim(V)[1] - 1), -1)

  obj <- quadprog::solve.QP(D, d, A, b0)
  sum((theta - V[dim(V)[1], ])^2) + 2 * obj$value
}

error1_A <- function(A, A_hat) {
  K <- dim(A)[2]
  all_perm <- combinat::permn(seq_len(K))
  error <- Inf

  for (i in seq_along(all_perm)) {
    error <- min(error, mean(colSums(abs(A[, all_perm[[i]]] - A_hat))))
  }

  error
}

error2_A <- function(A, A_hat) {
  K <- dim(A)[2]
  used <- rep(1, K)
  A_perm <- matrix(0, dim(A)[1], dim(A)[2])

  for (k in seq_len(K)) {
    dis <- colSums(abs(A - A_hat[, k])) * used
    index <- which(dis == min(dis))[1]
    A_perm[, k] <- A[, index]
    used[index] <- Inf
  }

  mean(colSums(abs(A_perm - A_hat)))
}

matrix_lp_distance <- function(A, B, lp = 2) {
  if (lp == 2) {
    error_matrix <- outer(
      seq_len(ncol(A)),
      seq_len(ncol(B)),
      Vectorize(function(i, j) l2_error(A[, i], B[, j]))
    )
  } else {
    error_matrix <- outer(
      seq_len(ncol(A)),
      seq_len(ncol(B)),
      Vectorize(function(i, j) l1_error(A[, i], B[, j]))
    )
  }

  permutation <- solve_lsap_safe(error_matrix)
  B_permuted <- B[, permutation]

  if (lp == 2) {
    error <- l2_error(A, B_permuted)
  } else {
    error <- l1_error(A, B_permuted)
  }

  permutation_matrix <- matrix(0, nrow = ncol(A), ncol = ncol(A))
  for (i in seq_along(permutation)) {
    permutation_matrix[i, permutation[i]] <- 1
  }

  list(error = error, permutation = permutation_matrix)
}

plot_slice <- function(tensor, k = 1, xlab = "Groups", ylab = "Topics", yes = TRUE, option = "H", limits = c(0, 1)) {
  slice_frames <- vector("list", dim(tensor)[k])

  for (i in seq_len(dim(tensor)[k])) {
    if (k == 1) {
      ts <- tensor[i, , ]
    } else if (k == 2) {
      ts <- tensor[, i, ]
    } else {
      ts <- tensor[, , i]
    }

    ts <- as.matrix(ts)
    slice_df <- expand.grid(
      X = seq_len(nrow(ts)),
      Y = seq_len(ncol(ts))
    )
    slice_df$Value <- as.vector(ts)
    slice_df$Slice <- factor(i)
    slice_frames[[i]] <- slice_df
  }

  df_long <- do.call(rbind, slice_frames)

  if (isTRUE(yes)) {
    df_long$X <- as.factor(df_long$X)
    df_long$Y <- as.factor(df_long$Y)
  }

  g <- ggplot2::ggplot(df_long, ggplot2::aes(x = X, y = Y, fill = Value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(limits = limits, option = option, trans = "sqrt") +
    ggplot2::facet_wrap(~Slice, ncol = 10) +
    ggplot2::labs(x = xlab, y = ylab, fill = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      strip.text = ggplot2::element_text(face = "bold")
    )

  print(g)
}

heatmap_matrix <- function(matrix_data, xlab = "Topics", ylab = "Mode 3", trans = "sqrt") {
  matrix_data <- as.matrix(matrix_data)
  df <- expand.grid(
    Row = seq_len(nrow(matrix_data)),
    Column = seq_len(ncol(matrix_data))
  )
  df$Value <- as.vector(matrix_data)

  g <- ggplot2::ggplot(df, ggplot2::aes(x = as.factor(Column), y = Row, fill = Value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(limits = c(0, 1), option = "H", trans = trans) +
    ggplot2::labs(x = xlab, y = ylab, fill = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "red"),
      strip.text = ggplot2::element_text(face = "bold")
    )

  print(g)
}

heatmap_matrix2 <- function(matrix_data, xlab = "Topics", ylab = "Mode 3", trans = "sqrt") {
  matrix_data <- as.matrix(matrix_data)
  df <- expand.grid(
    Row = seq_len(nrow(matrix_data)),
    Column = seq_len(ncol(matrix_data))
  )
  df$Value <- as.vector(matrix_data)

  g <- ggplot2::ggplot(df, ggplot2::aes(x = as.factor(Column), y = Row, fill = Value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(limits = c(0, 1), option = "H", trans = trans) +
    ggplot2::labs(x = xlab, y = ylab, fill = "") +
    ggplot2::scale_y_continuous(breaks = seq(1, max(df$Row), by = 5)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "lightblue"),
      strip.text = ggplot2::element_text(face = "bold")
    )

  print(g)
}

error_sim <- function(D, hatD, method = NULL, i = NA_integer_) {
  data.frame(
    l1 = l1_error(D, hatD),
    l2 = l2_error(D, hatD),
    method = method,
    i = i
  )
}

get_cp <- function(A, B, C, core_values) {
  tensor_dims <- c(nrow(A), nrow(B), nrow(C))
  cp_tensor <- array(0, dim = tensor_dims)

  for (r in seq_along(core_values)) {
    rank_one_tensor <- outer(A[, r], B[, r])
    rank_one_tensor <- array(
      apply(rank_one_tensor, 1:2, function(x) outer(x, C[, r])),
      dim = tensor_dims
    )
    cp_tensor <- cp_tensor + rank_one_tensor
  }

  cp_tensor
}
