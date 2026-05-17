# Data Generation Utilities for Tensor Topic Benchmarks
#
# This file defines:
# 1) Tensor/matrix conversion helpers used across methods.
# 2) Synthetic data generation under the same model family used by benchmarks.
#
# Returned dataset fields:
# - Y: observed count tensor (Q1 x Q2 x |V_active|)
# - D: population probability tensor for active vocabulary
# - A1, A2: mode-1/mode-2 membership matrices
# - A3: topic-word matrix (active vocabulary only)
# - G: core tensor
# - vocab: active vocabulary indices in the original R-sized vocabulary

library(VGAM)
library(rTensor)

# Convert membership matrix rows into hard anchor rows by forcing the largest
# entry in each topic column to be one-hot.
anchor_document <- function(A) {
  result_matrix <- A
  k <- ncol(A)
  col_index <- seq_len(k)
  for (col in seq_len(k)) {
    max_index <- which.max(result_matrix[, col])
    result_matrix[max_index, col] <- 1
    result_matrix[max_index, col_index[-col]] <- 0
  }
  result_matrix
}

# Reconstruct tensor G x1 A1 x2 A2 x3 A3.
tensor_create <- function(G, A1, A2, A3) {
  t1 <- ttm(G, A1, 1)
  t2 <- ttm(t1, A2, 2)
  ttm(t2, A3, 3)
}

# Tensor matricization along selected mode.
matrization_tensor <- function(G, mode) {
  tensor_data <- G@data
  if (mode == 1) {
    return(matrix(aperm(tensor_data, c(1, 3, 2)), nrow = dim(G)[1]))
  }
  if (mode == 2) {
    return(matrix(aperm(tensor_data, c(2, 3, 1)), nrow = dim(G)[2]))
  }
  if (mode == 3) {
    return(matrix(aperm(tensor_data, c(3, 2, 1)), nrow = dim(G)[3]))
  }
  stop("mode must be one of {1,2,3}")
}

# Inverse operation for matrization_tensor().
tensorization <- function(M, mode, Q1, Q2, Q3) {
  if (mode == 1) {
    reshaped <- array(as.vector(M), dim = c(Q1, Q3, Q2))
    tensor_back <- aperm(reshaped, c(1, 3, 2))
  } else if (mode == 2) {
    reshaped <- array(as.vector(M), dim = c(Q2, Q3, Q1))
    tensor_back <- aperm(reshaped, c(3, 1, 2))
  } else if (mode == 3) {
    reshaped <- array(as.vector(M), dim = c(Q3, Q2, Q1))
    tensor_back <- aperm(reshaped, c(3, 2, 1))
  } else {
    stop("mode must be one of {1,2,3}")
  }
  as.tensor(tensor_back)
}

normalize_probability_vector <- function(prob) {
  prob <- as.numeric(prob)
  prob[!is.finite(prob)] <- 0
  prob <- pmax(prob, 0)
  total <- sum(prob)
  if (total <= 0) {
    stop("probability vector must have positive total mass")
  }
  prob / total
}

normalize_count_model <- function(count_model) {
  count_model <- tolower(gsub("-", "_", as.character(count_model)))
  if (count_model %in% c("dm", "dirichlet", "dirichlet_multinomial")) {
    return("dirichlet_multinomial")
  }
  if (count_model %in% c("zi", "zero_inflated", "zero_inflated_multinomial")) {
    return("zero_inflated")
  }
  if (count_model == "multinomial") {
    return("multinomial")
  }
  stop("count_model must be one of: multinomial, dirichlet_multinomial, zero_inflated")
}

sample_multinomial_counts <- function(prob, M) {
  prob <- normalize_probability_vector(prob)
  as.integer(stats::rmultinom(1, size = M, prob = prob)[, 1])
}

sample_dirichlet_multinomial_counts <- function(prob, M, dirichlet_concentration = 100) {
  if (!is.finite(dirichlet_concentration) || dirichlet_concentration <= 0) {
    stop("dirichlet_concentration must be positive")
  }

  prob <- normalize_probability_vector(prob)
  support <- which(prob > 0)
  if (length(support) == 1) {
    counts <- integer(length(prob))
    counts[support] <- M
    return(counts)
  }

  sampled_prob <- numeric(length(prob))
  sampled_prob[support] <- as.numeric(
    VGAM::rdiric(1, dirichlet_concentration * prob[support])
  )
  sample_multinomial_counts(sampled_prob, M)
}

sample_zero_inflated_multinomial_counts <- function(prob, M, zero_inflation_prob = 0.2) {
  if (!is.finite(zero_inflation_prob) || zero_inflation_prob < 0 || zero_inflation_prob >= 1) {
    stop("zero_inflation_prob must be in [0, 1)")
  }

  prob <- normalize_probability_vector(prob)
  support <- which(prob > 0)
  keep <- rep(FALSE, length(prob))
  keep[support] <- stats::runif(length(support)) >= zero_inflation_prob
  if (!any(keep[support])) {
    keep[sample(support, 1)] <- TRUE
  }

  inflated_prob <- prob
  inflated_prob[!keep] <- 0
  sample_multinomial_counts(inflated_prob, M)
}

sample_count_matrix <- function(
  D3,
  M,
  count_model = "multinomial",
  dirichlet_concentration = 100,
  zero_inflation_prob = 0.2
) {
  count_model <- normalize_count_model(count_model)
  vapply(seq_len(ncol(D3)), function(i) {
    if (count_model == "dirichlet_multinomial") {
      return(sample_dirichlet_multinomial_counts(D3[, i], M, dirichlet_concentration))
    }
    if (count_model == "zero_inflated") {
      return(sample_zero_inflated_multinomial_counts(D3[, i], M, zero_inflation_prob))
    }
    sample_multinomial_counts(D3[, i], M)
  }, integer(nrow(D3)))
}

# Synthetic benchmark generator.
#
# Notes:
# - `n_max_zipf` is kept for API compatibility with older scripts.
# - `M` is document length for count sampling.
synthetic_dataset_creation <- function(
  Q1,
  Q2,
  R,
  K1,
  K2,
  K3,
  alpha_dirichlet = 1,
  n_max_zipf = 5 * 1e3,
  a_zipf = 1,
  offset_zipf = 2.7,
  n_anchors = 0,
  delta_anchor = 1,
  M = 500,
  seed = 123,
  vary_by_topic = FALSE,
  sparsity = TRUE,
  count_model = "multinomial",
  dirichlet_concentration = 100,
  zero_inflation_prob = 0.2
) {
  set.seed(seed)
  count_model <- normalize_count_model(count_model)

  # Mode memberships and core tensor.
  A1 <- VGAM::rdiric(Q1, rep(alpha_dirichlet, K1))
  A2 <- VGAM::rdiric(Q2, rep(alpha_dirichlet, K2))
  A1 <- anchor_document(A1)
  A2 <- anchor_document(A2)

  G <- VGAM::rdiric(K1 * K2, rep(alpha_dirichlet, K3))
  G <- array(G, dim = c(K1, K2, K3))
  G <- as.tensor(G)

  # Topic-word matrix A3.
  # Conditions controlled by:
  # - sparsity: TRUE uses Zipf-like heavy tail (sparse/long-tail vocabulary);
  #             FALSE uses uniform random weights (denser vocabulary).
  # - n_anchors: number of anchor words per topic. If > 0, first K3*n_anchors
  #              vocabulary entries are reserved as anchor blocks.
  # - vary_by_topic (only used when sparsity=TRUE and n_anchors>0):
  #                  TRUE gives each topic its own shuffled Zipf ranks;
  #                  FALSE shares the same Zipf decay profile across topics.
  if (sparsity) {
    # Sparse topic-word distributions (Zipf + exponential noise).
    if (n_anchors > 0) {
      # Case 1: sparse + anchors.
      # Build K3 topic rows, then transpose to R x K3 at the end.
      A <- matrix(0, nrow = K3, ncol = R)
      for (k in seq_len(K3)) {
        # Topic k gets its dedicated anchor block with fixed mass delta_anchor.
        A[k, ((k - 1) * n_anchors + 1):(k * n_anchors)] <- delta_anchor
      }
      if (vary_by_topic) {
        # Each topic has a different permutation of Zipf ranks.
        for (k in seq_len(K3)) {
          idx <- sample(seq_len(R - n_anchors * K3), R - n_anchors * K3)
          A[k, (K3 * n_anchors + 1):R] <- sapply(
            1 / (idx + offset_zipf)^a_zipf,
            function(u) rexp(1, u)
          )
        }
      } else {
        # All topics share the same Zipf rank ordering.
        A[, (K3 * n_anchors + 1):R] <- sapply(
          1 / (seq_len(R - n_anchors * K3) + offset_zipf)^a_zipf,
          function(u) rexp(K3, u)
        )
      }
      A <- t(A)
      # Rescale non-anchor rows so each topic column sums to 1 after anchor mass
      # has been assigned.
      A[(K3 * n_anchors + 1):R, ] <- A[(K3 * n_anchors + 1):R, ] %*% diag(
        (1 - colSums(A[1:(K3 * n_anchors), , drop = FALSE])) /
          colSums(A[(K3 * n_anchors + 1):R, , drop = FALSE])
      )
    } else {
      # Case 2: sparse without anchors.
      A <- sapply(1 / (seq_len(R) + offset_zipf)^a_zipf, function(u) rexp(K3, u))
      A <- t(A)
      A <- A %*% diag(1 / colSums(A))
    }
  } else {
    # Dense topic-word distributions (uniform random noise).
    if (n_anchors > 0) {
      # Case 3: dense + anchors.
      A <- matrix(0, nrow = K3, ncol = R)
      for (k in seq_len(K3)) {
        A[k, ((k - 1) * n_anchors + 1):(k * n_anchors)] <- delta_anchor
      }
      A[, (K3 * n_anchors + 1):R] <- matrix(runif(K3 * (R - K3 * n_anchors)), nrow = K3)
      A <- t(A)
      # Same normalization idea: fill non-anchor mass to keep column sums at 1.
      A[(K3 * n_anchors + 1):R, ] <- A[(K3 * n_anchors + 1):R, ] %*% diag(
        (1 - colSums(A[1:(K3 * n_anchors), , drop = FALSE])) /
          colSums(A[(K3 * n_anchors + 1):R, , drop = FALSE])
      )
    } else {
      # Case 4: dense without anchors.
      A <- matrix(runif(K3 * R), ncol = K3)
      A <- A %*% diag(1 / colSums(A))
    }
  }
  A3 <- A

  # Population tensor and observed counts.
  D0 <- tensor_create(G, A1, A2, A3)
  D3 <- matrization_tensor(D0, 3)
  Y3 <- sample_count_matrix(
    D3,
    M = M,
    count_model = count_model,
    dirichlet_concentration = dirichlet_concentration,
    zero_inflation_prob = zero_inflation_prob
  )

  # Keep active vocabulary only.
  active <- which(rowSums(Y3) > 0)
  D3 <- D3[active, , drop = FALSE]
  Y3 <- Y3[active, , drop = FALSE]
  A3 <- A3[active, , drop = FALSE]

  col_sums <- colSums(A3)
  col_sums[col_sums == 0] <- 1
  A3 <- A3 %*% diag(1 / col_sums)

  D <- tensorization(D3, mode = 3, Q1 = Q1, Q2 = Q2, Q3 = length(active))
  Y <- tensorization(Y3, mode = 3, Q1 = Q1, Q2 = Q2, Q3 = length(active))

  list(
    Y = Y,
    A1 = A1,
    A2 = A2,
    A3 = A3,
    vocab = active,
    D = D,
    G = G,
    count_model = count_model,
    count_parameters = list(
      dirichlet_concentration = dirichlet_concentration,
      zero_inflation_prob = zero_inflation_prob
    )
  )
}

synthetic_dataset_creation_overdispersed <- function(..., dirichlet_concentration = 100) {
  synthetic_dataset_creation(
    ...,
    count_model = "dirichlet_multinomial",
    dirichlet_concentration = dirichlet_concentration
  )
}

synthetic_dataset_creation_zero_inflated <- function(..., zero_inflation_prob = 0.2) {
  synthetic_dataset_creation(
    ...,
    count_model = "zero_inflated",
    zero_inflation_prob = zero_inflation_prob
  )
}
