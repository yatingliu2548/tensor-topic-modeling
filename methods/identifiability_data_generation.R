# Targeted data generation for identifiability stress tests.
#
# Main idea:
# - Build explicit anchor-style structure in A1, A2, and A3.
# - Use epsilon to contaminate those anchors continuously.
# - Generate two independent half datasets from the same latent tensor so we can
#   measure split-half stability for methods and VH backends.

normalize_cols_identifiability <- function(mat) {
  cs <- colSums(mat)
  cs[cs == 0] <- 1
  sweep(mat, 2, cs, "/")
}

approx_anchor_row <- function(K, topic_id, epsilon) {
  if (K == 1) {
    return(1)
  }
  row <- rep(epsilon / (K - 1), K)
  row[topic_id] <- 1 - epsilon
  row
}

generate_approx_anchor_membership <- function(Q, K, epsilon, alpha_dirichlet = 0.3) {
  if (Q < K) {
    stop(sprintf("Q=%d must be at least K=%d to create one anchor row per topic.", Q, K))
  }

  A <- VGAM::rdiric(Q, rep(alpha_dirichlet, K))
  for (k in seq_len(K)) {
    A[k, ] <- approx_anchor_row(K, k, epsilon)
  }
  A
}

sample_membership_template <- function(Q, K, alpha_dirichlet = 0.3) {
  if (Q < K) {
    stop(sprintf("Q=%d must be at least K=%d to create one anchor row per topic.", Q, K))
  }
  list(
    Q = Q,
    K = K,
    background = VGAM::rdiric(Q, rep(alpha_dirichlet, K)),
    anchor_rows = seq_len(K)
  )
}

build_membership_from_template <- function(template, epsilon) {
  A <- template$background
  for (k in seq_len(template$K)) {
    A[template$anchor_rows[k], ] <- approx_anchor_row(template$K, k, epsilon)
  }
  A
}

generate_approx_anchor_topics <- function(
  R,
  K,
  epsilon,
  n_anchors = 2,
  delta_anchor = 0.08,
  anchor_mode = c("membership_like", "topic_mass"),
  sparsity = FALSE,
  a_zipf = 1,
  offset_zipf = 2.7,
  vary_by_topic = FALSE
) {
  anchor_mode <- match.arg(anchor_mode)
  if (R < K * n_anchors) {
    stop(sprintf("R=%d must be at least K*n_anchors=%d.", R, K * n_anchors))
  }
  if (n_anchors * delta_anchor >= 1) {
    stop("Need n_anchors * delta_anchor < 1 so non-anchor mass remains positive.")
  }

  A <- matrix(0, nrow = R, ncol = K)
  anchor_blocks <- vector("list", K)
  remaining_idx <- if (K * n_anchors < R) seq.int(K * n_anchors + 1, R) else integer(0)

  if (anchor_mode == "membership_like") {
    memberships <- matrix(0, nrow = R, ncol = K)
    row_scales <- numeric(R)

    for (k in seq_len(K)) {
      idx <- ((k - 1) * n_anchors + 1):(k * n_anchors)
      anchor_blocks[[k]] <- idx
      memberships[idx, ] <- matrix(
        rep(approx_anchor_row(K, k, epsilon), times = length(idx)),
        nrow = length(idx),
        byrow = TRUE
      )
      row_scales[idx] <- delta_anchor
    }

    if (length(remaining_idx) > 0) {
      memberships[remaining_idx, ] <- VGAM::rdiric(length(remaining_idx), rep(0.3, K))
      if (sparsity) {
        rank_idx <- seq_len(length(remaining_idx))
        base_scale <- vapply(
          1 / (rank_idx + offset_zipf)^a_zipf,
          function(u) stats::rexp(1, rate = u),
          numeric(1)
        )
      } else {
        base_scale <- stats::runif(length(remaining_idx))
      }
      row_scales[remaining_idx] <- base_scale
    }

    A <- memberships * row_scales
    return(list(
      A3 = normalize_cols_identifiability(A),
      anchor_blocks = anchor_blocks
    ))
  }

  for (k in seq_len(K)) {
    idx <- ((k - 1) * n_anchors + 1):(k * n_anchors)
    anchor_blocks[[k]] <- idx
    if (K == 1) {
      A[idx, k] <- delta_anchor
    } else {
      A[idx, ] <- matrix(delta_anchor * epsilon / (K - 1), nrow = length(idx), ncol = K)
      A[idx, k] <- delta_anchor * (1 - epsilon)
    }
  }

  if (length(remaining_idx) > 0) {
    if (sparsity) {
      background <- matrix(0, nrow = length(remaining_idx), ncol = K)
      if (vary_by_topic) {
        for (k in seq_len(K)) {
          rank_idx <- sample(seq_len(length(remaining_idx)), length(remaining_idx))
          background[, k] <- vapply(
            1 / (rank_idx + offset_zipf)^a_zipf,
            function(u) stats::rexp(1, rate = u),
            numeric(1)
          )
        }
      } else {
        background <- sapply(
          1 / (seq_len(length(remaining_idx)) + offset_zipf)^a_zipf,
          function(u) stats::rexp(K, rate = u)
        )
        background <- t(background)
      }
    } else {
      background <- matrix(stats::runif(length(remaining_idx) * K), nrow = length(remaining_idx), ncol = K)
    }

    target_mass <- 1 - colSums(A[seq_len(K * n_anchors), , drop = FALSE])
    bg_mass <- colSums(background)
    bg_mass[bg_mass == 0] <- 1
    background <- background %*% diag(target_mass / bg_mass)
    A[remaining_idx, ] <- background
  }

  list(
    A3 = normalize_cols_identifiability(A),
    anchor_blocks = anchor_blocks
  )
}

sample_topic_template <- function(
  R,
  K,
  n_anchors = 2,
  delta_anchor = 0.08,
  anchor_mode = c("membership_like", "topic_mass"),
  sparsity = FALSE,
  a_zipf = 1,
  offset_zipf = 2.7,
  vary_by_topic = FALSE
) {
  anchor_mode <- match.arg(anchor_mode)
  if (R < K * n_anchors) {
    stop(sprintf("R=%d must be at least K*n_anchors=%d.", R, K * n_anchors))
  }
  if (n_anchors * delta_anchor >= 1) {
    stop("Need n_anchors * delta_anchor < 1 so non-anchor mass remains positive.")
  }

  anchor_blocks <- lapply(seq_len(K), function(k) ((k - 1) * n_anchors + 1):(k * n_anchors))
  remaining_idx <- if (K * n_anchors < R) seq.int(K * n_anchors + 1, R) else integer(0)

  if (anchor_mode == "membership_like") {
    memberships <- matrix(0, nrow = R, ncol = K)
    row_scales <- numeric(R)

    for (k in seq_len(K)) {
      idx <- anchor_blocks[[k]]
      memberships[idx, ] <- matrix(
        rep(approx_anchor_row(K, k, 0), times = length(idx)),
        nrow = length(idx),
        byrow = TRUE
      )
      row_scales[idx] <- delta_anchor
    }

    if (length(remaining_idx) > 0) {
      memberships[remaining_idx, ] <- VGAM::rdiric(length(remaining_idx), rep(0.3, K))
      if (sparsity) {
        rank_idx <- seq_len(length(remaining_idx))
        row_scales[remaining_idx] <- vapply(
          1 / (rank_idx + offset_zipf)^a_zipf,
          function(u) stats::rexp(1, rate = u),
          numeric(1)
        )
      } else {
        row_scales[remaining_idx] <- stats::runif(length(remaining_idx))
      }
    }

    return(list(
      anchor_mode = anchor_mode,
      R = R,
      K = K,
      n_anchors = n_anchors,
      delta_anchor = delta_anchor,
      anchor_blocks = anchor_blocks,
      remaining_idx = remaining_idx,
      memberships = memberships,
      row_scales = row_scales
    ))
  }

  background <- NULL
  if (length(remaining_idx) > 0) {
    if (sparsity) {
      background <- matrix(0, nrow = length(remaining_idx), ncol = K)
      if (vary_by_topic) {
        for (k in seq_len(K)) {
          rank_idx <- sample(seq_len(length(remaining_idx)), length(remaining_idx))
          background[, k] <- vapply(
            1 / (rank_idx + offset_zipf)^a_zipf,
            function(u) stats::rexp(1, rate = u),
            numeric(1)
          )
        }
      } else {
        background <- sapply(
          1 / (seq_len(length(remaining_idx)) + offset_zipf)^a_zipf,
          function(u) stats::rexp(K, rate = u)
        )
        background <- t(background)
      }
    } else {
      background <- matrix(stats::runif(length(remaining_idx) * K), nrow = length(remaining_idx), ncol = K)
    }
  }

  list(
    anchor_mode = anchor_mode,
    R = R,
    K = K,
    n_anchors = n_anchors,
    delta_anchor = delta_anchor,
    anchor_blocks = anchor_blocks,
    remaining_idx = remaining_idx,
    background = background
  )
}

build_topics_from_template <- function(template, epsilon) {
  A <- matrix(0, nrow = template$R, ncol = template$K)

  if (template$anchor_mode == "membership_like") {
    memberships <- template$memberships
    for (k in seq_len(template$K)) {
      idx <- template$anchor_blocks[[k]]
      memberships[idx, ] <- matrix(
        rep(approx_anchor_row(template$K, k, epsilon), times = length(idx)),
        nrow = length(idx),
        byrow = TRUE
      )
    }
    A <- memberships * template$row_scales
    return(list(
      A3 = normalize_cols_identifiability(A),
      anchor_blocks = template$anchor_blocks
    ))
  }

  for (k in seq_len(template$K)) {
    idx <- template$anchor_blocks[[k]]
    if (template$K == 1) {
      A[idx, k] <- template$delta_anchor
    } else {
      A[idx, ] <- matrix(template$delta_anchor * epsilon / (template$K - 1), nrow = length(idx), ncol = template$K)
      A[idx, k] <- template$delta_anchor * (1 - epsilon)
    }
  }

  if (length(template$remaining_idx) > 0) {
    target_mass <- 1 - colSums(A[seq_len(template$K * template$n_anchors), , drop = FALSE])
    bg_mass <- colSums(template$background)
    bg_mass[bg_mass == 0] <- 1
    background <- template$background %*% diag(target_mass / bg_mass)
    A[template$remaining_idx, ] <- background
  }

  list(
    A3 = normalize_cols_identifiability(A),
    anchor_blocks = template$anchor_blocks
  )
}

sample_identifiability_latent_template <- function(
  Q1 = 30,
  Q2 = 30,
  R = 5000,
  K1 = 2,
  K2 = 2,
  K3 = 4,
  alpha_membership = 0.3,
  alpha_core = 1,
  n_anchors = 2,
  delta_anchor = 0.08,
  a3_anchor_mode = "membership_like",
  seed = 123,
  sparsity = FALSE,
  a_zipf = 1,
  offset_zipf = 2.7,
  vary_by_topic = FALSE
) {
  set.seed(seed)

  A1_template <- sample_membership_template(Q1, K1, alpha_dirichlet = alpha_membership)
  A2_template <- sample_membership_template(Q2, K2, alpha_dirichlet = alpha_membership)
  A3_template <- sample_topic_template(
    R = R,
    K = K3,
    n_anchors = n_anchors,
    delta_anchor = delta_anchor,
    anchor_mode = a3_anchor_mode,
    sparsity = sparsity,
    a_zipf = a_zipf,
    offset_zipf = offset_zipf,
    vary_by_topic = vary_by_topic
  )

  G_array <- VGAM::rdiric(K1 * K2, rep(alpha_core, K3))
  G <- rTensor::as.tensor(array(G_array, dim = c(K1, K2, K3)))

  list(
    A1_template = A1_template,
    A2_template = A2_template,
    A3_template = A3_template,
    G = G,
    seed = seed,
    alpha_membership = alpha_membership,
    alpha_core = alpha_core,
    n_anchors = n_anchors,
    delta_anchor = delta_anchor,
    a3_anchor_mode = a3_anchor_mode,
    sparsity = sparsity
  )
}

build_tensor_dataset_from_counts <- function(Y3, D3, A1, A2, A3_full, G, active_vocab, Q1, Q2) {
  A3_active <- A3_full[active_vocab, , drop = FALSE]
  A3_active <- normalize_cols_identifiability(A3_active)

  list(
    Y = tensorization(Y3[active_vocab, , drop = FALSE], mode = 3, Q1 = Q1, Q2 = Q2, Q3 = length(active_vocab)),
    D = tensorization(D3[active_vocab, , drop = FALSE], mode = 3, Q1 = Q1, Q2 = Q2, Q3 = length(active_vocab)),
    A1 = A1,
    A2 = A2,
    A3 = A3_active,
    G = G,
    vocab = active_vocab
  )
}

generate_identifiability_split_tensor <- function(
  Q1 = 30,
  Q2 = 30,
  R = 5000,
  K1 = 2,
  K2 = 2,
  K3 = 4,
  epsilon = 0,
  alpha_membership = 0.3,
  alpha_core = 1,
  n_anchors = 2,
  delta_anchor = 0.08,
  a3_anchor_mode = "membership_like",
  M_half = 2500,
  seed = 123,
  latent_seed = NULL,
  split_seed = NULL,
  latent_template = NULL,
  sparsity = FALSE,
  a_zipf = 1,
  offset_zipf = 2.7,
  vary_by_topic = FALSE
) {
  required_fns <- c("tensor_create", "matrization_tensor", "tensorization")
  missing_fns <- required_fns[!vapply(required_fns, exists, logical(1), mode = "function")]
  if (length(missing_fns) > 0) {
    stop(sprintf("Load methods/data_generation.R first. Missing: %s", paste(missing_fns, collapse = ", ")))
  }

  if (is.null(latent_seed) || !is.finite(latent_seed)) {
    latent_seed <- seed
  }
  if (is.null(split_seed) || !is.finite(split_seed)) {
    split_seed <- seed
  }

  if (is.null(latent_template)) {
    latent_template <- sample_identifiability_latent_template(
      Q1 = Q1,
      Q2 = Q2,
      R = R,
      K1 = K1,
      K2 = K2,
      K3 = K3,
      alpha_membership = alpha_membership,
      alpha_core = alpha_core,
      n_anchors = n_anchors,
      delta_anchor = delta_anchor,
      a3_anchor_mode = a3_anchor_mode,
      seed = latent_seed,
      sparsity = sparsity,
      a_zipf = a_zipf,
      offset_zipf = offset_zipf,
      vary_by_topic = vary_by_topic
    )
  }

  A1 <- build_membership_from_template(latent_template$A1_template, epsilon)
  A2 <- build_membership_from_template(latent_template$A2_template, epsilon)
  A3_obj <- build_topics_from_template(latent_template$A3_template, epsilon)
  A3_full <- A3_obj$A3
  G <- latent_template$G

  D0 <- tensor_create(G, A1, A2, A3_full)
  D3 <- matrization_tensor(D0, 3)

  set.seed(split_seed)
  Y3_half1 <- sapply(seq_len(Q1 * Q2), function(i) stats::rmultinom(1, M_half, D3[, i]))
  Y3_half2 <- sapply(seq_len(Q1 * Q2), function(i) stats::rmultinom(1, M_half, D3[, i]))
  Y3_full <- Y3_half1 + Y3_half2

  active_vocab <- which(rowSums(Y3_full) > 0)
  if (length(active_vocab) == 0) {
    stop("No active vocabulary remained after sampling; increase M_half or adjust parameters.")
  }

  truth <- build_tensor_dataset_from_counts(
    Y3 = Y3_full,
    D3 = D3,
    A1 = A1,
    A2 = A2,
    A3_full = A3_full,
    G = G,
    active_vocab = active_vocab,
    Q1 = Q1,
    Q2 = Q2
  )

  half1 <- build_tensor_dataset_from_counts(
    Y3 = Y3_half1,
    D3 = D3,
    A1 = A1,
    A2 = A2,
    A3_full = A3_full,
    G = G,
    active_vocab = active_vocab,
    Q1 = Q1,
    Q2 = Q2
  )

  half2 <- build_tensor_dataset_from_counts(
    Y3 = Y3_half2,
    D3 = D3,
    A1 = A1,
    A2 = A2,
    A3_full = A3_full,
    G = G,
    active_vocab = active_vocab,
    Q1 = Q1,
    Q2 = Q2
  )

  list(
    truth = truth,
    half1 = half1,
    half2 = half2,
    meta = list(
      epsilon = epsilon,
      alpha_membership = alpha_membership,
      alpha_core = alpha_core,
      n_anchors = n_anchors,
      delta_anchor = delta_anchor,
      a3_anchor_mode = a3_anchor_mode,
      M_half = M_half,
      seed = seed,
      latent_seed = latent_seed,
      split_seed = split_seed,
      template_seed = latent_template$seed,
      sparsity = sparsity,
      anchor_rows_mode1 = seq_len(K1),
      anchor_rows_mode2 = seq_len(K2),
      anchor_blocks_mode3 = A3_obj$anchor_blocks
    )
  )
}
