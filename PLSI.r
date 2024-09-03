library(irlba)
library(clue)


preprocess_U <- function(U, K) {
  #' This function inverts the singular vectors to make sure that the first entry is non-negative. 
  #' This is done to ensure that the SPOC algorithm converges to the same solution every time.
  #'
  #' @param U the singular vector
  for (k in 1:K) {
    if (U[1, k] < 0) { 
      U[, k] <- -1 * U[, k]
    }
  }
  return(U)
}


euclidean_proj_simplex <- function(v) {
  n <- length(v)
  u <- sort(v, decreasing = TRUE)
  sv <- cumsum(u)
  rho <- max(which(u * (1:n) - (sv - 1) > 0))
  theta <- (sv[rho] - 1) / rho
  w <- pmax(v - theta, 0)
  return(w)
}

get_W_hat <- function(U, H) {
  projector <- t(H) %*% solve(H %*% t(H))
  theta <- U %*% projector
  theta_simplex_proj <- t(apply(theta, 1, euclidean_proj_simplex))
  return(theta_simplex_proj)
}

get_component_mapping <- function(stats_1, stats_2) {
  similarity <- stats_1 %*% t(stats_2)
  cost_matrix <- -similarity
  assignment <- solve_LSAP(cost_matrix, maximum = FALSE)
  P <- matrix(0, nrow = nrow(cost_matrix), ncol = ncol(cost_matrix))
  P[cbind(1:nrow(P), assignment)] <- 1
  return(P)
}

trunc_svd <- function(X, K) {
  svd_res <- svd(X)
  U_k <- svd_res$u[, 1:K]
  L_k <- diag(svd_res$d[1:K])
  VT_k <- t(svd_res$v[, 1:K])
  return(list(U_k = U_k, L_k = L_k, VT_k = VT_k))
}


fit_SPOC <- function(X, K) {
  svd_res = trunc_svd(X, K)
  U <- svd_res$U_k
  L <- svd_res$L_k
  V <- svd_res$V_k

  J <- c()
  S <- t(preprocess_U(U, K))

  for (t in 1:K) {
    maxind <- which.max(colSums(S^2))
    s <- matrix(S[, maxind], nrow = K, ncol = 1)
    S1 <- (diag(K) - (s %*% t(s)) / sum(s^2)) %*% S
    S <- S1
    J <- c(J, maxind)
  }

  H_hat <- U[J, , drop = FALSE]
  W_hat <- get_W_hat(U, H_hat)
  return(list(What = W_hat))
}

evaluate_W <- function(W, W_hat, grp){
  P <- get_component_mapping(t(W_hat), W)
  W_hat <- W_hat %*% P
  
  assgn <- apply(W_hat, 1, which.max)
  accuracy <- sum(assgn == grp) / nrow(X)
  err <- norm(t(W) - W_hat, type = "F")
  cat(err, "\n")
  return(list(acc=acc, err= err))
}



