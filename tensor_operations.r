library(nnTensor)

create_tensor <- function(G, A1, A2, A3){
  # First, multiply G by A along the first mode
  T1 <- ttm(G, A1, 1)
  # Then, multiply the result by B along the second mode
  T2 <- ttm(T1, A2, 2)
  # Finally, multiply by C along the third mode
  reconstructed_tensor <- ttm(T2, A3, 3)
  return(reconstructed_tensor)
}

matricization <- function(G, mode){
  tensordata = G@data
  if (mode == 1){
    matrix_G = aperm(tensordata, c(1, 3, 2)) %>% 
                    as.vector() %>% 
                    matrix(nrow = dim(G)[1], 
                    byrow = FALSE)

  }
  if (mode == 2){
    matrix_G = aperm(tensordata, c(2, 3, 1)) %>% 
                     as.vector() %>% 
                     matrix(nrow = dim(G)[2],
                            byrow = FALSE)

  }
  if (mode == 3){
    matrix_G = aperm(tensordata, c(3, 2, 1)) %>% 
                     as.vector() %>% 
                     matrix(nrow = dim(G)[3], 
                            byrow = FALSE)

  }
  return(matrix_G)
}


tensorization <- function(M, mode, Q1, Q2, Q3){
  if(mode == 1){
    reshaped_tensor <- array(as.vector(M), dim = c(Q1, Q3, Q2))
    tensor_back <- aperm(reshaped_tensor, c(1, 3, 2))
  }
  if(mode == 2){
    reshaped_tensor <- array(as.vector(M), dim = c(Q2, Q3, Q1))
    tensor_back <- aperm(reshaped_tensor, c(3, 1, 2))
  }
  if(mode == 3){
    reshaped_tensor <- array(as.vector(M), dim = c(Q3, Q2, Q1))
    tensor_back <- aperm(reshaped_tensor, c(3, 2, 1))
  }
  tensor_back = as.tensor(tensor_back)
  return(tensor_back)
}


get_cp=function(A,B,C,core_values){
 
  tensor_dims <- c(nrow(A), nrow(B), nrow(C))
  cp_tensor <- array(0, dim = tensor_dims)
 
  for (r in 1:length(core_values)) {
    rank_one_tensor <- outer(A[, r], B[, r])
    rank_one_tensor <- array(apply(rank_one_tensor, 1:2, function(x) outer(x, C[, r])), dim = tensor_dims)
    cp_tensor <- cp_tensor + 1* rank_one_tensor
  }
  return(cp_tensor)
}

