
data {
  int<lower=1> N1; // Number of reviewers
  int<lower=1> N2; // Number of papers
  int<lower=1> V; // Vocabulary size
  int<lower=1> K; // Number of topics
  int<lower=1> K1; // Number of reviewer types
  int<lower=1> K2; // Number of paper categories
  int<lower=1> D; // Total number of reviews (documents)
  int<lower=0> X[D, V]; // Document-term matrix (word counts)
  int<lower=1> reviewer_ids[D]; // Reviewer indices for each document
  int<lower=1> paper_ids[D]; // Paper indices for each document
}

parameters {
  simplex[V] phi[K]; // Word distributions for each topic
  simplex[K1] reviewer_type[N1]; // Reviewer type distribution
  simplex[K2] paper_category[N2]; // Paper category distribution
  //simplex[K] topic_assignment[N1 * N2]; // Topic assignment matrices for each level of the third dimension
  simplex[K] core[K1 * K2]; // Core
}

model {
  // Priors
  for (k in 1:K)
    phi[k] ~ dirichlet(rep_vector(1.0, V));
  
  for (n in 1:N1)
    reviewer_type[n] ~ dirichlet(rep_vector(1.0, K1));
  
  for (n in 1:N2)
    paper_category[n] ~ dirichlet(rep_vector(1.0, K2));
  
  for (k1 in 1:K1){
    for (k2 in 1:K2){
      core[(k1-1) * K2 + k2] ~ dirichlet(rep_vector(1.0, K));
    }
  }
  
  // Likelihood using document-term matrix X
  
  for (d in 1:D) {
    vector[K] topic_probs;  // Initialize topic_probs as a vector of size K3
    
    for (k3 in 1:K) {
      real sum_terms = 0.0;  // Initialize the summation term for each k3
      
      for (k1 in 1:K1) {
        for (k2 in 1:K2) {
          sum_terms += core[(k1-1) * K2 + k2, k3] * reviewer_type[reviewer_ids[d], k1] * paper_category[paper_ids[d], k2];
        }
      }
      topic_probs[k3] = sum_terms;
    }
    // Compute the likelihood using the calculated topic_probs
    vector[V] expected_counts;
    for (v in 1:V) {
      expected_counts[v] = dot_product(to_vector(phi[, v]), topic_probs);
    }
    // Normalize expected counts to get probabilities
    //vector[V] normalized_probs = expected_counts / sum(expected_counts);
    vector[V] normalized_probs = expected_counts;
    X[d] ~ multinomial(normalized_probs);
  }
}
