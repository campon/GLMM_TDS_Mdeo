functions {
  real icar_normal_lpdf(vector phi, int N, array[] int node1,
                        array[] int node2) {
    return -0.5 * dot_self(phi[node1] - phi[node2]);
  }
}

data {
  int<lower=0> N;
  int<lower=0> N_edges;
  array[N_edges] int<lower=1, upper=N> node1; // node1[i] adjacent to node2[i]
  array[N_edges] int<lower=1, upper=N> node2; // and node1[i] < node2[i]
  
  int<lower=0, upper=1> y[N]; // Binary outcome (0 or 1)
  vector<lower=0>[N] x; // coefficient
}

parameters {
  real beta0; // intercept
  real beta1; // slope
  real<lower=0> sigma; // overall standard deviation
  vector[N] phi; // spatial effects
}

model {
  y ~ bernoulli_logit(beta0 + beta1*x + phi * sigma);
  beta0 ~ normal(0.0, 1.0);
  beta1 ~ normal(0.0, 1.0);
  sigma ~ normal(0.0, 1.0);
  phi ~ icar_normal(N, node1, node2);
  // soft sum-to-zero constraint on phi
  // more efficient than mean(phi) ~ normal(0, 0.001)
  sum(phi) ~ normal(0, 0.001 * N);
}

generated quantities {
  vector[N] eta = beta0 + beta1*x + phi * sigma;
  vector[N] p = inv_logit(eta); // Probability
}
