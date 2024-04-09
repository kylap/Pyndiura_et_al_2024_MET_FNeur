data {
  int<lower = 0> N; // number of participants
  int<lower = 1, upper = 5> stage[N]; // stage variable
  vector[N] hr;
}
parameters {
  real<lower = 0> nu;
  vector[5] b_raw;
  real mu_b;
  real<lower = 0> sig_b;
  real<lower = 0> sig;
}
transformed parameters {
  vector[5] b;
  for (i in 1:5){
    b[i] = mu_b + sig_b * b_raw[i];
  }
}
model {
  // priors
  b_raw ~ normal(0, 1);
  mu_b ~ normal(0, 0.3);
  sig_b ~ exponential(1);
  nu ~ gamma(2, 0.1);
  sig ~ exponential(1);

  //likelihood
  for (i in 1:N) {
    hr[i] ~ student_t(nu,b[stage[i]], sig);
  }
}
generated quantities {
  vector[N] log_lik;
  vector[N] mu;
  for (i in 1:N) {
    mu[i] = b[stage[i]];
  }
  for (i in 1:N) {
    log_lik[i] = student_t_lpdf(hr[i] | nu, mu[i], sig);
  }
}
