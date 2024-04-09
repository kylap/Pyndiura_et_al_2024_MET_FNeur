data {
  int<lower = 0> N; // number of participants
  int<lower = 1, upper = 5> stage[N]; // stage variable
  int<lower = 1> id[N]; // id variable
  vector[N] hr;
}
parameters {
  real<lower = 0> nu;
  vector[5] stage_raw; // non-centered parameterization for stage 
  real mu_stage;
  real<lower = 0> sig_stage;
  vector [10]b_id;
  real mu_id;
  real< lower = 0> sig_id;
  real<lower = 0> sig;
}
transformed parameters {
  vector[5] b_stage;
  for (i in 1:5){
    b_stage[i] = mu_stage + sig_stage* stage_raw[i];
  }
}
model {
  // priors
  stage_raw ~ normal(0, 1);
  mu_stage ~ normal(0, 0.3);
  sig_stage ~ exponential(1);
  b_id ~ normal(mu_id, sig_id);
  mu_id ~ normal(0,0.2);
  sig_id ~ exponential(1);
  nu ~ gamma(2, 0.1);
  sig ~ exponential(1);

  //likelihood
  for (i in 1:N) {
    hr[i] ~ student_t(nu,b_stage[stage[i]] + b_id[id[i]], sig);
  }
}
generated quantities {
  vector[N] log_lik;
  vector[N] mu;
  for (i in 1:N) {
    mu[i] = b_stage[stage[i]] + b_id[id[i]];
  }
  for (i in 1:N) {
    log_lik[i] = student_t_lpdf(hr[i] | nu, mu[i], sig);
  }
}
