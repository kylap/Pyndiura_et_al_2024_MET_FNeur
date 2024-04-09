//
data {
  int<lower = 0> N; // number of participants
  int<lower = 1, upper = 5> stage[N]; //stage variable
  int<lower = 1> N_stages; // number of stages
  int<lower = 1, upper = 14> id[N]; //id variable
  int<lower = 1> N_ids; // number of ids
  vector [N]sx;
}
parameters {
  real<lower = 0> nu;
  vector[5]b_stage_raw;
  vector[14]b_id_raw;
  real mu_stage;
  real mu_id;
  real<lower = 0> sig_stage;
  real<lower = 0> sig_id; 
  real<lower = 0>sig;

}

transformed parameters {
  vector[N_stages] b_stage;
  vector[N_ids] b_id;
  for (i in 1:N_stages) {
      b_stage[i] = mu_stage + sig_stage * b_stage_raw[i];
  }
  for (i in 1:N_ids) {
      b_id[i] = mu_id + sig_id * b_id_raw[i];
  }
}

model {
  // priors
 b_stage_raw ~ normal(0,1);
 b_id_raw ~ normal(0,1);
 mu_stage ~ normal(0,0.5);
 sig_stage ~ exponential( 1 );
 mu_id ~ normal(0,0.2);
 sig_id ~ exponential( 1 );
 sig ~ exponential( 1 );
 nu ~ gamma( 2 ,0.1 );
  //likelihood
  for (i in 1:N) {
    sx[i] ~ student_t(nu,b_stage[stage[i]] + b_id[id[i]], sig);
  }
}

generated quantities{
    vector[N] log_lik;
     vector[N] mu;
    for ( i in 1:N ) {
        mu[i] = b_stage[stage[i]] +  b_id[id[i]];
    }
    for ( i in 1:N ) log_lik[i] = student_t_lpdf( sx[i] | nu,mu[i] , sig );
    
}
