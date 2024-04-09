data {
  int<lower = 0> N; // number of participants
  int<lower = 1> id[N]; //id variable
  int<lower = 1> N_ids;  //number of ids     
  int<lower = 1, upper = 2> sex[N];
  vector [N]sx;
}

parameters {
  real<lower = 0> nu;
  vector[14]b_id;
  vector[2]b_sex;
  real<lower = 0>sig;
}

model {
  // priors
 b_id ~ normal( 0, 0.5 ); 
 b_sex ~ normal(0, 0.5);
 nu ~ gamma( 2 ,0.1 );
 sig ~ exponential( 1 );

  //likelihood
  for (i in 1:N) {
    sx[i] ~ student_t(nu, b_id[id[i]] +
    b_sex[sex[i]], sig);
  }
}
generated quantities{
    vector[N] log_lik;
     vector[N] mu;
    for ( i in 1:N ) {
        mu[i] = b_id[id[i]] +
        b_sex[sex[i]];
    }
    for ( i in 1:N ) log_lik[i] = student_t_lpdf( sx[i] | nu,mu[i] , sig );
}
