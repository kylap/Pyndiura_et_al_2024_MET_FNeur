//
data {
  int<lower = 0> N; // number of participants
  int<lower = 1, upper = 5> stage[N]; //stage variable
  int<lower=1> N_stages; //number of stages
  int<lower = 1, upper = 10> id[N];
  int<lower = 1, upper = 2> sex[N];
  vector [N]hr;
}
parameters {
  real<lower = 0> nu;
  vector [5]b;
  vector [10]b_id;
  vector [2]b_sex;
  matrix[N_stages, 2] b_st_sex;
  real<lower = 0>sig;
}

model {
  // priors
 b ~ normal( 0 , 0.4 );
 b_id ~ normal(0, 0.2);
 b_sex ~ normal(0, 0.4);
 to_vector(b_st_sex) ~ normal(0,0.2);
 nu ~ gamma( 2 ,0.1 );
 sig ~ exponential( 1 );

  //likelihood
  for (i in 1:N) {
    hr[i] ~ student_t(nu,b[stage[i]] + b_id[id[i]] + 
    b_sex[sex[i]] + b_st_sex[stage[i], sex[i]], sig);
  }
}
generated quantities{
    vector[N] log_lik;
     vector[N] mu;
    for ( i in 1:N ) {
        mu[i] = b[stage[i]] + b_id[id[i]] + 
        b_sex[sex[i]] + b_st_sex[stage[i], sex[i]];
    }
    for ( i in 1:N ) log_lik[i] = student_t_lpdf( hr[i] | nu,mu[i] , sig );
}
