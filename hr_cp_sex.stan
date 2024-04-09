//
data {
  int<lower = 0> N; // number of participants
  int<lower = 1, upper = 5> stage[N]; //stage variable
  int<lower = 1, upper = 2> sex[N]; // sex variable
  vector [N]hr;
}
parameters {
  real<lower = 0> nu;
  vector [5]b;
  vector [2]b_sex;
  real<lower = 0>sig;

}
model {
  // priors
 b ~ normal( 0 , 0.4 );
 b_sex ~ normal(0, 0.4);
 nu ~ gamma( 2 ,0.1 );
 sig ~ exponential( 1 );

  //likelihood
  for (i in 1:N) {
    hr[i] ~ student_t(nu,b[stage[i]] +  b_sex[sex[i]], sig);
  }
}
generated quantities{
    vector[N] log_lik;
     vector[N] mu;
    for ( i in 1:N ) {
        mu[i] = b[stage[i]] +  b_sex[sex[i]];
    }
    for ( i in 1:N ) log_lik[i] = student_t_lpdf( hr[i] | nu,mu[i] , sig );
}
