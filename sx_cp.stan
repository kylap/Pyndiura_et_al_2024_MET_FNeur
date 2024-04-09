//
data {
  int<lower = 0> N; // number of participants
  int<lower = 1, upper = 5> stage[N]; //stage variable
  vector [N]sx;
}
parameters {
  real<lower = 0> nu;
  vector [5]b;
  real<lower = 0>sig;

}
model {
  // priors
 b ~ normal( 0 , 0.5 );
 nu ~ gamma( 2 ,0.1 );
 sig ~ exponential( 1 );

  //likelihood
  for (i in 1:N) {
    sx[i] ~ student_t(nu,b[stage[i]], sig);
  }
}
generated quantities{
    vector[N] log_lik;
     vector[N] mu;
    for ( i in 1:N ) {
        mu[i] = b[stage[i]];
    }
    for ( i in 1:N ) log_lik[i] = student_t_lpdf( sx[i] | nu,mu[i] , sig );
    
}
