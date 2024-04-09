//
data {
  int<lower = 1> N; // number of observations
  int<lower = 1, upper = 5> stage[N];
  int<lower = 1> task[N]; //task variable
  int<lower = 1> id[N]; // participant variable
  vector [N]sx;
}
parameters {
  real<lower = 0> nu;
  vector [5]b_stage;
  vector [13]b_task;
  vector [14]b_id;
  real<lower = 0>sig;

}
model {
  // priors
 b_stage ~ normal( 0 , 0.5 );
 b_task ~ normal( 0 , 0.2 );
 b_id ~ normal(0, 0.2);
 nu ~ gamma( 2 ,0.1 );
 sig ~ exponential( 1 );

  //likelihood
  for (i in 1:N) {
    sx[i] ~ student_t(nu,b_stage[stage[i]] + b_task[task[i]] + 
                      b_id[id[i]], sig);
  }
}
generated quantities{
    vector[N] log_lik;
     vector[N] mu;
    for ( i in 1:N ) {
        mu[i] = b_stage[stage[i]] + b_task[task[i]] + b_id[id[i]];
    }
    for ( i in 1:N ) log_lik[i] = student_t_lpdf( sx[i] | nu,mu[i] , sig );

}

