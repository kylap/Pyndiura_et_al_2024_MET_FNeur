//
data {
  int<lower = 0> N; // number of participants
  int<lower = 1, upper = 5> stage[N]; //stage variable
  int<lower = 1> N_stages; // number of stages
  int<lower = 1, upper = 13> task[N]; //task variable
  int<lower = 1> N_tasks; // number of tasks
  int<lower = 1> sex[N];
  vector [N]sx;
}
parameters {
  real<lower = 0> nu;
  vector[5]b_stage_raw;
  real mu_stage;
  real<lower = 0> sig_stage;
  vector[13]b_task_raw;
  real mu_task;
  real<lower = 0> sig_task;
  vector[2] b_sex;
  real<lower = 0>sig;

}

transformed parameters {
  vector[N_stages] b_stage;
  vector[N_tasks] b_task;
  for (i in 1:N_stages) {
      b_stage[i] = mu_stage + sig_stage * b_stage_raw[i];
  }
  for (i in 1:N_tasks) {
      b_task[i] = mu_task + sig_task * b_task_raw[i];
  }
}

model {
  // priors
 b_stage_raw ~ normal(0,1);
 b_task_raw ~ normal(0,1);
 b_sex ~ normal(0, 0.5);
 mu_stage ~ normal(0,0.5);
 sig_stage ~ exponential( 1 );
 mu_task ~ normal(0,0.2);
 sig_task ~ exponential( 1 );
 sig ~ exponential( 1 );
 nu ~ gamma( 2 ,0.1 );
  //likelihood
  for (i in 1:N) {
    sx[i] ~ student_t(nu,b_stage[stage[i]] + b_task[task[i]] + b_sex[sex[i]], sig);
  }
}

generated quantities{
    vector[N] log_lik;
     vector[N] mu;
    for ( i in 1:N ) {
        mu[i] = b_stage[stage[i]] + b_task[task[i]] + b_sex[sex[i]];
    }
    for ( i in 1:N ) log_lik[i] = student_t_lpdf( sx[i] | nu,mu[i] , sig );
    
}
