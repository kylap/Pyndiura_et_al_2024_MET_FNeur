data {
  int<lower = 0> N; // number of participants
  int<lower = 1> stage[N]; //stage variable
  int<lower=1> N_stages; //number of stages
  int<lower = 1, upper = 2> sex[N];
  vector [N]sx;
}

parameters {
  real<lower = 0> nu;
  vector [5]b_stage_raw;
  matrix[N_stages, 2] b_st_sex_raw;
  real mu_stage;
  real mu_st_sex;
  real<lower = 0> sig_stage;
  real<lower = 0> sig_st_sex;
  vector [2]b_sex;
  real<lower = 0>sig;
}

transformed parameters {
  vector[N_stages] b_stage;
  matrix[N_stages, 2] b_st_sex;
  
  for (i in 1:N_stages) {
    for (j in 1:2) {
      b_st_sex[i,j] = mu_st_sex + sig_st_sex * b_st_sex_raw[i,j];
    }
  }
  for (i in 1:N_stages){
    b_stage[i] = mu_stage + sig_stage * b_stage_raw[i];
  }
}


model {
  // priors
 b_stage_raw ~ normal( 0 , 1 );
 to_vector(b_st_sex_raw) ~ normal(0,1);
 mu_stage ~ normal(0,0.2);
 sig_stage ~ exponential(1);
 mu_st_sex ~ normal(0,0.2);
 sig_st_sex ~ exponential(1);
 b_sex ~ normal(0, 0.4);
 nu ~ gamma( 2 ,0.1 );
 sig ~ exponential( 1 );

  //likelihood
  for (i in 1:N) {
    sx[i] ~ student_t(nu,b_stage[stage[i]] + 
    b_sex[sex[i]] + b_st_sex[stage[i], sex[i]], sig);
  }
}
generated quantities{
    vector[N] log_lik;
     vector[N] mu;
    for ( i in 1:N ) {
        mu[i] = b_stage[stage[i]] + 
        b_sex[sex[i]] + b_st_sex[stage[i], sex[i]];
    }
    for ( i in 1:N ) log_lik[i] = student_t_lpdf( sx[i] | nu,mu[i] , sig );
}
