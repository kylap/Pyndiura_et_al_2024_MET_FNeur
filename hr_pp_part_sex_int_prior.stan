data {
  int<lower = 0> N; // number of participants
  int<lower = 1> stage[N]; //stage variable
  int<lower=1> N_stages; //number of stages
  int<lower = 1> id[N];
  int<lower = 1, upper = 2> sex[N];
  vector [N]hr;
}

parameters {
  real<lower = 0> nu;
  vector [5]b_stage;
  vector [10]b_id;
  real mu_id;
  real< lower = 0> sig_id;
  vector [2]b_sex;
  matrix[N_stages, 2] b_st_sex_raw;
  real mu_st_sex;
  real <lower = 0>sig_st_sex;
  real<lower = 0>sig;
}

transformed parameters {
  matrix[N_stages, 2] b_st_sex;
  for (i in 1:N_stages) {
    for (j in 1:2) {
      b_st_sex[i,j] = mu_st_sex + sig_st_sex * b_st_sex_raw[i,j];
    }
  }
}

model {
  // priors
 b_stage ~ normal( 0 , 0.4 );
 b_id ~ normal(mu_id, sig_id);
 mu_id ~ normal(0,0.2);
 sig_id ~ exponential(1);
 b_sex ~ normal(0, 0.4);
 to_vector(b_st_sex_raw) ~ normal(0,1);
 nu ~ gamma( 2 ,0.1 );
 mu_st_sex ~ normal(0,0.2);
 sig_st_sex ~ exponential(1);
 sig ~ exponential( 1 );

  //likelihood
 // for (i in 1:N) {
   // hr[i] ~ student_t(nu,b_stage[stage[i]] + b_id[id[i]] + 
    //b_sex[sex[i]] + b_st_sex[stage[i], sex[i]], sig);
  //}
}

generated quantities{
    vector[N] log_lik;
     vector[N] mu;
    for ( i in 1:N ) {
        mu[i] = b_stage[stage[i]] + b_id[id[i]] + 
        b_sex[sex[i]] + b_st_sex[stage[i], sex[i]];
    }
    for ( i in 1:N ) log_lik[i] = student_t_lpdf( hr[i] | nu,mu[i] , sig );
}
