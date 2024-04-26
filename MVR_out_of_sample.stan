data {
  int<lower=1> K;  // num dependent variables - settings 
  int<lower=1> J;  // num independent variables - MCA factor dimensions
  int<lower=1> n_train; // total number of observations
  int<lower=1> n_pred;  // total number of predictions
  int<lower=0> I;       // number of pairs of age groups
  matrix[n_train,J] x_train;  // MCA factor dimensions
  matrix[n_pred,J] x_pred;
  vector[K] beta_train[n_train];
  vector[I] beta0_train;  
  matrix[I, K] X;   // Polymod
}

parameters {
  cholesky_factor_corr[K] L_Omega;// For the correlations between outcome variables
  vector<lower=0>[K] L_sigma;     //Residual SD of outcome variables
  vector[K] delta0;     //intercepts
  vector[J] delta[K];   //slopes
}

transformed parameters {
  vector[K] mu[n_train];
  matrix[K, K] L_Sigma;
  
  L_Sigma = diag_pre_multiply(L_sigma, L_Omega);
  
  for (i in 1:n_train){
    for (k in 1:K){
      mu[i, k] = delta0[k] + dot_product(to_vector(delta[k, 1:J]), x_train[i, 1:J]);
    }
  }
}

model {
  delta0 ~ normal(0, 1);
  for (k in 1:K){
    to_vector(delta[k,]) ~ normal(0, 1);
  }
  L_Omega ~ lkj_corr_cholesky(0.5);
  L_sigma ~ cauchy(0,5);
  
  for (i in 1:n_train){
    beta_train[i, 1:K] ~ multi_normal_cholesky(mu[i, 1:K], L_Sigma);
  }
}

generated quantities {
  real dev;                   // deviance
  vector[n_train] log_lik;    // log-likelihood for loo package
  matrix[K,K] Omega;
  matrix[K,K] Sigma;
  vector[K] mu_pred[n_pred];
  vector[K] beta_pred[n_pred];
  matrix[n_pred,I] C_pred;
  
  Omega = multiply_lower_tri_self_transpose(L_Omega);
  Sigma = quad_form_diag(Omega, L_sigma);
  
  for (i in 1:n_pred){
    for (k in 1:K){
      mu_pred[i, k] = delta0[k] + dot_product(to_vector(delta[k, 1:J]), x_pred[i, 1:J]);
    }
  }
  
  beta_pred = multi_normal_cholesky_rng(mu_pred, L_Sigma);
  
  for (i in 1:I){
    for (t in 1:n_pred) {
      C_pred[t,i] = beta0_train[i]+beta_pred[t,1]*X[i,1]+beta_pred[t,2]*X[i,2]+beta_pred[t,3]*X[i,3]+beta_pred[t,4]*X[i,4];
    }
  }
  
  for (i in 1:n_train) {
    log_lik[i] = multi_normal_cholesky_lpdf(beta_train[i]| mu[i], L_Sigma);
  }
  dev = (-2) * sum(log_lik);
}

