data {
  int<lower=1> K;  // num dependent variables
  int<lower=1> J;  // num independent variables
  int<lower=1> n_obs; //Total number of observations
  matrix[n_obs,J] x;
  vector[K] y[n_obs];
  //vector[J] b0;
  //matrix[J,J] inv_XX;
}

parameters {
  cholesky_factor_corr[K] L_Omega; //For the correlations between outcome variables
  vector<lower=0>[K] L_sigma; //Residual SD of outcome variables
  vector[K] delta0;     //intercepts
  vector[J] delta[K];   //slopes
}

transformed parameters {
  vector[K] mu[n_obs];
  matrix[K, K] L_Sigma;
  
  L_Sigma = diag_pre_multiply(L_sigma, L_Omega);
  
  for (i in 1:n_obs){
    for (k in 1:K){
    mu[i, k] = delta0[k] + dot_product(to_vector(delta[k, 1:J]), x[i, 1:J]);
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
  
  for (i in 1:n_obs){
        y[i, 1:K] ~ multi_normal_cholesky(mu[i, 1:K], L_Sigma);
    }
}

generated quantities {
  real dev;                    // deviance
  vector[n_obs] log_lik;       // log-likelihood for loo package
  matrix[K,K] Omega;
  matrix[K,K] Sigma;
  vector[K] y_pred[n_obs];
  
  Omega = multiply_lower_tri_self_transpose(L_Omega);
  Sigma = quad_form_diag(Omega, L_sigma);
  
  y_pred = multi_normal_cholesky_rng(mu, L_Sigma);
  for (i in 1:n_obs) {
    log_lik[i] = multi_normal_cholesky_lpdf(y[i]| mu[i], L_Sigma);
 }
    dev = (-2) * sum(log_lik);
}

