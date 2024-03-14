functions {
  vector build_b_spline(real[] t, real[] ext_knots, int ind, int order);
  vector build_b_spline(real[] t, real[] ext_knots, int ind, int order) {
    // INPUTS:
      //    t:          the points at which the b_spline is calculated
    //    ext_knots:  the set of extended knots
    //    ind:        the index of the b_spline
    //    order:      the order of the b-spline
    vector[size(t)] b_spline;
    vector[size(t)] w1 = rep_vector(0, size(t));
    vector[size(t)] w2 = rep_vector(0, size(t));
    if (order==1)
      for (i in 1:size(t)) // B-splines of order 1 are piece-wise constant
    b_spline[i] = (ext_knots[ind] <= t[i]) && (t[i] < ext_knots[ind+1]);
    else {
      if (ext_knots[ind] != ext_knots[ind+order-1])
        w1 = (to_vector(t) - rep_vector(ext_knots[ind], size(t))) /
          (ext_knots[ind+order-1] - ext_knots[ind]);
      if (ext_knots[ind+1] != ext_knots[ind+order])
        w2 = 1 - (to_vector(t) - rep_vector(ext_knots[ind+1], size(t))) /
          (ext_knots[ind+order] - ext_knots[ind+1]);
      // Calculating the B-spline recursively as linear interpolation of two lower-order splines
      b_spline = w1 .* build_b_spline(t, ext_knots, ind, order-1) +
        w2 .* build_b_spline(t, ext_knots, ind+1, order-1);
    }
    return b_spline;
  }
}
data {
  //int<lower=0> N;   // number of data points I*T
  int<lower=0> I;   // number of pairs of age groups
  int<lower=0> T;   // number of Comix survey waves
  int<lower=0> K;   // number of covariates
  int<lower=1> num_knots;      // num of knots
  int<lower=0> spline_degree;  // the degree of spline (is equal to order-1)
  //int<lower = 1, upper = I> pair[N];
  //int<lower = 1, upper = T> time[N];
  real<lower = 1, upper = T> time[T];
  vector[num_knots] knots;  // the sequence of knots
  //vector[N] Y;
  //vector[I] X;        // In our case X is 1 observation in time
  matrix[I, K] X;        // Covariate matrix
  matrix[T,I] Y;
  real aa;
  real tt;
  real ss;
}
transformed data {
  int num_basis = num_knots + spline_degree - 1; // total number of B-splines
  matrix[num_basis, T] B;  // matrix of B-splines
  vector[spline_degree + num_knots] ext_knots_temp;
  vector[2*spline_degree + num_knots] ext_knots; // set of extended knots
  ext_knots_temp = append_row(rep_vector(knots[1], spline_degree), knots);
  ext_knots = append_row(ext_knots_temp, rep_vector(knots[num_knots], spline_degree));
  for (ind in 1:num_basis)
    B[ind,:] = to_row_vector(build_b_spline(time, to_array_1d(ext_knots), ind, spline_degree + 1));
  B[num_knots + spline_degree - 1, T] = 1;
}
parameters {
  row_vector[num_basis] a_raw1;
  row_vector[num_basis] a_raw2;
  row_vector[num_basis] a_raw3;
  row_vector[num_basis] a_raw4;
  real<lower=0> sigma;
  real<lower=0> tau1;
  real<lower=0> tau2;
  real<lower=0> tau3;
  real<lower=0> tau4;
}
transformed parameters {
  row_vector[num_basis] a1; // spline coefficients
  row_vector[num_basis] a2;
  row_vector[num_basis] a3;
  row_vector[num_basis] a4;
  vector[T] beta1;
  vector[T] beta2;
  vector[T] beta3;
  vector[T] beta4;
  
  a1[1] = a_raw1[1];
  a2[1] = a_raw2[1];
  a3[1] = a_raw3[1];
  a4[1] = a_raw4[1];
  for (i in 2:num_basis){
    a1[i] = a1[i-1] + a_raw1[i]*tau1;
    a2[i] = a2[i-1] + a_raw2[i]*tau2;
    a3[i] = a3[i-1] + a_raw3[i]*tau3;
    a4[i] = a4[i-1] + a_raw4[i]*tau4;
  }
  
  beta1 = to_vector(a1*B);
  beta2 = to_vector(a2*B);
  beta3 = to_vector(a3*B);
  beta4 = to_vector(a4*B);
}
model {
  // Priors
  a_raw1 ~ normal(0, aa);
  a_raw2 ~ normal(0, aa);
  a_raw3 ~ normal(0, aa);
  a_raw4 ~ normal(0, aa);
  //tau1 ~ normal(0, 1);
  //tau2 ~ normal(0, 1);
  //tau3 ~ normal(0,1);
  //tau4 ~ normal(0, 1);
  //sigma ~ normal(0, 1);
  
  tau1 ~ cauchy(0,tt);
  tau2 ~ cauchy(0,tt);
  tau3 ~ cauchy(0,tt);
  tau4 ~ cauchy(0,tt);
  sigma ~ cauchy(0,ss);
  
  //Likelihood
  for (i in 1:I){
    for (t in 1:T){
      Y[t,i] ~ normal(beta1[t]*X[i,1]+beta2[t]*X[i,2]+beta3[t]*X[i,3]+beta4[t]*X[i,4], sigma);
    }
  }
}
generated quantities {
  matrix[T,I] y_pred;
  matrix[T,I] log_lik;       // log-likelihood for loo package
  real dev;                  // deviance
  
  for (i in 1:I){
    for (t in 1:T) {
      y_pred[t,i] = normal_rng(beta1[t]*X[i,1]+beta2[t]*X[i,2]+beta3[t]*X[i,3]+beta4[t]*X[i,4], sigma);
      log_lik[t,i] = normal_lpdf(Y[t,i]| beta1[t]*X[i,1]+beta2[t]*X[i,2]+beta3[t]*X[i,3]+beta4[t]*X[i,4], sigma);
    }
  }
  dev = (-2) * sum(log_lik);
} 
