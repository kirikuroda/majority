//
// Author: Kiri Kuroda
// Subjective accuracy in each trial (Wu-Gonzalez function)
//

data {
  int<lower=0> N; // Number of observations
  int<lower=0, upper=1> choice[N]; // 0: sure; 1: risky
  vector<lower=0>[N] reward; // Reward magnitude of the risky option
  vector<lower=0, upper=1>[N] p_hat; // Calculated p-hat
  real<lower=0> rho; // Individual risk preferences
  vector<lower=0>[N] theta_var; // Variance of orientations
  real<lower=0> scale; // Scaling parameter of payoff
}

transformed data {
  // Scale the payoff
  vector<lower=0>[N] scaled_reward;
  real<lower=0> scaled_500;
  scaled_reward = reward / scale;
  scaled_500 = 500 / scale;
}

parameters {
  real<lower=0> tau;
  real<lower=0> alpha;
  real<lower=0> beta;
}

model {
  vector[N] q;
  vector[N] predictor;
  
  for (i in 1:N) {
    q[i] = pow(p_hat[i], alpha) / pow(pow(p_hat[i], alpha) + pow(1 - p_hat[i], alpha), beta);
    predictor[i] = tau * (pow(scaled_reward[i], rho) * q[i] - pow(scaled_500, rho));
  }
  
  // Compute likelihood
  choice ~ bernoulli_logit(predictor);
}
