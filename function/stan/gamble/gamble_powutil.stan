//
// Author: Kiri Kuroda
//
// Model of risky decision in the gambling task.
// Here we assume power utility function and logit choice model.
//

data {
  int<lower=0> N; // Number of observations (trials)
  int<lower=0> N_subj; // Number of participants
  int<lower=0, upper=N_subj> id[N]; // Participant's ID
  int<lower=0, upper=1> choice[N]; // 0: sure; 1: risky
  vector<lower=0>[N] reward; // scaled_reward magnitude
  vector<lower=0, upper=1>[N] prob; // scaled_reward probability
  real<lower=0> scale; // Scaling parameter of scaled_reward
}

transformed data {
  // Rescale the payoff
  vector<lower=0>[N] scaled_reward;
  real<lower=0> scaled_500;
  scaled_reward = reward / scale;
  scaled_500 = 500 / scale;
}

parameters {
  real<lower=0> mean_rho;
  real<lower=0> mean_tau;
  real<lower=0> sd_rho;
  real<lower=0> sd_tau;
  vector<lower=0>[N_subj] rho; // Individual risk preference
  vector<lower=0>[N_subj] tau; // Individual inverse temperature
}

model {
  
  // Calculate the predictor inside the logistic function
  vector[N] predictor;
  for (i in 1:N) {
    predictor[i] = tau[id[i]] * (
      pow(scaled_reward[i], rho[id[i]]) * prob[i] - pow(scaled_500, rho[id[i]])
    );
  }
  
  // Global (group) parameters
  mean_rho ~ cauchy(0, 5);
  mean_tau ~ cauchy(0, 5);
  
  // Scale parameters
  sd_rho ~ student_t(4, 0, 1);
  sd_tau ~ student_t(4, 0, 1);
  
  // Local (individual) parameters
  rho ~ normal(mean_rho, sd_rho);
  tau ~ normal(mean_tau, sd_tau);
  
  // Calculate the likelihood
  choice ~ bernoulli_logit(predictor);
}

generated quantities {
  // Generate posterior predictive data and calculate the log-likelihood.
  // y_pred: Individual choice frequency of risky option
  
  // Initialize the vector
  vector[N] log_lik;
  vector[N_subj] y_pred;
  for (i in 1:N_subj) {
    y_pred[i] = 0;
  }
  
  for (i in 1:N) {
    // Generate data
    y_pred[id[i]] = y_pred[id[i]] + 
      bernoulli_logit_rng(
        tau[id[i]] * (
          pow(scaled_reward[i], rho[id[i]]) * prob[i] -
          pow(scaled_500, rho[id[i]])
        )
      );
    // Calculate log-likelihood
    log_lik[i] = bernoulli_logit_lpmf(choice[i] | 
      tau[id[i]] * (
        pow(scaled_reward[i], rho[id[i]]) * prob[i] -
        pow(scaled_500, rho[id[i]])
      )
    );
  }
}
