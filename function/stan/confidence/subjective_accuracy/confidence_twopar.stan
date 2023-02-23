//
// Author: Kiri Kuroda
// Subjective accuracy in each trial (Prelec 2-parameter function)
//

data {
  int<lower=0> N; // Number of observations
  int<lower=0> N_subj; // Number of subjects
  int<lower=0, upper=N_subj> id[N]; // Participant's ID
  int<lower=0, upper=1> choice[N]; // 0: sure; 1: risky
  vector<lower=0>[N] reward; // Reward magnitude of the risky option
  vector<lower=0, upper=1>[N] p_hat; // Calculated p-hat
  vector<lower=0>[N_subj] rho; // Individual risk preferences
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
  real<lower=0> mean_alpha;
  real<lower=0> mean_beta;
  real<lower=0> mean_tau;
  real<lower=0> sd_alpha;
  real<lower=0> sd_beta;
  real<lower=0> sd_tau;
  vector<lower=0>[N_subj] alpha;
  vector<lower=0>[N_subj] beta;
  vector<lower=0>[N_subj] tau;
}

transformed parameters {
  // Prelec 2-parameter function
  // q: Subjective accuracy
  vector[N] q;
  for (i in 1:N) {
    q[i] = exp(-beta[id[i]] * pow(-log(p_hat[i]), alpha[id[i]]));
  }
}

model {
  vector[N] predictor;
  for (i in 1:N) {
    predictor[i] = tau[id[i]] * 
      (pow(scaled_reward[i], rho[id[i]]) * q[i] - pow(scaled_500, rho[id[i]]));
  }
  
  mean_alpha ~ cauchy(0, 5);
  mean_beta ~ cauchy(0, 5);
  mean_tau ~ cauchy(0, 5);
  sd_alpha ~ student_t(4, 0, 1);
  sd_beta ~ student_t(4, 0, 1);
  sd_tau ~ student_t(4, 0, 1);
  alpha ~ normal(mean_alpha, sd_alpha);
  beta ~ normal(mean_beta, sd_beta);
  tau ~ normal(mean_tau, sd_tau);
  
  // Compute likelihood
  choice ~ bernoulli_logit(predictor);
}

generated quantities {
  // Generate posterior predictive data
  // and calculate log-likelihood
  // y_pred: Individual choice frequency of the risky option

  // Initialize the vector
  vector[N] log_lik;
  vector[4] y_pred[N_subj];
  {
    // The following data is not going to be added to the output
    vector[N] predictor;
    vector[N] risky_choice;
    
    // Initialize the vector
    for (i in 1:4) {
      for (j in 1:N_subj) {
        y_pred[j, i] = 0;
      }
    }
    
    for (i in 1:N) {
      // Generate data
      predictor[i] = tau[id[i]] * (pow(scaled_reward[i], rho[id[i]]) * q[i] -
        pow(scaled_500, rho[id[i]]));
      risky_choice[i] = bernoulli_logit_rng(predictor[i]);
      
      // Save data
      if (theta_var[i] == 8) {
        y_pred[id[i], 1] = y_pred[id[i], 1] + risky_choice[i];
      } else if (theta_var[i] == 16) {
        y_pred[id[i], 2] = y_pred[id[i], 2] + risky_choice[i];
      } else if (theta_var[i] == 32) {
        y_pred[id[i], 3] = y_pred[id[i], 3] + risky_choice[i];
      } else {
        y_pred[id[i], 4] = y_pred[id[i], 4] + risky_choice[i];
      }
      
      // Log likelihood
      log_lik[i] = bernoulli_logit_lpmf(choice[i] | 
        tau[id[i]] * (
          pow(scaled_reward[i], rho[id[i]]) * q[i] -
          pow(scaled_500, rho[id[i]])
        )
      );
    }
  }
}
