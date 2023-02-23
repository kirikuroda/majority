//
// Author: Kiri Kuroda
// Stochastic updating model (see Navajas et al., 2017 for details)
//

data {
  int<lower=0> N; // Number of trials
  int<lower=0> N_subj; // Number of subjects
  int<lower=0, upper=N_subj> id[N]; // Subject's id
  int<lower=0, upper=1> judge[N]; // 0: anticlockwise; 1: clockwise
  int<lower=0, upper=1> answer[N]; // 0: anticlockwise; 1: clockwise
  vector<lower=0>[N] theta_var; // Variance of orientations
  matrix[30, N] theta; // Orientations of stimuli
}

parameters {
  real<lower=0> mean_epsilon;
  real<lower=0> mean_lambda;
  real<lower=0> sd_epsilon;
  real<lower=0> sd_lambda;
  vector<lower=0>[N_subj] epsilon; // Individual noise parameter
  vector<lower=0>[N_subj] lambda; // Individual relative weight for past information
}

model {
  matrix[30, N] mu_term;
  matrix[30, N] sigma_term;
  vector[N] z;
  
  // Calculate mu, sigma, and z for each trial
  for (obs_i in 1:N) {
    for (theta_i in 1:30) {
      mu_term[theta_i, obs_i] = pow(lambda[id[obs_i]], 30 - theta_i) * theta[theta_i, obs_i];
      sigma_term[theta_i, obs_i] = pow(lambda[id[obs_i]], 2 * (30 - theta_i)) * 
        pow(theta[theta_i, obs_i], 2);
    }
    z[obs_i] = sum(mu_term[, obs_i]) / 
      (epsilon[id[obs_i]] * sqrt(sum(sigma_term[, obs_i])));
  }
    
  // Global (group) parameters
  mean_epsilon ~ cauchy(0, 5);
  mean_lambda ~ cauchy(0, 5);
  
  // Scale parameters
  sd_epsilon ~ student_t(4, 0, 1);
  sd_lambda ~ student_t(4, 0, 1);
  
  // Individual (local) parameters
  epsilon ~ normal(mean_epsilon, sd_epsilon);
  lambda ~ normal(mean_lambda, sd_lambda);
  
  // Calculate the cumulative probability from mu and sigma and compute the likelihood
  judge ~ bernoulli(Phi_approx(z));
}

generated quantities {
  // Generate posterior predictive data
  // And compute the log-likelihood
  vector[4] y_pred[N_subj];
  
  {
    // The following data is not going to be added to the output
    matrix[30, N] mu_term;
    matrix[30, N] sigma_term;
    vector[N] z;
    vector[N] p;
    vector[N] resp;
    vector[N] correct;
    
    // Initialize the vectors
    for (i in 1:4) {
      for (j in 1:N_subj) {
        y_pred[j, i] = 0;
      }
    }
    
    // Generate data and compute log-likelihood
    for (obs_i in 1:N) {
      
      // Calculate mu, sigma, and z
      for (theta_i in 1:30) {
        mu_term[theta_i, obs_i] = 
          pow(lambda[id[obs_i]], 30 - theta_i) * 
          theta[theta_i, obs_i];
        sigma_term[theta_i, obs_i] = 
          pow(lambda[id[obs_i]], 2 * (30 - theta_i)) * 
          pow(theta[theta_i, obs_i], 2);
      }
      z[obs_i] = sum(mu_term[, obs_i]) / 
        (epsilon[id[obs_i]] * sqrt(sum(sigma_term[, obs_i])));
      p[obs_i] = Phi_approx(z[obs_i]);
      
      // Generate data
      resp[obs_i] = bernoulli_rng(p[obs_i]);
      if (resp[obs_i] == answer[obs_i]) {
        correct[obs_i] = 1;
      } else {
        correct[obs_i] = 0;
      }
      
      // Save data
      if (theta_var[obs_i] == 8) {
        y_pred[id[obs_i], 1] = y_pred[id[obs_i], 1] + correct[obs_i];
      } else if (theta_var[obs_i] == 16) {
        y_pred[id[obs_i], 2] = y_pred[id[obs_i], 2] + correct[obs_i];
      } else if (theta_var[obs_i] == 32) {
        y_pred[id[obs_i], 3] = y_pred[id[obs_i], 3] + correct[obs_i];
      } else {
        y_pred[id[obs_i], 4] = y_pred[id[obs_i], 4] + correct[obs_i];
      }
    }
  }
}
