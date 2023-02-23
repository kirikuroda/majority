//
// Author: Kiri Kuroda
// Logistic regression predicting the voting rate
// from the task and individual parameters.
// 

data {
  int<lower=0> N; // Number of observations (trials)
  int<lower=0> N_subj; // Number of participants
  int<lower=0, upper=N_subj> id[N]; // Participant's ID
  int<lower=0, upper=1> choice[N]; // 0: group; 1: exit
  vector[N] scaled_reward; // Normalized reward
  vector[N] scaled_var; // Normalized variance of orientations
  vector<lower=0>[N] theta_var; // Unnormalized (i.e. raw) variance of theta
  vector[N] scaled_rho; // Normalized risk preference
  vector[N] scaled_gamma; // Normalized competence
  vector[N] scaled_lambda; // Normalized lambda
  vector[N] scaled_confidence; // Normalized confidence
}

parameters {
  real coef_rho;
  real coef_gamma;
  real coef_lambda;
  real coef_confidence;
  real mean_intercept;
  real mean_coef_reward;
  real mean_coef_var;
  real<lower=0> sd_intercept;
  real<lower=0> sd_coef_reward;
  real<lower=0> sd_coef_var;
  vector[N_subj] intercept;
  vector[N_subj] coef_reward;
  vector[N_subj] coef_var;
}

model {
  vector[N] predictor;
  for (i in 1:N) {
    predictor[i] = intercept[id[i]] + coef_reward[id[i]] * scaled_reward[i] +
      coef_var[id[i]] * scaled_var[i] + coef_rho * scaled_rho[i] +
      coef_gamma * scaled_gamma[i] + coef_lambda * scaled_lambda[i] + 
      coef_confidence * scaled_confidence[i];
  }
  
  // Coefficients on the individual parameters
  coef_rho ~ cauchy(0, 5);
  coef_gamma ~ cauchy(0, 5);
  coef_lambda ~ cauchy(0, 5);
  coef_confidence ~ cauchy(0, 5);
  
  // Group-level means
  mean_intercept ~ cauchy(0, 5);
  mean_coef_reward ~ cauchy(0, 5);
  mean_coef_var ~ cauchy(0, 5);
  
  // Scaling parameter
  sd_intercept ~ student_t(4, 0, 1);
  sd_coef_reward ~ student_t(4, 0, 1);
  sd_coef_var ~ student_t(4, 0, 1);
  
  // Individual parameters
  intercept ~ normal(mean_intercept, sd_intercept);
  coef_reward ~ normal(mean_coef_reward, sd_coef_reward);
  coef_var ~ normal(mean_coef_var, sd_coef_var);
  
  choice ~ bernoulli_logit(predictor);
}

generated quantities {
  // Generate posterior predictive data and calculate the log likelihood
  // y_pred: Individual choice frequency of the exit option

  // Initialize the vector
  vector[4] y_pred[N_subj];
  
  {
    // The following data is not going to be added to the output
    vector[N] predictor;
    vector[N] exit_choice;
    
    // Initialize the vector
    for (i in 1:4) {
      for (j in 1:N_subj) {
        y_pred[j, i] = 0;
      }
    }
    
    for (i in 1:N) {
      // Generate data
      predictor[i] = intercept[id[i]] + coef_reward[id[i]] * scaled_reward[i] +
        coef_var[id[i]] * scaled_var[i] + coef_rho * scaled_rho[i] +
        coef_gamma * scaled_gamma[i] + coef_lambda * scaled_lambda[i] + 
        coef_confidence * scaled_confidence[i];
      exit_choice[i] = bernoulli_logit_rng(predictor[i]);
      
      // Save data
      if (theta_var[i] == 8) {
        y_pred[id[i], 1] = y_pred[id[i], 1] + exit_choice[i];
      } else if (theta_var[i] == 16) {
        y_pred[id[i], 2] = y_pred[id[i], 2] + exit_choice[i];
      } else if (theta_var[i] == 32) {
        y_pred[id[i], 3] = y_pred[id[i], 3] + exit_choice[i];
      } else {
        y_pred[id[i], 4] = y_pred[id[i], 4] + exit_choice[i];
      }
    }
  }
}
