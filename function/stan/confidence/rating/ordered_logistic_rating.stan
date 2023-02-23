//
// Author: Kiri Kuroda
// Ordered logistic regression predicting
// confidence rating from subjective accuracy.
// 

data {
  int<lower=0> N; // Number of observations
  int<lower=2> L; // Number of confidence levels (i.e. 6)
  int<lower=1> N_subj; // Number of subjects
  int<lower=1, upper=N_subj> id[N]; // Subject's ID
  int<lower=1, upper=L> rating[N]; // Confidence rating (1 ~ K)
  vector[N] scaled_x; // Subjective / estimated accuracy
}

parameters {
  real mean_slope;
  ordered[L-1] mean_cutoff;
  real<lower=0> sd_slope;
  vector<lower=0>[L-1] sd_cutoff;
  vector[N_subj] slope;
  ordered[L-1] cutoff[N_subj];
}

model {
  vector[N] predictor;
  vector[L-1] c[N];
  for (i in 1:N) {
    predictor[i] = scaled_x[i] * slope[id[i]];
    c[i] = cutoff[id[i]];
  }
  
  // Group-level means
  mean_slope ~ cauchy(0, 5);
  mean_cutoff ~ cauchy(0, 5);
  
  // Scaling parameters
  sd_slope ~ student_t(4, 0, 1);
  sd_cutoff ~ student_t(4, 0, 1);
  
  // Individual parameters
  slope ~ normal(mean_slope, sd_slope);
  for (i in 1:L-1) {
    cutoff[, i] ~ normal(mean_cutoff[i], sd_cutoff[i]);
  }
  
  // Compute log-likelihood
  rating ~ ordered_logistic(predictor, c);
}

generated quantities {
  // Generate posterior predictive samples and log-likelihood
  
  // Initialize the vectors
  vector[N] log_lik;
  vector[L] y_pred[N_subj];
  for (i in 1:L) {
    for (j in 1:N_subj) {
      y_pred[j, i] = 0;
    }
  }
  {
    int rating_pred[N];
    for (i in 1:N) {
      // Generate and save posterior predictive samples
      rating_pred[i] = ordered_logistic_rng(scaled_x[i] * slope[id[i]], cutoff[id[i]]);
      y_pred[id[i], rating_pred[i]] = y_pred[id[i], rating_pred[i]] + 1;
      
      // Log likelihood
      log_lik[i] = ordered_logistic_lpmf(rating[i] | scaled_x[i] * slope[id[i]], cutoff[id[i]]);
    }
  }
}
