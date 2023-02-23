# Author: Kiri Kuroda
# User-defined functions

# General settings ----------

# Options
options(mc.cores = parallel::detectCores())

# knitr
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  tidy = TRUE
)

# ggplot2 setting
theme_set(
  theme_minimal(base_size = 14, base_family = "Helvetica") +
    theme(
      axis.text = element_text(color = "black"),
      panel.grid.minor = element_blank()
    )
)

# Theme when facetting
theme_facet <- 
  theme_bw(base_size = 14, base_family = "Helvetica") +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_text(color = "black"),
    panel.border = element_rect(color = NA),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(color = NA),
    strip.text = element_text(margin = margin(3, 3, 3, 3))
  )

# Calculate standard error
se <- function(x) {
  sd(x) / sqrt(length(x))
}

# WORKAROUND: https://github.com/rstudio/rstudio/issues/6692
# Revert to 'sequential' setup of PSOCK cluster in RStudio Console on macOS and R 4.0.0.
# See https://discourse.mc-stan.org/t/r-4-0-0-and-cran-macos-binaries/13989/18
if (Sys.getenv("RSTUDIO") == "1" & !nzchar(Sys.getenv("RSTUDIO_TERM")) &
    Sys.info()["sysname"] == "Darwin" && getRversion() == "4.0.0") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}


# 00_merge.Rmd ----------

# Bind the individual files into one csv file
bind_rawdata <- function(task) {
  fs::dir_create("../data/preprocessed")
  for (i in task) {
    map_dfr(fs::dir_ls(str_c("../data/", i), glob = "*.csv"), read_csv) %>%
      arrange(id) %>%
      write_csv(str_c("../data/preprocessed/", i, ".csv"))
  }
}


# 01_gamble.Rmd ----------

# Count the individual choice frequency of risky option
# and arrange the data frame
count_n_risky <- function(data) {
  data %>%
    filter(prob < 1) %>%
    group_by(id) %>%
    summarise(n_risky = sum(choice == "risky")) %>%
    arrange(n_risky) %>%
    mutate(yaxis_num = row_number())
}

# Convert cmdstanr object to rstanfit
cmdstan2rstan <- function(fit) {
  fit$output_files() %>%
    rstan::read_stan_csv()
}

# Mutate y axis number
mutate_yaxis_num <- function(data, column) {
  data %>%
    arrange({{ column }}) %>%
    mutate(yaxis_num = row_number()) %>%
    select(!{{ column }})
}


# 02_solo.Rmd ----------

# Remove the trials where participants chose
# whether to vote or not before the stimulus presentation.
remove_order_pre <- function(data) {
  data %>%
    filter(order == "post" | is.na(order)) %>%
    mutate(
      ori_var = factor(ori_var),
      block = fct_relevel(block, c("solo", "group")),
      block = fct_recode(block, Solo = "solo", Group = "group")
    )
}

# Simulation of the stochastic updating model
calc_mu <- function(theta, lambda, epsilon, id) {
  n_theta <- length(theta)
  mu_samples <- numeric(n_theta)
  noise <- rnorm(n_theta)
  for (i in 1:n_theta) {
    if (i == 1) {
      mu <- 0
    } else {
      mu <- lambda * mu + theta[i] + epsilon * theta[i] * noise[i]
    }
    mu_samples[i] <- mu
  }
  
  #Output
  list(sim_id = id, order = 0:(n_theta - 1), mu = mu_samples)
}

sim_updating_model <- function(n_theta = 30, min_theta = -13, max_theta = 19,
                               epsilon, lambda, n_sim = 1000, seed = 1) {
  set.seed(seed)
  theta <- runif(n = n_theta + 1, min = min_theta, max = max_theta)
  1:n_sim %>%
    future_imap_dfr(~calc_mu(
      theta = theta, lambda = lambda, epsilon = epsilon, id = .y
    ),
    .options = furrr_options(seed = seed)
    )
}

# Calculate stats of the stochastic updating model
stat_updating_model <- function(theta = NULL, n_theta = 30, min_theta = -13,
                                max_theta = 19, epsilon, lambda, seed = 1) {
  if (is.null(theta)) {
    set.seed(seed)
    theta <- runif(n = n_theta, min = min_theta, max = max_theta)
  }
  mu <- theta %>%
    imap_dbl(function(.x, i = .y) {
      (lambda ^ (30 - i)) * .x[1]
    }) %>%
    sum()
  sigma <- theta %>%
    imap_dbl(function(.x, i = .y) {
      (lambda ^ (2 * (30 - i))) * (.x[1] ^ 2)
    }) %>%
    sum() %>%
    sqrt() * epsilon
  phi <- pnorm(mu / sigma)
  
  # Output
  list(mu = mu, sigma = sigma, phi = phi)
}


# 03_confidence.Rmd ----------

# Calculate Z (normalization constant)
calc_z_const <- function(mu_bar, sigma, judge) {
  pnorm(mu_bar * judge / sigma)
}

# Terms inside integral
p_hat_integrand <- function(mu_30, mu_bar, sigma, judge) {
  dnorm(mu_30, mean = mu_bar, sd = sigma) * 
    as.numeric(mu_30 * judge > 0) * 
    pnorm(abs(mu_30) / sigma, mean = 0, sd = 1)
}

# optimization for each participant
optimize_confidence_function <- function(data, model, n_iter = 10, seed = 1, ...) {
  datalist <- data %$%
    list(
      N = nrow(.),
      choice = as.numeric(choice == "risky"),
      reward = payoff_risky + payoff_jitter,
      p_hat = p_hat,
      rho = rho[1],
      theta_var = ori_var,
      scale = 1000
    )
  fits <- list()
  lls <- numeric(n_iter)
  for (i in 1:n_iter) {
    fit <- model$optimize(datalist, ...)
    e <- try(fit$lp())
    if (class(e) == "try-error") {
      lls[i] <- NA_real_
      fits <- append(fits, NA)
      message("Error!")
    } else {
      lls[i] <- fit$lp()
      fits <- append(fits, fit)
    }
  }
  fits[[which.max(lls)]]
}

# Compute LOO
my_loo <- function(..., method = "loo") {
  models <- c(...)
  n_model <- length(models)
  list_model <- list()
  
  for (i in 1:n_model) {
    list_model[str_c("model_", i)] <- list(models[i])
  }
  
  loos <- list_model %>%
    as_tibble() %>%
    pivot_longer(
      cols = everything(),
      names_to = "model",
      values_to = "fit"
    ) %>%
    mutate(
      loo = future_map(fit, ~{
        if (method == "loo") {
          .x$loo()
        } else if (method == "waic") {
          waic(extract_log_lik(cmdstan2rstan(.x)))
        }
      },
      .options = furrr_options(seed = 1)
    )) %>%
    select(!fit)
  
  if (method == "waic") {
    loos <- loos %>%
      rename(waic = loo)
  }
  
  loos
}

goldstein_einhorn <- function(p, alpha, beta) {
  alpha * (p ^ beta) / (alpha * (p ^ beta) + (1 - p) ^ beta)
}

q_integrand <- function(p, alpha, beta) {
  goldstein_einhorn(p, alpha, beta) - p
}

# 04_group.Rmd ----------

# Calculate the ideal accuracy of majority
calc_ideal_group_accuracy <- function(df_group, n_iter = 1000, seed = 1) {
  
  df_group <- df_group %>%
    filter(order == "post")
  
  set.seed(seed)
  list_group_accuracy <- vector("list", n_iter * length(seq(1, 25, 2)))
  counter = 0
  for (iter_i in 1:n_iter) {
    tmp <- df_group %>%
      group_by(id, ori_var) %>%
      sample_n(1) %>%
      ungroup()
    for (n_i in seq(1, 25, 2)) {
      counter = counter + 1
      list_group_accuracy[[counter]] <- tmp %>%
        group_by(ori_var) %>%
        sample_n(n_i) %>%
        summarise(n_correct = sum(result == "correct")) %>%
        mutate(
          group_correct = as.numeric(n_correct > n_i / 2),
          n_member      = n_i
        )
    }
  }
  map_dfr(list_group_accuracy, bind_rows) %>% return()
}

# Simulate the majority accuracy
sim_group_accuracy <- function(n_iter = 1000, seed = 1) {
  
  # Parameter list
  param = tibble(
    n    = rep(seq(1, 25, 2), 16),
    mean = rep(rep(seq(0.55, 0.85, 0.1), each = 13), 4),
    sd   = rep(seq(0.05, 0.2, 0.05), each = 52)
  )

  # Mean group and best-members' accuracy
  all_group_resps <- numeric(nrow(param))
  all_best_ps     <- numeric(nrow(param))
  
  # Random seed
  set.seed(seed)
  
  # Calculate accuracy for each parameter
  for (param_i in 1:nrow(param)) {
    
    # Parameters
    n           <- param$n[param_i]
    mean        <- param$mean[param_i]
    sd          <- param$sd[param_i]
    group_resps <- numeric(n_iter)
    best_ps     <- numeric(n_iter)
    
    # Run simulation n_iter times
    for (iter_i in 1:n_iter) {
      while (1 == 1) {
        ps <- rnorm(n = n, mean = mean, sd = sd)
        if (min(ps) > 0.4 && max(ps) < 0.99) {
          break
        }
      }
      resps = numeric(n)
      for (n_i in 1:n) {
        resps[n_i] <- rbinom(1, 1, ps[n_i])
      }
      if (sum(resps) > n * 0.5) {
        group = 1
      } else {
        group = 0
      }
      group_resps[iter_i] <- group
      best_ps[iter_i]     <- max(ps)
    }
    all_group_resps[param_i] <- mean(group_resps)
    all_best_ps[param_i]     <- mean(best_ps)
  }

  param %>%
    mutate(
      group = all_group_resps,
      best  = all_best_ps
    ) %>%
    return()
}


function(data, n) {
  
}

# Calculate the actual majority accuracy
# 実際に多数決を選んだ人達の間で正答率を計算する
calc_actual_group_accuracy <- function(df_group, n_iter = 1000, seed = 1) {
  df_group <- df_group %>%
    filter(order == "post")
  reward_levels <- levels(factor(df_group$payoff_solo))
  set.seed(seed)
  list_group_accuracy <- vector("list", n_iter * length(reward_levels))
  counter <- 0
  for (iter_i in 1:n_iter) {
    tmp <- df_group %>%
      group_by(id, ori_var, payoff_solo) %>%
      sample_n(1) %>%
      ungroup() %>%
      group_by(ori_var, payoff_solo) %>%
      sample_n(25)
    for (reward_i in reward_levels) {
      counter = counter + 1
      list_group_accuracy[[counter]] <- tmp %>%
        filter(payoff_solo == reward_i) %>%
        group_by(ori_var, payoff_solo) %>%
        summarise(
          n_group = sum(choice == "group"),
          n_correct = sum(choice == "group" & result == "correct")
        ) %>%
        mutate(majority = if_else(n_group == 0, "cancel", if_else(n_correct >= n_group / 2, "correct", "wrong")))
    }
  }
  map_dfr(list_group_accuracy, bind_rows)
}
