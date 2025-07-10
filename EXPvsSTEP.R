participants <- unique(strat_data$participant_id)
results <- data.frame()

for (pid in participants) {
  dat <- strat_data %>% filter(participant_id == pid)
  group <- unique(dat$group)

  if (group == "Group 1") {
    baseline_trials <- dat %>% filter(cutrial_no %in% 81:88)
    rotated_trials <- dat %>% filter(cutrial_no %in% 89:138) %>% head(50)
  } else if (group == "Group 2") {
    baseline_trials <- dat %>% filter(cutrial_no %in% 105:112)
    rotated_trials <- dat %>% filter(cutrial_no %in% 113:162) %>% head(50)
  } else {
    next  # skip unknown group
  }
  
  all_trials <- rbind(baseline_trials, rotated_trials)
  all_trials <- all_trials %>% arrange(cutrial_no)
  trials <- 0:(nrow(all_trials) - 1)  # Re-indexing so trial 0 = rotation onset
  aim <- all_trials$aimdeviation_deg
  n_trials <- length(aim)
  
  
  
  step_model <- function(params, trials, data) {
    with(as.list(params), {
      pred <- ifelse(trials < trial1, 0,
                     mean2)  # 0 = baseline, mean2 = post-step level
      residuals <- data - pred
      mse <- mean(ifelse(trials < trial1,
                         dnorm(residuals, mean = 0, sd = sd1, log = TRUE),
                         dnorm(residuals, mean = 0, sd = sd2, log = TRUE)))
      return(-sum(mse))  # minimize negative log likelihood
    })
  }
  
  fit_onestep_model <- function(trials, aim) {
    neg_log_likelihood <- function(par) {
      mean2 <- par[1]
      trial1 <- par[2]
      sd1 <- abs(par[3])
      sd2 <- abs(par[4])
      
      pred <- ifelse(trials < trial1, 0, mean2)
      residuals <- aim - pred
      
      ll <- ifelse(trials < trial1,
                   dnorm(residuals, mean = 0, sd = sd1, log = TRUE),
                   dnorm(residuals, mean = 0, sd = sd2, log = TRUE))
      return(-sum(ll))  # return negative log-likelihood
    }
    
    init <- c(mean2 = 15, trial1 = 5, sd1 = 2, sd2 = 2)
    
    fit <- optim(init, neg_log_likelihood,
                 method = "L-BFGS-B",
                 lower = c(0, 1, 0.1, 0.1),
                 upper = c(50, max(trials), 20, 20))
    
    return(list(mse = fit$value,
                params = fit$par,
                k = 4))
  }
  
  
  ##################
  
  
  fit_twostep_model <- function(trials, aim) {
    neg_log_likelihood <- function(par) {
      mean2 <- par[1]
      mean3 <- par[2]
      trial1 <- par[3]
      trial2 <- par[4]
      sd1 <- abs(par[5])
      sd2 <- abs(par[6])
      
      pred <- ifelse(trials < trial1, 0,
                     ifelse(trials < trial2, mean2, mean3))
      residuals <- aim - pred
      
      ll <- ifelse(trials < trial1,
                   dnorm(residuals, mean = 0, sd = sd1, log = TRUE),
                   dnorm(residuals, mean = 0, sd = sd2, log = TRUE))
      return(-sum(ll))
    }
    
    init <- c(mean2 = 6, mean3 = 15, trial1 = 5, trial2 = 15, sd1 = 2, sd2 = 2)
    
    fit <- optim(init, neg_log_likelihood,
                 method = "L-BFGS-B",
                 lower = c(0, 0, 1, 2, 0.1, 0.1),
                 upper = c(50, 50, max(trials) - 2, max(trials), 20, 20))
    
    return(list(mse = fit$value,
                params = fit$par,
                k = 6))
  }
  
  
  
  #############
  signal <- aim
  timepoints <- trials
  fit_exponential_model <- function(trials, aim) {
    fit_values <- Reach::exponentialFit(
      signal = aim,
      timepoints = trials,
      mode = "learning",
      gridpoints = 11,
      gridfits = 10,
      setN0 = NULL,
      asymptoteRange = NULL
    )
    mse <- mean((fit_values - aim)^2)
    return(list(mse = mse, k = 2))
  }
  
  compute_aic <- function(mse, k, n) {
    Reach::AIC(mse, k, n)
  }  
  

  #now fit 
  step1_fit <- fit_onestep_model(trials, aim)
  step2_fit <- fit_twostep_model(trials, aim)
  exp_fit   <- fit_exponential_model(trials, aim)
  
  ## Compute AICs
  step1_aic <- compute_aic(step1_fit$mse, step1_fit$k, n_trials)
  step2_aic <- compute_aic(step2_fit$mse, step2_fit$k, n_trials)
  exp_aic   <- compute_aic(exp_fit$mse, exp_fit$k, n_trials)
  
  best_model <- c("exponential", "one-step", "two-step")[which.min(c(exp_aic, step1_aic, step2_aic))]
  
  results <- rbind(results, data.frame(
    participant = pid,
    group = group,
    exp_aic = exp_aic,
    step1_aic = step1_aic,
    step2_aic = step2_aic,
    best_model = best_model
  ))
}
  
  