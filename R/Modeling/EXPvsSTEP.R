strat_data <- read.csv("data/strategy_only_participants.csv")

fitAllModels <- function(strat_data) {
results <- data.frame(
  participant = character(),
  group = character(),
  exp_aic = numeric(),
  step1_aic = numeric(),
  step2_aic = numeric(),
  best_model = character(),
  stringsAsFactors = FALSE
)


participants <- unique(strat_data$participant_id)

for (pid in participants) {
  dat <- strat_data %>% filter(participant_id == pid)
  group <- unique(dat$group)
  
  baseline_trials <- dat %>% filter(trial_type.x == 'aligned') %>% tail(8)
  rotated_trials  <- dat %>% filter(trial_type.x == 'rotated') %>% head(60)
  
  all_trials <- rbind(baseline_trials, rotated_trials) %>% arrange(cutrial_no)
  
  trials <- 0:(nrow(all_trials) - 1)      
  aim    <- all_trials$aimdeviation_deg
  rotation_vals <- all_trials$rotation
  first_rot_trial <- which(all_trials$trial_type.x == "rotated")[1]
  
  ################## one-step model with noise + stability
  fit_onestep_model <- function(trials, aim, first_rot_trial, min_step=6, noise_sd=4){
    neg_log_likelihood <- function(par){
      mean2  <- par[1]                       # plateau after step
      trial1 <- par[2] + first_rot_trial - 1
      sd1    <- abs(par[3])
      sd2    <- abs(par[4])
      
      if(mean2 < min_step) return(1e6)
      
      pred <- ifelse(trials < trial1, 0, mean2)
      residuals <- aim - pred
      
      extreme_mask <- !is.na(residuals) & (abs(residuals) > 2 * max(rotation_vals, na.rm=TRUE))
      residuals[extreme_mask] <- residuals[extreme_mask] / noise_sd
      
      ll <- ifelse(trials < trial1,
                   dnorm(residuals, mean=0, sd=sd1, log=TRUE),
                   dnorm(residuals, mean=0, sd=sd2, log=TRUE))
      return(-sum(ll, na.rm=TRUE))
    }
    
    # Try multiple starts
    init_list <- list(
      c(mean2=10, trial1=3, sd1=3, sd2=5),
      c(mean2=20, trial1=5, sd1=3, sd2=5),
      c(mean2=40, trial1=10, sd1=3, sd2=5)
    )
    
    fits <- lapply(init_list, function(init) {
      optim(init, neg_log_likelihood, method="L-BFGS-B",
            lower=c(min_step, 1, 1, 1),
            upper=c(max(rotation_vals, na.rm=TRUE)*2, length(trials), 10, 10))
    })
    
    best_fit <- fits[[which.min(sapply(fits, function(f) f$value))]]
    names(best_fit$par) <- c("mean2", "trial1", "sd1", "sd2")
    return(list(mse=best_fit$value, params=best_fit$par, k=4))
  }
  
  ################## two-step model with noise + stability
  fit_twostep_model <- function(trials, aim, first_rot_trial, min_step1=6, min_step2=6, noise_sd=4){
    neg_log_likelihood <- function(par){
      mean2  <- par[1]                       # plateau after step 1
      mean3  <- par[2]                       # plateau after step 2
      trial1 <- par[3] + first_rot_trial - 1
      trial2 <- par[4] + first_rot_trial - 1
      sd1    <- abs(par[5])
      sd2    <- abs(par[6])
      
      # Enforce constraints
      if(mean2 < min_step1) return(1e6)
      if((mean3 - mean2) < min_step2) return(1e6)
      if(trial2 <= trial1) return(1e6)  # step 2 must come after step 1
      
      pred <- ifelse(trials < trial1, 0,
                     ifelse(trials < trial2, mean2, mean3))
      residuals <- aim - pred
      
      extreme_mask <- !is.na(residuals) & (abs(residuals) > 2 * max(rotation_vals, na.rm=TRUE))
      residuals[extreme_mask] <- residuals[extreme_mask] / noise_sd
      
      ll <- ifelse(trials < trial1,
                   dnorm(residuals, mean=0, sd=sd1, log=TRUE),
                   dnorm(residuals, mean=0, sd=sd2, log=TRUE))
      return(-sum(ll, na.rm=TRUE))
    }
    
    init_list <- list(
      c(mean2=10, mean3=20, trial1=3, trial2=10, sd1=3, sd2=5),
      c(mean2=15, mean3=30, trial1=5, trial2=15, sd1=3, sd2=5),
      c(mean2=20, mean3=40, trial1=8, trial2=20, sd1=3, sd2=5)
    )
    
    fits <- lapply(init_list, function(init) {
      optim(init, neg_log_likelihood, method="L-BFGS-B",
            lower=c(min_step1, min_step1+min_step2, 1, 2, 1, 1),
            upper=c(max(rotation_vals, na.rm=TRUE)*2, max(rotation_vals, na.rm=TRUE)*2,
                    length(trials), length(trials), 10, 10))
    })
    
    best_fit <- fits[[which.min(sapply(fits, function(f) f$value))]]
    if (is.null(best_fit$par) || length(best_fit$par) != 4) {
      return(list(mse = 1e12, params = rep(NA, 4), k = 4))
    }
    
    names(best_fit$par) <- c("mean2", "trial1", "sd1", "sd2")
    return(list(mse=best_fit$value, params=best_fit$par, k=4))
  }
  
  ################## exponential model (continuous increase)
  
  #okay i have to block this one bc this isn't aligning with the reach fits...
 # fit_exponential_model <- function(trials, aim) {
  #  fit_values <- Reach::exponentialFit(
   #   signal = aim,
   #   timepoints = trials,
   #   mode = "learning",
   #   gridpoints = 11,
   #   gridfits = 10
  #  )
  #  mse <- mean((fit_values - aim)^2)
  #  return(list(mse = mse, k = 2))
#  }
  
  
  fit_exponential_model <- function(trials, aim) {
    exp_func <- function(time, A, tau) {
      A * (1 - exp(-time / tau))
    }
    
    neg_log_likelihood <- function(par) {
      A   <- par[1]
      tau <- abs(par[2])       
      sd  <- abs(par[3])       
      
      pred <- exp_func(trials, A, tau)
      residuals <- aim - pred
      
      -sum(dnorm(residuals, mean=0, sd=sd, log=TRUE), na.rm=TRUE)
    }
    
    # Multiple starting values
    init_list <- list(
      c(20,  5, 5),
      c(40, 10, 5),
      c(60, 20, 5)
    )
    
    fits <- lapply(init_list, function(init) {
      optim(init, neg_log_likelihood, method="L-BFGS-B",
            lower=c(0, 0.01, 0.1),
            upper=c(200, 100, 20))
    })
    
    best_fit <- fits[[which.min(sapply(fits, function(f) f$value))]]
    
    return(list(
      mse = best_fit$value,   
      params = best_fit$par,
      k = 3                   
    ))
  }
  ##this code increases exponential participants by 6 more

  ################## AIC helper
  compute_aic <- function(mse, k, n) {
    Reach::AIC(mse, k, n)
  }
  

  step1_fit <- fit_onestep_model(trials, aim, first_rot_trial)
  step2_fit <- fit_twostep_model(trials, aim, first_rot_trial)
  exp_fit   <- fit_exponential_model(trials, aim)
  
  n_trials <- length(trials)
  

  step1_aic <- compute_aic(step1_fit$mse, step1_fit$k, n_trials)
  step2_aic <- compute_aic(step2_fit$mse, step2_fit$k, n_trials)
  exp_aic   <- compute_aic(exp_fit$mse, exp_fit$k, n_trials)
  
  best_model <- c("exponential", "one-step", "two-step")[which.min(c(exp_aic, step1_aic, step2_aic))]
  

  rotation_onset <- if (group == 'Group 1') 89 else if (group == 'Group 2') 113 else stop("Unknown group!")
  

  if (best_model %in% c("one-step", "two-step")) {
    step1_trial_val  <- round(step1_fit$params["trial1"])   # relative to onset
    step1_cut_trial  <- rotation_onset + step1_trial_val - 1
    step1_size       <- step1_fit$params["mean2"]
    step1_aim        <- dat$aimdeviation_deg[dat$cutrial_no == step1_cut_trial]
  } else {
    step1_cut_trial <- NA
    step1_size <- NA
    step1_aim <- NA
  }

  if (best_model == "two-step") {
    step2_trial_val  <- round(step2_fit$params["trial2"])
    step2_cut_trial  <- rotation_onset + step2_trial_val - 1
    step2_size       <- step2_fit$params["mean3"]
    step2_aim        <- dat$aimdeviation_deg[dat$cutrial_no == step2_cut_trial]
  } else {
    step2_cut_trial <- NA
    step2_size <- NA
    step2_aim <- NA
  }
  

  results <- rbind(results, data.frame(
    participant = pid,
    group = group,
    exp_aic = exp_aic,
    step1_aic = step1_aic,
    step2_aic = step2_aic,
    best_model = best_model
  ))
  

  print(results)
}
summary_overall <- as.data.frame(table(results$best_model))
colnames(summary_overall) <- c("best model", "count")

return(list(
  results = results,
  summary_overall = summary_overall
))
}


fit_results <- fitAllModels(strat_data)

results_joined <- fit_results$results %>%
  left_join(
    strat_data %>% select(participant_id, rotation, trial_type.x) %>% distinct(),
    by = c("participant" = "participant_id")
  )

###chi sq to see if model # signficantly differs from # of exp

ModelChi <- function () {
model_counts <- results_joined %>%
  group_by(best_model, rotation) %>%
  summarise(count = n())
chisq.test(model_counts$count)
}


#does rotation have an effect on predicted model fit?
ModelChi2 <- function () {
  model_counts <- results_joined %>%
    group_by(best_model, rotation) %>%
    summarise(count = n())
chisq.test(
  xtabs(count ~ rotation + best_model, data = model_counts)
)
}




