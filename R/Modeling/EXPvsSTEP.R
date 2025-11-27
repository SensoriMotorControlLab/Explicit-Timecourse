library(dplyr)
library(Reach)

#data file used:
strat_data <- read.csv("data/strategy_only_participants.csv")



##ONE STEP MODEL ## 
#negative log likelihood
oneStepNLL <- function(par, trials, aim, first_rot_trial, rotation_vals, noise_sd=4, min_step=6){
  mean2 <- par[1] # mean of step size
  trial1 <- par[2] + first_rot_trial - 1 # trial of first aim change
  sd1 <- abs(par[3]) # baseline variation
  sd2 <- abs(par[4]) # step variation
  
  if(mean2 < min_step) return(1e6) # avoid tiny steps (ie 1 degree)
  
  pred <- ifelse(trials < trial1, 0, mean2)
  residuals <- aim - pred
  
  extreme_mask <- !is.na(residuals) & (abs(residuals) > 2 * max(rotation_vals, na.rm=TRUE))
  residuals[extreme_mask] <- residuals[extreme_mask] / noise_sd
  
  ll <- ifelse(trials < trial1,
               dnorm(residuals, mean=0, sd=sd1, log=TRUE),
               dnorm(residuals, mean=0, sd=sd2, log=TRUE))
  
  return(-sum(ll, na.rm=TRUE))
}


fit_onestep_model <- function(trials, aim, first_rot_trial, rotation_vals, min_step=6, noise_sd=4){
  init_list <- list(
    c(mean2=10, trial1=3, sd1=3, sd2=5),
    c(mean2=20, trial1=5, sd1=3, sd2=5),
    c(mean2=40, trial1=10, sd1=3, sd2=5)
  )
  
  fits <- lapply(init_list, function(init){
    tryCatch(
      optim(init, oneStepNLL, method="L-BFGS-B",
            lower=c(min_step, 1, 0.1, 0.1),
            upper=c(max(rotation_vals, na.rm=TRUE)*2, length(trials), 10, 10),
            trials=trials, aim=aim, first_rot_trial=first_rot_trial, rotation_vals=rotation_vals, noise_sd=noise_sd),
      error=function(e) list(value=1e12, par=rep(NA,4))
    )
  })
  
  best_fit <- fits[[which.min(sapply(fits, function(f) f$value))]]
  if(is.null(best_fit$par) || length(best_fit$par)!=4){
    best_fit$par <- rep(NA,4)
    best_fit$value <- 1e12
  }
  names(best_fit$par) <- c("mean2","trial1","sd1","sd2")
  
  return(list(mse=best_fit$value, params=best_fit$par, k=4))
}



##TWO STEP MODEL ###

#negative log likelihood
twoStepNLL <- function(par, trials, aim, first_rot_trial, rotation_vals, noise_sd=4, min_step1=6, min_step2=6){
  mean2 <- par[1] # step 1 size
  mean3 <- par[2] # step 2 size
  trial1 <- par[3] + first_rot_trial - 1 # trial of first step
  trial2 <- par[4] + first_rot_trial - 1 # trial of second step
  sd1 <- abs(par[5]) # baseline variation
  sd2 <- abs(par[6]) # step variation
  
  
  if(mean2 < min_step1) return(1e6)
  if((mean3 - mean2) < min_step2) return(1e6) #step 2 should be bigger than step 1
  if(trial2 <= trial1) return(1e6) #step 2 should happen after step 1
  
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

fit_twostep_model <- function(trials, aim, first_rot_trial, rotation_vals, min_step1=6, min_step2=6, noise_sd=4){
  init_list <- list(
    c(mean2=10, mean3=20, trial1=3, trial2=10, sd1=3, sd2=5),
    c(mean2=15, mean3=30, trial1=5, trial2=15, sd1=3, sd2=5),
    c(mean2=20, mean3=40, trial1=8, trial2=20, sd1=3, sd2=5)
  )
  
  fits <- lapply(init_list, function(init){
    tryCatch(
      optim(init, twoStepNLL, method="L-BFGS-B",
            lower=c(min_step1, min_step1+min_step2, 1, 2, 0.1, 0.1),
            upper=c(max(rotation_vals, na.rm=TRUE)*2, max(rotation_vals, na.rm=TRUE)*2,
                    length(trials), length(trials), 10, 10),
            trials=trials, aim=aim, first_rot_trial=first_rot_trial, rotation_vals=rotation_vals, noise_sd=noise_sd, min_step1=min_step1, min_step2=min_step2),
      error=function(e) list(value=1e12, par=rep(NA,6))
    )
  })
  
  best_fit <- fits[[which.min(sapply(fits, function(f) f$value))]]
  if(is.null(best_fit$par) || length(best_fit$par)!=6){
    best_fit$par <- rep(NA,6)
    best_fit$value <- 1e12
  }
  names(best_fit$par) <- c("mean2","mean3","trial1","trial2","sd1","sd2")
  
  return(list(mse=best_fit$value, params=best_fit$par, k=6))
}

#this was my original exp fit but when applid to reach data, it seemed to strict so everyone was assigned as one-step for their cursor reaches
# expFit <- function(trials, aim) {
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
##the block being used now (3 params) increases exponential participants by 6 more


##EXP MODEL##

#negative log likelihood
expNLL <- function(par, trials, aim){
  A <- par[1] # asymptote
  tau <- max(abs(par[2]), 0.01) 
  sd <- max(abs(par[3]), 0.1) #  noise
  
  pred <- A * (1 - exp(-trials / tau))
  residuals <- aim - pred
  
  -sum(dnorm(residuals, mean=0, sd=sd, log=TRUE), na.rm=TRUE)
}

fit_exponential_model <- function(trials, aim){
  init_list <- list(
    c(20, 5, 5),
    c(40, 10, 5),
    c(60, 20, 5)
  )
  
  fits <- lapply(init_list, function(init){
    tryCatch(
      optim(init, expNLL, method="L-BFGS-B",
            lower=c(0, 0.01, 0.1), upper=c(200, 100, 20),
            trials=trials, aim=aim),
      error=function(e) list(value=1e12, par=rep(NA,3))
    )
  })
  
  best_fit <- fits[[which.min(sapply(fits, function(f) f$value))]]
  if(is.null(best_fit$par) || length(best_fit$par)!=3){
    best_fit$par <- rep(NA,3)
    best_fit$value <- 1e12
  }
  names(best_fit$par) <- c("A","tau","sd")
  
  return(list(mse=best_fit$value, params=best_fit$par, k=3))
}

##aic
compute_aic <- function(mse, k, n){
  Reach::AIC(mse, k, n)
}

##fit all models and find lowest aic
fitAllModels <- function(strat_data){
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
  
  
  #loop through all participants
  for(pid in participants){
    dat <- strat_data %>% filter(participant_id == pid)
    group <- unique(dat$group)
    
    baseline_trials <- dat %>% filter(trial_type.x=='aligned') %>% tail(8)
    rotated_trials  <- dat %>% filter(trial_type.x=='rotated') %>% head(60)
    all_trials <- rbind(baseline_trials, rotated_trials) %>% arrange(cutrial_no)
    
    trials <- 0:(nrow(all_trials)-1)
    aim <- all_trials$aimdeviation_deg
    rotation_vals <- all_trials$rotation
    first_rot_trial <- which(all_trials$trial_type.x=="rotated")[1]
    
    step1_fit <- fit_onestep_model(trials, aim, first_rot_trial, rotation_vals)
    step2_fit <- fit_twostep_model(trials, aim, first_rot_trial, rotation_vals)
    exp_fit   <- fit_exponential_model(trials, aim)
    
    n_trials <- length(trials)
    
    step1_aic <- compute_aic(step1_fit$mse, step1_fit$k, n_trials)
    step2_aic <- compute_aic(step2_fit$mse, step2_fit$k, n_trials)
    exp_aic   <- compute_aic(exp_fit$mse, exp_fit$k, n_trials)
    
    best_model <- c("exponential","one-step","two-step")[which.min(c(exp_aic, step1_aic, step2_aic))]
    
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
  colnames(summary_overall) <- c("best_model","count")
  
  return(list(results=results, summary_overall=summary_overall))
}


ModelChi <- function () {
model_results <- fitAllModels(strat_data)
model_results$summary_overall
best_model_counts <- model_results$summary_overall
chisq_test <- chisq.test(best_model_counts$count)
chisq_test
}



ModelRotChi <- function () {
rotation_summary <- strat_data %>%
    group_by(participant_id) %>%
    summarize(rotation_size = unique(rotation)[1])  

  participant_results <- model_results$results %>%
    left_join(rotation_summary, by = c("participant" = "participant_id"))
  
  table_model_rotation <- table(participant_results$rotation_size, participant_results$best_model)
  table_model_rotation
  chisq.test(table_model_rotation)
}







