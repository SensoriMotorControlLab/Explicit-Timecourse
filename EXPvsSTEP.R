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


#lets plot 
results$step_aic <- pmin(results$step1_aic, results$step2_aic)

# Select participant and AICs for exponential and step models
aic_long <- results %>%
  select(participant, exp_aic, step_aic) %>%
  pivot_longer(cols = c(exp_aic, step_aic),
               names_to = "model",
               values_to = "AIC")

# Optional: make model labels pretty
aic_long$model <- factor(aic_long$model, levels = c("step_aic", "exp_aic"),
                         labels = c("Step Model", "Exponential Model"))

# Plot
ggplot(aic_long, aes(x = model, y = AIC)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.7, color = "blue", size = 3) +
  geom_boxplot(alpha = 0.3, outlier.shape = NA) +
  labs(
    title = "AIC Scores per Participant",
    x = "Model",
    y = "AIC)"
  ) +
  theme_minimal()


#with three models
aic_long <- results %>%
  select(participant, step1_aic, step2_aic, exp_aic) %>%
  pivot_longer(cols = c(step1_aic, step2_aic, exp_aic),
               names_to = "model",
               values_to = "AIC") %>%
  mutate(model = recode(model,
                        step1_aic = "one-step",
                        step2_aic = "two-step",
                        exp_aic = "exponential"))

best_models <- aic_long %>%
  group_by(participant) %>%
  slice_min(AIC, with_ties = FALSE) %>%
  ungroup() %>%
  select(participant, winning_model = model)


aic_long <- aic_long %>%
  left_join(best_models, by = "participant")
aic_long$model <- factor(aic_long$model, levels = c("one-step", "two-step", "exponential"))
aic_long$winning_model <- factor(aic_long$winning_model,
                                 levels = c("one-step", "two-step", "exponential"))


ggplot(aic_long, aes(x = model, y = AIC)) +
  geom_boxplot(
    aes(fill = model),
    alpha = 0.4,
    outlier.shape = NA,
    color = "navy",
    show.legend = FALSE  
  ) +
  
  geom_point(
    aes(fill = winning_model),
    shape = 21, size = 3.7, alpha = 0.4, color = "grey",
    position = position_jitter(width = 0.15, height = 0)
  ) +
  
  scale_fill_manual(
    values = c(
      "one-step" = "orange",
      "two-step" = "cyan",
      "exponential" = "purple"
    )
  ) +
  
  labs(
    title = "",
    x = "Model",
    y = "AIC Value",
    fill = "Winning Model"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

###or just plot winners
aic_long <- results %>%
  select(participant, step1_aic, step2_aic, exp_aic) %>%
  pivot_longer(cols = c(step1_aic, step2_aic, exp_aic),
               names_to = "model",
               values_to = "AIC") %>%
  mutate(model = recode(model,
                        step1_aic = "one-step",
                        step2_aic = "two-step",
                        exp_aic = "exponential"))


best_models <- aic_long %>%
  group_by(participant) %>%
  slice_min(AIC, with_ties = FALSE) %>%
  ungroup()
best_models$model <- factor(best_models$model, levels = c("one-step", "two-step", "exponential"))

ggplot(best_models, aes(x = model, y = AIC, fill = model)) +

  geom_jitter(width = 0.15, size = 4, alpha = 0.6, color = "grey70") +
  

  geom_boxplot(alpha = 0.4, outlier.shape = NA, color = "black") +
  

  scale_fill_manual(values = c(
    "one-step" = "magenta",
    "two-step" = "cyan",
    "exponential" = "purple"
  )) +
  
  labs(
    title = "Best Model AIC per Participant",
    x = "Winning Model",
    y = "AIC (lower is better)",
    fill = "Winning Model"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )





#now the question is are there types of strategies (fast,slow,eratic) 
#and do we wanna include them all??



