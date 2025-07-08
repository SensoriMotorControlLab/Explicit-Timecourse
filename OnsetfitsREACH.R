last_8_aligned <-   strat_data [
  ( strat_data$cutrial_no %in% 81:88 & strat_data$group == 'Group 1') |
    (strat_data$cutrial_no %in% 105:112 & strat_data$group == 'Group 2'),]

first_50_rotated <- strat_data[
  (strat_data$cutrial_no %in% 89:138 & strat_data$group == 'Group 1') |
    (strat_data$cutrial_no %in% 113:162 & strat_data$group == 'Group 2'),]


######build step function######
first_step_over_6 <- first_50_rotated %>%
  filter(aimdeviation_deg > 6) %>%
  group_by(participant_id, group) %>%
  slice_min(order_by = cutrial_no, n = 1)

first_step_over_6 <- first_step_over_6 %>%
  mutate(relative_step_trial = case_when(
    group == "Group 1" ~ cutrial_no - 89,
    group == "Group 2" ~ cutrial_no - 105
  ))


data_stepfit <- rbind(last_8_aligned, first_50_rotated) %>%
  group_by(participant_id) %>%
  mutate(trial = cutrial_no - min(cutrial_no)) %>%
  ungroup()

participant_ids <- unique(data_stepfit$participant_id)
predictions_list <- list()

for (pid in participant_ids) {
  
  subdata <- data_stepfit %>% filter(participant_id == pid)
  trial_nums <- subdata$trial
  aimdev <- subdata$aimdeviation_deg
  
  step_over_6 <- subdata %>% filter(trial >= 0 & aimdeviation_deg > 6)
  if (nrow(step_over_6) == 0) next
  
  est_step_trial <- min(step_over_6$trial)
  est_mean2 <- mean(step_over_6$aimdeviation_deg)
  
  explicit_step_likelihood <- function(par) {
    step_trial <- par["step_trial"]
    mean2 <- par["mean2"]
    sd <- par["sd"]
    
    predicted <- ifelse(trial_nums < step_trial, 0, mean2)
    -sum(dnorm(aimdev, mean = predicted, sd = sd, log = TRUE))
  }
  
  init_par_step <- c(step_trial = est_step_trial, mean2 = est_mean2, sd = 5)
  
  stepfit <- optim(
    par = init_par_step,
    fn = explicit_step_likelihood,
    method = "L-BFGS-B",
    lower = c(1, 0, 1),
    upper = c(max(trial_nums), 90, 20)
  )
  
  fit_results[[pid]] <- list(par = stepfit$par, n = nrow(subdata))
  
  predicted_mean <- ifelse(trial_nums < stepfit$par["step_trial"], 0, stepfit$par["mean2"])
  sq_error <- (aimdev - predicted_mean)^2
  
  predictions_list[[pid]] <- tibble(
    participant_id = pid,
    trial = trial_nums,
    predicted_mean = predicted_mean,
    sq_error = sq_error
  )
}

all_predictions <- bind_rows(predictions_list)

mse_by_participant <- all_predictions %>%
  group_by(participant_id) %>%
  summarise(mse = mean(sq_error, na.rm = TRUE))

n_per_participant <- tibble(
  participant_id = names(fit_results),
  n = sapply(fit_results, function(x) x$n)
)

stepAIC <- Reach::AIC(mse_by_participant$mse, 3, n_per_participant$n)

#########two step
two_step_fit_results <- list()
two_step_predictions_list <- list()

for (pid in participant_ids) {
  
  subdata <- data_stepfit %>% filter(participant_id == pid)
  trial_nums <- subdata$trial
  aimdev <- subdata$aimdeviation_deg
  
  step_over_6 <- subdata %>% filter(trial >= 0 & aimdeviation_deg > 6)
  if (nrow(step_over_6) == 0) next
  
  est_step_trial <- min(step_over_6$trial)
  est_mean2 <- mean(step_over_6$aimdeviation_deg)
  
  explicit_2step_likelihood <- function(par) {
    step1 <- par["step_trial1"]
    step2 <- par["step_trial2"]
    mean2 <- par["mean2"]
    mean3 <- par["mean3"]
    sd <- par["sd"]
    
    # Penalize invalid step ordering
    if (step2 <= step1) return(1e6)
    
    predicted <- ifelse(trial_nums < step1, 0,
                        ifelse(trial_nums < step2, mean2, mean3))
    -sum(dnorm(aimdev, mean = predicted, sd = sd, log = TRUE))
  }
  
  init_par_2step <- c(
    step_trial1 = est_step_trial,
    step_trial2 = est_step_trial + 10,
    mean2 = est_mean2,
    mean3 = est_mean2 + 15,
    sd = 5
  )
  
  two_step_fit <- optim(
    par = init_par_2step,
    fn = explicit_2step_likelihood,
    method = "L-BFGS-B",
    lower = c(1, 2, 0, 0, 1),
    upper = c(max(trial_nums) - 1, max(trial_nums), 90, 90, 20)
  )
  
  two_step_fit_results[[pid]] <- list(par = two_step_fit$par, n = nrow(subdata))
  
  predicted_mean <- ifelse(trial_nums < two_step_fit$par["step_trial1"], 0,
                           ifelse(trial_nums < two_step_fit$par["step_trial2"], two_step_fit$par["mean2"], two_step_fit$par["mean3"]))
  
  sq_error <- (aimdev - predicted_mean)^2
  
  two_step_predictions_list[[pid]] <- tibble(
    participant_id = pid,
    trial = trial_nums,
    predicted_mean = predicted_mean,
    sq_error = sq_error
  )
}

all_two_step_predictions <- bind_rows(two_step_predictions_list)

mse_by_participant_two_step <- all_two_step_predictions %>%
  group_by(participant_id) %>%
  summarise(mse = mean(sq_error, na.rm = TRUE))

n_per_participant_two_step <- tibble(
  participant_id = names(two_step_fit_results),
  n = sapply(two_step_fit_results, function(x) x$n)
)

twostepAIC <- Reach::AIC(mse_by_participant_two_step$mse, 5, n_per_participant_two_step$n)


#########exponential


##bc. were. fitting to all participants,we need to loop
participant_ids <- unique(data_stepfit$participant_id)

results_list <- list()  # empty list to store results

for (pid in participant_ids) {

  subdata <- data_stepfit %>% filter(participant_id == pid)
  signal <- subdata$aimdeviation_deg
  timepoints <- 0:(length(signal) - 1)
  

  fit <- Reach::exponentialFit(
    signal = signal,
    timepoints = timepoints,
    mode = "learning",
    gridpoints = 11,
    gridfits = 10,
    setN0 = NULL,
    asymptoteRange = NULL
  )
  
  params <- fit
  predicted <- Reach::exponentialModel(par = params, timepoints = timepoints)
  mse <- Reach::exponentialMSE(par = params, signal = signal, timepoints = timepoints, mode = "learning")
  

  results_list[[pid]] <- tibble(
    participant_id = pid,
    lambda = params["lambda"],
    N0 = params["N0"],
    mse = mse
  )
}

results_df <- bind_rows(results_list)

print(results_df)

expAIC <- Reach::AIC(results_df$mse,2,58)


#the lower the AIC the better the model

comparison_table <- tibble(
  participant_id = mse_by_participant$participant_id,  # or results_df$participant_id, same order
  AIC_step = stepAIC,
  AIC_exp = expAIC,
  AIC_twostep = twostepAIC,
  best_model = case_when(
    AIC_step < AIC_exp & AIC_step < AIC_twostep ~ "Step",
    AIC_exp < AIC_step & AIC_exp < AIC_twostep ~ "Exponential",
    AIC_twostep < AIC_step & AIC_twostep < AIC_exp ~ "Two-step",
    TRUE ~ "Tie"
  )
)
  
