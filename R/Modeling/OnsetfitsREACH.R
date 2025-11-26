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
    group == "Group 2" ~ cutrial_no - 113
  ))


data_stepfit <- rbind(last_8_aligned, first_50_rotated) %>%
  group_by(participant_id) %>%
  mutate(trial = cutrial_no - min(cutrial_no)) %>%
  ungroup()

participant_ids <- unique(data_stepfit$participant_id)
fit_results <- list()
predictions_list <- list()

for (pid in participant_ids) {
  
  subdata <- data_stepfit %>% filter(participant_id == pid)
  trial_nums <- subdata$trial
  aimdev <- subdata$aimdeviation_deg
  
  #### STEP MODEL ####
  est_step_trial <- min(subdata$trial[aimdev > 10], na.rm = TRUE)
  if (is.infinite(est_step_trial)) est_step_trial <- median(trial_nums)
  
  step_likelihood <- function(par) {
    step_trial <- par["step_trial"]
    mean2 <- par["mean2"]
    sd <- par["sd"]
    
    predicted <- ifelse(trial_nums < step_trial, 0, mean2)
    -sum(dnorm(aimdev, mean = predicted, sd = sd, log = TRUE))
  }
  
  init_par_step <- c(step_trial = est_step_trial, mean2 = 12, sd = 5)
  
  step_fit <- optim(
    par = init_par_step,
    fn = step_likelihood,
    method = "L-BFGS-B",
    lower = c(1, 12, 1),
    upper = c(max(trial_nums), 18, 20)
  )
  
  predicted <- ifelse(trial_nums < step_fit$par["step_trial"], 0, step_fit$par["mean2"])
  sq_error <- (aimdev - predicted)^2
  
  predictions_list[[pid]] <- tibble(
    participant_id = pid,
    trial = trial_nums,
    aimdev = aimdev,
    predicted = predicted,
    sq_error = sq_error
  )

  fit_results[[pid]] <- list(par = step_fit$par, n = length(trial_nums))

  
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
  
  est_step1 <- min(step_over_6$trial)
  est_step2 <- est_step1 + 10  # reasonable initial guess
  
  two_step_likelihood <- function(par) {
    step1 <- par["step_trial1"]
    step2 <- par["step_trial2"]
    mean2 <- par["mean2"]
    mean3 <- par["mean3"]
    sd <- par["sd"]
    
    if (step2 <= step1) return(1e6)  # enforce step2 > step1
    
    predicted <- ifelse(trial_nums < step1, 0,
                        ifelse(trial_nums < step2, mean2, mean3))
    -sum(dnorm(aimdev, mean = predicted, sd = sd, log = TRUE))
  }
  
  init_par_2step <- c(
    step_trial1 = est_step1,
    step_trial2 = est_step2,
    mean2 = 6,
    mean3 = 18,
    sd = 5
  )
  
  two_step_fit <- optim(
    par = init_par_2step,
    fn = two_step_likelihood,
    method = "L-BFGS-B",
    lower = c(1, 2, 0, 10, 1),
    upper = c(max(trial_nums) - 1, max(trial_nums), 120, 120, 20)
  )
  
  two_step_predicted <- ifelse(trial_nums < two_step_fit$par["step_trial1"], 0,
                               ifelse(trial_nums < two_step_fit$par["step_trial2"],
                                      two_step_fit$par["mean2"],
                                      two_step_fit$par["mean3"]))
  two_step_sq_error <- (aimdev - two_step_predicted)^2
  
  # Store predictions and fit parameters
  two_step_predictions_list[[pid]] <- tibble(
    participant_id = pid,
    trial = trial_nums,
    predicted_mean = two_step_predicted,
    sq_error = two_step_sq_error
  )
  
  two_step_fit_results[[pid]] <- list(
    par = two_step_fit$par,
    value = two_step_fit$value,
    n = length(trial_nums)
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

mse_by_participant <- results_df %>%
  group_by(participant_id) %>%
  summarise(mse = mean(sq_error, na.rm = TRUE),
            n = n())  # number of observations per participant

expAIC <- Reach::AIC(mse, 2, n)
  
  mse_by_participant <- mse_by_participant %>%
  rowwise() %>%
  mutate(
    AIC = Reach::AIC(mse, 2, n)
  ) %>%
  ungroup()

# Print AICs per participant
print(mse_by_participant %>% select(participant_id, AIC))


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

#Exponential
df <- total_group_data[total_group_data$participant_id == "3091de", ] 
plot(df$aimdeviation_deg, type = "l", main = "", ylim = c(-10, 60),
     col= "forestgreen", lwd = 1)
abline(h = 0, col = "red", lty = 2)
abline(h = 50, col = "red", lty = 2)
#pure systematic exploration

df <- total_group_data[total_group_data$participant_id == "1896cb", ] 
plot(df$aimdeviation_deg, type = "l", main = "", ylim = c(-10, 60),
     col= "forestgreen", lwd = 1)
abline(h = 0, col = "red", lty = 2)
abline(h = 50, col = "red", lty = 2)
#erratic maybe even systematic

df <- total_group_data[total_group_data$participant_id == "194dab", ] 
plot(df$aimdeviation_deg, type = "l", main = "", ylim = c(-20, 60),
     col= "forestgreen", lwd = 1)
abline(h = 0, col = "red", lty = 2)
abline(h = 50, col = "red", lty = 2)
#erratic exploration

df <- total_group_data[total_group_data$participant_id == "13d986", ] 
plot(df$aimdeviation_deg, type = "l", main = "", ylim = c(-10, 60),
     col= "forestgreen", lwd = 1)
abline(h = 0, col = "red", lty = 2)
abline(h = 50, col = "red", lty = 2)
#this looks like a step - slow insight

df <- total_group_data[total_group_data$participant_id == "e066de", ] 
plot(df$aimdeviation_deg, type = "l", main = "", ylim = c(-20, 60),
     col= "forestgreen", lwd = 1)
abline(h = 0, col = "red", lty = 2)
abline(h = 50, col = "red", lty = 2)
#this is giving erratic exploration







