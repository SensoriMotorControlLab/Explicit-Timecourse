###fit step or curved to steps per participant
last_8_aligned <- strat_data[
  (strat_data$cutrial_no %in% 81:88 & strat_data$group == 'Group 1') |
    (strat_data$cutrial_no %in% 105:112 & strat_data$group == 'Group 2'), ]

# First 50 rotated trials after rotation
first_50_rotated <- strat_data[
  (strat_data$cutrial_no %in% 89:138 & strat_data$group == 'Group 1') |
    (strat_data$cutrial_no %in% 113:162 & strat_data$group == 'Group 2'), ]

combined_data <- rbind(last_8_aligned, first_50_rotated)
combined_data$trial_relative <- NA

combined_data$trial_relative[combined_data$group == "Group 1"] <- combined_data$cutrial_no[combined_data$group == "Group 1"] - 88
combined_data$trial_relative[combined_data$group == "Group 2"] <- combined_data$cutrial_no[combined_data$group == "Group 2"] - 112


fit_participant_models <- function(df) {
  df <- df[order(df$trial_relative), ]
  

  df <- df %>% filter(!is.na(aimdeviation_deg), !is.na(trial_relative))
  pid <- unique(df$participant_id)
  
  
  above_thresh <- which(df$trial_relative > 0 & df$aimdeviation_deg > 10)
  
  if (length(above_thresh) == 0) {
    first_step_trial <- NA_real_
  } else {
    first_step_trial <- df$trial_relative[above_thresh[1]]
  }
  
  # step model
  step_model <- function(par, trial, aimdev) {
    t_step <- par[1]
    height <- par[2]
    noise_sd <- par[3]
    pred <- ifelse(trial >= t_step, height, 0)
    -sum(dnorm(aimdev, mean = pred, sd = noise_sd, log = TRUE))
  }
  
  # exponential model
  exp_model <- function(par, trial, aimdev) {
    asymptote <- par[1]
    rate <- par[2]
    noise_sd <- par[3]
    pred <- asymptote * (1 - exp(-rate * trial))
    -sum(dnorm(aimdev, mean = pred, sd = noise_sd, log = TRUE))
  }
  
  # 2-step model
  two_step_model <- function(par, trial, aimdev) {
    t_step1 <- par[1]
    t_step2 <- par[2]
    mean2 <- par[3]
    mean3 <- par[4]
    noise_sd <- par[5]
    
    # Penalize if step times are not ordered or out of bounds
    if (t_step2 <= t_step1 || t_step1 < 0 || t_step2 < 0) return(1e6)
    
    pred <- ifelse(trial < t_step1, 0,
                   ifelse(trial < t_step2, mean2, mean3))
    
    -sum(dnorm(aimdev, mean = pred, sd = noise_sd, log = TRUE))
  }
  
  # Fit 1-step model
  step_fit <- tryCatch({
    optim(par = c(max(first_step_trial, 0), 20, 5),
          fn = step_model,
          trial = df$trial_relative,
          aimdev = df$aimdeviation_deg,
          method = "L-BFGS-B",
          lower = c(0, -180, 1e-2),
          upper = c(max(df$trial_relative), 180, 50))
  }, error = function(e) NULL)
  
  # Fit exponential model
  exp_fit <- tryCatch({
    optim(par = c(20, 0.1, 5),
          fn = exp_model,
          trial = df$trial_relative,
          aimdev = df$aimdeviation_deg,
          method = "L-BFGS-B",
          lower = c(-180, 1e-3, 1e-2),
          upper = c(180, 2, 50))
  }, error = function(e) NULL)
  
  # Fit 2-step model
  two_step_fit <- tryCatch({
    optim(par = c(max(first_step_trial, 0), max(first_step_trial, 0) + 5, 15, 30, 5), 
          fn = two_step_model,
          trial = df$trial_relative,
          aimdev = df$aimdeviation_deg,
          method = "L-BFGS-B",
          lower = c(0, 0, -180, -180, 1e-2),
          upper = c(max(df$trial_relative), max(df$trial_relative), 180, 180, 50))
  }, error = function(e) NULL)
  
  # Handle cases where fits fail
  if (is.null(step_fit) || is.null(exp_fit) || is.null(two_step_fit)) {
    return(tibble(
      participant_id = pid,
      step_aic = if(!is.null(step_fit)) 2*length(step_fit$par) + 2*step_fit$value else NA_real_,
      exp_aic = if(!is.null(exp_fit)) 2*length(exp_fit$par) + 2*exp_fit$value else NA_real_,
      two_step_aic = if(!is.null(two_step_fit)) 2*length(two_step_fit$par) + 2*two_step_fit$value else NA_real_,
      first_step_trial = first_step_trial,
      best_model = NA_character_
    ))
  }
  
  # Calculate AIC for all models
  step_aic <- 2 * length(step_fit$par) + 2 * step_fit$value
  exp_aic <- 2 * length(exp_fit$par) + 2 * exp_fit$value
  two_step_aic <- 2 * length(two_step_fit$par) + 2 * two_step_fit$value
  
  # Determine best model by lowest AIC
  aic_values <- c(step = step_aic, exp = exp_aic, two_step = two_step_aic)
  best_model <- names(which.min(aic_values))
  
  tibble(
    participant_id = pid,
    step_aic = step_aic,
    exp_aic = exp_aic,
    two_step_aic = two_step_aic,
    first_step_trial = first_step_trial,
    best_model = best_model,
    step_fit_par = list(step_fit$par),
    exp_fit_par = list(exp_fit$par),
    two_step_fit_par = list(two_step_fit$par)
  )
}


fit_all <- combined_data %>%
  group_by(participant_id) %>%
  group_split() %>%
  map_df(fit_participant_models)

#output table for # of participants per model
participant_rotations <- strat_data %>%
  select(participant_id, rotation) %>%
  distinct()

fit_with_rotation <- fit_all %>%
  left_join(participant_rotations, by = "participant_id")

model_counts <- fit_with_rotation %>%
  filter(!is.na(best_model)) %>%    
  count(rotation, best_model)




create_prediction_df <- function(df, fit_results) {
  df <- df[order(df$trial_relative), ]
  pid <- unique(df$participant_id)
  fit <- fit_results %>% filter(participant_id == pid)
  
  if (nrow(fit) == 0 || is.na(fit$best_model)) return(tibble())
  
  trial_seq <- seq(min(df$trial_relative), max(df$trial_relative), by = 1)
  pred <- rep(NA_real_, length(trial_seq))
  
  if (fit$best_model == "step" && !is.null(fit$step_fit_par[[1]])) {
    t_step <- fit$step_fit_par[[1]][1]
    height <- fit$step_fit_par[[1]][2]
    pred <- ifelse(trial_seq < t_step, 0, height)
    
  } else if (fit$best_model == "two_step" && !is.null(fit$two_step_fit_par[[1]])) {
    t_step1 <- fit$two_step_fit_par[[1]][1]
    t_step2 <- fit$two_step_fit_par[[1]][2]
    mean2 <- fit$two_step_fit_par[[1]][3]
    mean3 <- fit$two_step_fit_par[[1]][4]
    pred <- ifelse(trial_seq < t_step1, 0,
                   ifelse(trial_seq < t_step2, mean2, mean3))
    
  } else if (fit$best_model == "exp" && !is.null(fit$exp_fit_par[[1]])) {
    asymptote <- fit$exp_fit_par[[1]][1]
    rate <- fit$exp_fit_par[[1]][2]
    pred <- asymptote * (1 - exp(-rate * trial_seq))
    
    # Force pre-rotation trials (trial < 0) to be 0
    pred[trial_seq < 0] <- 0
  }
  
  tibble(
    participant_id = pid,
    trial = trial_seq,
    prediction = pmax(pred, 0),  # Also clamp any small negatives
    rotation = fit$rotation
  )
}


#add rotation as column
rotation_info <- combined_data %>%
  select(participant_id, rotation) %>%
  distinct()

fit_all <- fit_all %>%
  left_join(rotation_info, by = "participant_id")

##########PLOT############


predictions_all <- combined_data %>%
  group_by(participant_id) %>%
  group_split() %>%
  map_df(~ create_prediction_df(.x, fit_all))


ggplot(predictions_all, aes(x = trial, y = prediction, group = participant_id)) +
  geom_line(aes(color = as.factor(rotation)), size = 1) +
  scale_color_brewer(palette = "Dark2", name = "Rotation") +
  coord_cartesian(ylim = c(0, 60)) +
  labs(
    title = "Model-Predicted Aiming Strategies (All Participants)",
    x = "Trial (relative)",
    y = "Predicted Aim Deviation (deg)"
  ) +
  theme_minimal()


fit_with_rotation <- fit_with_rotation %>%
  mutate(
    best_model_simple = ifelse(best_model %in% c("step", "two_step"), "step", "exp")
  )

ggplot(fit_with_rotation, aes(x = exp, y = step, color = factor(rotation))) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(color = "Rotation") +
  theme_minimal()



fit_with_rotation <- fit_with_rotation %>%
  mutate(
    step_combined_aic = pmin(step_aic, two_step_aic),
    exp_fit_score = -exp_aic,
    step_fit_score = -step_combined_aic,
    best_model_simple = ifelse(step_combined_aic < exp_aic, "step", "exp")
  )

ggplot(fit_with_rotation, aes(x = exp_fit_score, y = step_fit_score, color = best_model_simple)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "Exp Model Fit Score (higher = better)",
    y = "Step Model Fit Score (higher = better)",
    color = "Best Model (lower AIC)",
    title = ""
  ) +
  annotate("text", x = -550, y = -350, label = "n = 28", color = "darkgrey", size = 5) +   # above line
  annotate("text", x = -350, y = -550, label = "n = 10", color = "darkgrey", size = 5
           ) +  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line()
  )

#stats

n_step <- model_counts %>%
  filter(model_bin == "step") %>%
  summarise(total = sum(n)) %>%
  pull(total) #28

n_exp <- model_counts %>%
  filter(model_bin == "exp") %>%
  summarise(total = sum(n)) %>%
  pull(total) #10

model_counts <- model_counts %>%
  mutate(model_bin = ifelse(best_model %in% c("step", "two_step"), "step", "exp"))
model_counts_collapsed <- model_counts %>%
  group_by(rotation, model_bin) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = model_bin, values_from = n, values_fill = 0)


model_logreg <- glm(cbind(exp, step) ~ rotation, 
                    data = model_counts_collapsed, 
                    family = binomial())

summary(model_logreg)







