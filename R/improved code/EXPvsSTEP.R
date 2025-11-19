strat_data <- read.csv("data/strategy_only_participants.csv")

results <- data.frame(
  participant = character(),
  group = character(),
  exp_aic = numeric(),
  step1_aic = numeric(),
  step2_aic = numeric(),
  best_model = character(),
  stringsAsFactors = FALSE
)

detailed_results <- data.frame(
  participant = character(),
  best_model  = character(),
  step1_trial = numeric(),
  step1_size  = numeric(),
  step1_aim   = numeric(),
  step2_trial = numeric(),
  step2_size  = numeric(),
  step2_aim   = numeric(),
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
      
      # Enforce minimum stable step
      if(mean2 < min_step) return(1e6)
      
      pred <- ifelse(trials < trial1, 0, mean2)
      residuals <- aim - pred
      
      # Downweight extreme outliers (don’t inflate)
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
    names(best_fit$par) <- c("mean2", "mean3", "trial1", "trial2", "sd1", "sd2")
    return(list(mse=best_fit$value, params=best_fit$par, k=6))
  }
  
  ################## exponential model (continuous increase)
  fit_exponential_model <- function(trials, aim) {
    fit_values <- Reach::exponentialFit(
      signal = aim,
      timepoints = trials,
      mode = "learning",
      gridpoints = 11,
      gridfits = 10
    )
    mse <- mean((fit_values - aim)^2)
    return(list(mse = mse, k = 2))
  }
  
  ################## AIC helper
  compute_aic <- function(mse, k, n) {
    Reach::AIC(mse, k, n)
  }
  
  # --- Fit models
  step1_fit <- fit_onestep_model(trials, aim, first_rot_trial)
  step2_fit <- fit_twostep_model(trials, aim, first_rot_trial)
  exp_fit   <- fit_exponential_model(trials, aim)
  
  n_trials <- length(trials)
  
  # --- Compute AICs
  step1_aic <- compute_aic(step1_fit$mse, step1_fit$k, n_trials)
  step2_aic <- compute_aic(step2_fit$mse, step2_fit$k, n_trials)
  exp_aic   <- compute_aic(exp_fit$mse, exp_fit$k, n_trials)
  
  best_model <- c("exponential", "one-step", "two-step")[which.min(c(exp_aic, step1_aic, step2_aic))]
  
  # --- rotation onset
  rotation_onset <- if (group == 'Group 1') 89 else if (group == 'Group 2') 113 else stop("Unknown group!")
  
  # --- Step 1 details
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
  
  # --- Step 2 details
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
  
  # --- Save results
  results <- rbind(results, data.frame(
    participant = pid,
    group = group,
    exp_aic = exp_aic,
    step1_aic = step1_aic,
    step2_aic = step2_aic,
    best_model = best_model
  ))
  
  detailed_results <- rbind(detailed_results, data.frame(
    participant = pid,
    best_model  = best_model,
    step1_trial = step1_cut_trial,
    step1_size  = step1_size,
    step1_aim   = step1_aim,
    step2_trial = step2_cut_trial,
    step2_size  = step2_size,
    step2_aim   = step2_aim
  ))
  print(results)
}


results_joined <- results %>%
  left_join(
    strat_data %>%
      select(participant_id, rotation) %>%
      distinct(participant_id, rotation),  # ensures one per participant
    by = c("participant" = "participant_id")
  )


model_counts <- results_joined %>%
  group_by(rotation, best_model) %>%
  summarise(count = n(), .groups = "drop")

table <- model_counts %>%
  pivot_wider(
    names_from = rotation,
    values_from = count,
    values_fill = 0  # fill missing combos with 0
  ) %>%
  arrange(best_model)

table 

models <- results_joined

oneSt <- models %>%
  filter(best_model == "one-step")

twoSt <- models %>%
filter(best_model=="two-step")

exp <- models %>%
  filter(best_model=="exponential")




chisq_test <- chisq.test(
  xtabs(count ~ rotation + best_model, data = model_counts)
)
chisq_test
chisq_test$residuals
#20° rotation: Strong positive residual for exponential (+2.03) 👉 More participants than expected showed exponential model fits here.
#60° rotation:Positive residual for two-step (+1.90) 👉 More two-step model fits than expected for large rotations.


#df - include models

#null model with average x value - 0.


#erratic strategy search - high noise initially (high sd), shift in mean and decrease in noise again.
#take window of 10 trials to see where noise is high and when it goes down

#model second implicit process



#show jonathan tsay strategy types 
rotated_trials <- subset(strat_data, 
                         (group == "Group 1" & cutrial_no >= 89 & cutrial_no <= 139) |
                           (group == "Group 2" & cutrial_no >= 113 & cutrial_no <= 163))


first_50_rotated <- rotated_trials %>%
  group_by(participant_id) %>%
  arrange(cutrial_no) %>%
  slice_head(n = 50) %>%
  mutate(cutrial_no = row_number())  # Count trial numbers 1 to 50 within participant

first_step_over_10 <- first_50_rotated %>%
  filter(aimdeviation_deg > 10) %>%
  group_by(participant_id) %>%
  slice_min(order_by = cutrial_no, n = 1)


result_table <- dplyr::select(first_step_over_10, 
                              participant_id, rotation, cutrial_no, aimdeviation_deg)

df_steps <- result_table %>%
  rowwise() %>%
  mutate(
    trials = list(-8:50),
    aim_deviation = list(pmin(ifelse(-8:50 < cutrial_no, 0, aimdeviation_deg), 60))
  ) %>%
  unnest(c(trials, aim_deviation))


plotSteps <- function(target = "inline", main = NULL) {
  setupFigureFile(
    target = target,
    width = 3,
    height = 3,
    dpi = 300,
    sprintf("images/plotsteps.%s", target)
  )
  
  # join in best model info
  df_steps <- result_table %>%
    left_join(
      results %>% select(participant, best_model),
      by = c("participant_id" = "participant")
    ) %>%
    rowwise() %>%
    mutate(
      trials = list(-8:50),
      aim_deviation = list({
        x <- -8:50
        if (best_model == "one-step") {
          pmin(ifelse(x < cutrial_no, 0, aimdeviation_deg), 60)
        } else if (best_model == "two-step") {
          # toy example: half step at cutrial_no, full after +10
          pmin(ifelse(x < cutrial_no, 0,
                      ifelse(x < cutrial_no + 10, aimdeviation_deg / 2, aimdeviation_deg)), 60)
        } else if (best_model == "exponential") {
          # toy example exponential rise after cutrial_no
          rise <- aimdeviation_deg * (1 - exp(-(x - cutrial_no) / 5))
          rise[x < cutrial_no] <- 0
          pmin(rise, 60)
        } else {
          rep(0, length(x))
        }
      })
    ) %>%
    unnest(c(trials, aim_deviation))  %>%
    dplyr::filter(!rotation %in% c("20", "30"))
  
  p <- ggplot(df_steps, aes(x = trials, y = aim_deviation, color = factor(rotation,
                                                                          4lim(-10:80)))) +
    geom_line(aes(group = participant_id), size = 0.8) +
    geom_vline(data = result_table, aes(xintercept = cutrial_no),
               linetype = "dashed", color = NA) +
    labs(
      x = "",
      y = "",
      color = "Rotation",
      title = ""
    ) +
    geom_vline(aes(xintercept = 0), linetype = "dashed", color = "grey60") +
    scale_color_manual(values = c(
      "40" = "darkorange",
      "50" = "darkmagenta",
      "60" = "hotpink"
    )) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(),
      axis.title.x = element_text(size = 17),
      axis.title.y = element_text(size = 17),
      axis.text.x  = element_text(size = 24),
      axis.text.y  = element_text(size = 24),
      legend.title = element_text(size = 17),
      legend.text  = element_text(size = 16),
      plot.title   = element_text(size = 19, hjust = 0),
      legend.position = "inside",
      legend.position.inside = c(0.08, 0.5)
    )
  
  if (target %in% c("pdf", "svg", "png", "tiff")) {
    dev.off()
  }
  print(p)
}




# --- select rotated trials (Group 1 and 2) ---
rotated_trials <- subset(
  strat_data, 
  (group == "Group 1" & cutrial_no >= 89 & cutrial_no <= 139) |
    (group == "Group 2" & cutrial_no >= 113 & cutrial_no <= 163)
)

# --- take first 50 rotated trials per participant ---
first_50_rotated <- rotated_trials %>%
  group_by(participant_id) %>%
  arrange(cutrial_no) %>%
  slice_head(n = 60) %>%
  mutate(cutrial_no = row_number())  # renumber within-participant 1–50

# --- find first step over 10 deg ---
first_step_over_10 <- first_50_rotated %>%
  filter(aimdeviation_deg > 10) %>%
  group_by(participant_id) %>%
  slice_min(order_by = cutrial_no, n = 1)

# --- compact results ---
result_table <- dplyr::select(
  first_step_over_10, participant_id, rotation, cutrial_no, aimdeviation_deg
)

# --- build model predictions ---
df_steps <- result_table %>%
  left_join(results %>% select(participant, best_model),
            by = c("participant_id" = "participant")) %>%
  rowwise() %>%
  mutate(
    trials = list(-8:60),   # extend to 60 after rotation
    aim_deviation = list({
      x <- -8:60
      if (best_model == "one-step") {
        pmin(ifelse(x < cutrial_no, 0, aimdeviation_deg), 80)
      } else if (best_model == "two-step") {
        # toy example: half step at cutrial_no, full step after +10
        pmin(ifelse(x < cutrial_no, 0,
                    ifelse(x < cutrial_no + 10, aimdeviation_deg / 2, aimdeviation_deg)), 80)
      } else if (best_model == "exponential") {
        rise <- aimdeviation_deg * (1 - exp(-(x - cutrial_no) / 5))
        rise[x < cutrial_no] <- 0
        pmin(rise, 80)
      } else {
        rep(0, length(x))
      }
    })
  ) %>%
  unnest(c(trials, aim_deviation)) %>%
  filter(!rotation %in% c("20", "30"))

# --- plot function ---
plotSteps <- function(target = "inline", main = NULL) {
  setupFigureFile(
    target = target,
    width = 3,
    height = 3,
    dpi = 300,
    sprintf("images/plotsteps.%s", target)
  )
  
  p <- ggplot(df_steps, aes(x = trials, y = aim_deviation,
                            color = factor(rotation))) +
    geom_line(aes(group = participant_id), size = 0.8) +
    geom_vline(aes(xintercept = 0), linetype = "dashed", color = "grey60") +
    labs(x = "", y = "", color = "Rotation", title = main) +
    scale_color_manual(values = c(
      "40" = "darkorange",
      "50" = "cadetblue",
      "60" = "hotpink"
    )) +
    coord_cartesian(xlim = c(-8, 60), ylim = c(0, 80)) +   # enforce axis ranges
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(),
      axis.text.x  = element_text(size = 24),
      axis.text.y  = element_text(size = 24),
      axis.title.x = element_text(size = 17),
      axis.title.y = element_text(size = 17),
      legend.title = element_text(size = 17),
      legend.text  = element_text(size = 16),
      plot.title   = element_text(size = 19, hjust = 0),
      legend.position = "inside",
      legend.position.inside = c(0.08, 0.5)
    )
  
  if (target %in% c("pdf", "svg", "png", "tiff")) {
    dev.off()
  }
  print(p)
}

