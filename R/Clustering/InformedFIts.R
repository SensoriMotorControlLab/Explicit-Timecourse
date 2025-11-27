
preprocess_strategy_data <- function(strat_data) {
  strat_data %>%
    rename(participant_id = participant_id) %>%  
    select(-matches("participant_id\\..")) %>%   
    filter(trial_type.x == "rotated") %>%
    group_by(participant_id) %>%
    arrange(cutrial_no, .by_group = TRUE) %>%
    mutate(trial_after_rot = row_number() - 1) %>%
    ungroup()
}


extract_trial_features <- function(strategy_data, change_thresh = 7) {
  strategy_data %>%
    filter(trial_after_rot <= 32) %>%
    group_by(participant_id) %>%
    arrange(cutrial_no, .by_group = TRUE) %>%
    mutate(
      trial_of_change = {
        t <- which(aimdeviation_deg > change_thresh | aimdeviation_deg < -change_thresh)
        if (length(t) > 0) t[1] else NA
      }
    ) %>%
    ungroup() %>%
    select(participant_id, trial_after_rot, aimdeviation_deg, trial_of_change)
}

summarise_features <- function(trial_features) {
  trial_features %>%
    group_by(participant_id) %>%
    summarise(
      trial_of_change = ifelse(all(is.na(trial_of_change)), 32, mean(trial_of_change, na.rm = TRUE)),
      sd_aim = sd(aimdeviation_deg, na.rm = TRUE),
      mean_aim = mean(aimdeviation_deg, na.rm = TRUE),
      prop_negative = mean(aimdeviation_deg < -3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(trial_of_change = trial_of_change * 8)
}


fit_step_model_onset <- function(df, threshold = 7) {
  
  # handle empty participant data
  if (nrow(df) == 0) {
    return(tibble(
      participant_id = NA_character_,
      t0 = NA_real_,
      step_size = NA_real_
    ))
  }
  
  first_jump <- which(df$aimdeviation_deg >= threshold)[1]
  
  if (is.na(first_jump)) {
    return(tibble(
      participant_id = df$participant_id[1],
      t0 = NA,
      step_size = NA
    ))
  }
  
  baseline <- mean(df$aimdeviation_deg[1:(first_jump - 1)], na.rm = TRUE)
  step_size <- df$aimdeviation_deg[first_jump] - baseline
  
  tibble(
    participant_id = df$participant_id[1],
    t0 = df$trial_after_rot[first_jump],
    step_size = step_size
  )
}

run_step_fits <- function(strategy_data) {
  
  strategy_data %>%
    group_by(participant_id) %>%
    filter(n() > 0) %>%
    group_modify(~ fit_step_model_onset(.x)) %>%
    ungroup()
}


compute_early_sd <- function(strategy_data, cutoff = 15) {
  strategy_data %>%
    filter(trial_after_rot <= cutoff) %>%
    group_by(participant_id) %>%
    summarise(sd_early = sd(aimdeviation_deg, na.rm = TRUE), .groups = "drop")
}

compute_sign_flips <- function(strategy_data) {
  strategy_data %>%
    group_by(participant_id) %>%
    arrange(trial_after_rot, .by_group = TRUE) %>%
    summarise(
      sign_flips = sum(lag(aimdeviation_deg >= -7, default = TRUE) & aimdeviation_deg < -7, na.rm = TRUE),
      .groups = "drop"
    )
}


classify_participants <- function(step_fits, early_sd, sign_flips) {
  step_fits %>%
    left_join(early_sd, by = "participant_id") %>%
    left_join(sign_flips, by = "participant_id") %>%
    mutate(
      model_class = case_when(
        sd_early > 10 & sign_flips > 3 ~ "erratic",
        !is.na(t0) & t0 <= 10 ~ "rapid",
        !is.na(t0) & t0 > 10 ~ "delayed",
        TRUE ~ "unclassified"
      )
    )
}


prepare_plot_data <- function(strategy_data, classified, step_fits) {
  strategy_data %>%
    left_join(classified %>% select(participant_id, model_class), by = "participant_id") %>%
    left_join(step_fits, by = "participant_id") %>%
    mutate(predicted_step = ifelse(trial_after_rot >= t0, step_size, 0))
}

compute_mean_plot <- function(plot_data) {
  plot_data %>%
    group_by(model_class, trial_after_rot) %>%
    summarise(mean_aim = mean(aimdeviation_deg, na.rm = TRUE), .groups = "drop")
}

plot_step_model <- function(mean_data, plot_data) {
  ggplot() +
    geom_line(data = mean_data, aes(x = trial_after_rot, y = mean_aim), color = "grey", size = 1.1) +
    geom_line(data = plot_data, aes(x = trial_after_rot, y = predicted_step, group = participant_id),
              color = "red", alpha = 0.3) +
    facet_wrap(~model_class) +
    labs(
      x = "Trial after rotation",
      y = "Aim deviation (deg)",
      title = "Informed Step Model Fits"
    ) +
    theme_minimal() +
    theme(panel.background = element_blank(), panel.grid = element_blank())
}


informedResults <- function(strat_data) {
  suppressWarnings({ 
    strategy_data <- preprocess_strategy_data(strat_data)
    trial_features <- extract_trial_features(strategy_data)
    trial_summary  <- summarise_features(trial_features)
    
    step_fits <- run_step_fits(strategy_data)
    early_sd <- compute_early_sd(strategy_data)
    sign_flips <- compute_sign_flips(strategy_data)
    classified <- classify_participants(step_fits, early_sd, sign_flips)
    
    plot_data <- prepare_plot_data(strategy_data, classified, step_fits)
    mean_plot_data <- compute_mean_plot(plot_data)
  })
  
  plot_step_model(mean_plot_data, plot_data)
}





#assess agreement

InformedTable <- function(classified, rf_labels, unsupervised_labels) {
  all_labels <- classified %>%
    select(participant_id, step_model = model_class) %>%
    inner_join(rf_labels, by = "participant_id") %>%
    inner_join(unsupervised_labels, by = "participant_id") %>%
    mutate(
      step_model = factor(step_model),
      predicted_label = factor(predicted_label),
      unsupervised_cluster = factor(unsupervised_cluster)
    )
  table(all_labels$step_model, all_labels$predicted_label)
}



InformedAgreement <- function() {
  
  print(chisq.test(all_labels$step_model, all_labels$predicted_label))
  print(chisq.test(all_labels$step_model, all_labels$unsupervised_cluster))
  
}


