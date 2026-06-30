total_learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors =FALSE)

total_learners_data <- total_group_data %>%
  semi_join(learner_id, by = c("rotation", "participant_id")) %>%
  select(participant_id, cutrial_no, aimdeviation_deg, reachdeviation_deg, rotation, trial_type, group)

flag_outliers <- function(x) {
  m <- mean(x, na.rm = TRUE)
  abs(x) > 3 * abs(m)
}

cleaned_data <- total_learners_data %>%
  group_by(trial_type) %>%
  mutate(is_outlier = flag_outliers(reachdeviation_deg)) %>%
  ungroup() 
  

#####anova 1#### do asymptotic reaches differ depending on rotation size?

baseline_means <- cleaned_data %>%
  filter(trial_type == "aligned") %>%
  group_by(rotation, participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 8) %>%
  summarise(
    baseline_reach = mean(reachdeviation_deg, na.rm = TRUE),
    .groups = "drop"
  )


rotated_means <- cleaned_data %>%
  filter(trial_type == "rotated") %>%
  group_by(rotation, participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 16) %>%
  summarise(
    asymptotic_reach = mean(reachdeviation_deg, na.rm = TRUE),
    .groups = "drop"
  )


corrected_means <- rotated_means %>%
  left_join(baseline_means,
            by = c("rotation", "participant_id")) %>%
  mutate(
    corrected_reach = asymptotic_reach - baseline_reach
  )

# ANOVA
anova_res <- aov(corrected_reach ~ factor(rotation),
                 data = corrected_means)

summary(anova_res)
TukeyHSD(anova_res)


####anova 2##### Do participants detect and start compensating earlier for small rotations than large ones?”

baseline_means <- cleaned_data %>%
  filter(trial_type == "aligned") %>%
  group_by(participant_id, rotation) %>%
  summarise(mean_aligned = mean(reachdeviation_deg, na.rm = TRUE), .groups = "drop")

rotated_trials <- cleaned_data %>%
  filter(trial_type == "rotated") %>%
  left_join(baseline_means, by = c("participant_id", "rotation")) %>%
  group_by(participant_id, rotation) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  summarise(
    first_change_trial = cutrial_no[which(abs(reachdeviation_deg - mean_aligned) > 5)[1]],
    .groups = "drop"
  )

rotated_trials %>%
  group_by(rotation) %>%
  summarise(n = n())

# Run one-way ANOVA 
anova_res <- aov(first_change_trial ~ factor(rotation), data = rotated_trials)
summary(anova_res)
TukeyHSD(anova_res)



#MAIN ANOVA

###2 way anovas comparing reaches to rotation & strategy use
twoAnova <- function() {
  
  strategy_df <- getStrategies()
  
  strategy_data <- total_learners_data %>%
    left_join(
      strategy_df %>% select(participant_id, rotation, strategy),
      by = c("participant_id", "rotation")
    )
  
  # Baseline:
  baseline_data <- strategy_data %>%
    filter(trial_type == "aligned") %>%
    group_by(participant_id, rotation, strategy) %>%
    arrange(cutrial_no, .by_group = TRUE) %>%
    slice_tail(n = 8) %>%
    summarise(
      baseline_reach = mean(reachdeviation_deg, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Reaches: final 16 rotated trials
  asymptotic_data <- strategy_data %>%
    filter(trial_type == "rotated") %>%
    group_by(participant_id, rotation, strategy) %>%
    arrange(cutrial_no, .by_group = TRUE) %>%
    slice_tail(n = 16) %>%
    summarise(
      asymptotic_reach = mean(reachdeviation_deg, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Baseline correction
  anova_ready_data <- asymptotic_data %>%
    left_join(
      baseline_data,
      by = c("participant_id", "rotation", "strategy")
    ) %>%
    mutate(
      corrected_reach = asymptotic_reach - baseline_reach
    )
  
  anova_ready_data$rotation <- factor(anova_ready_data$rotation)
  anova_ready_data$strategy <- factor(anova_ready_data$strategy)
  
  print(table(anova_ready_data$rotation,
              anova_ready_data$strategy))
  
  res_aov <- aov(
    corrected_reach ~ rotation * strategy,
    data = anova_ready_data
  )
  
  print(summary(res_aov))
  
  return(res_aov)
}