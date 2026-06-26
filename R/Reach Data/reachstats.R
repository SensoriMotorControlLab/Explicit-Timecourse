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
rotated_means <- cleaned_data %>%
  filter(trial_type == "rotated") %>%
  group_by(rotation, participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 16) %>% 
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE), .groups = "drop")

anova_res <- aov(mean_reach_dev ~ factor(rotation), data = rotated_means)
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
