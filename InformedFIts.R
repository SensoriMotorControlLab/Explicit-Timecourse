##informed labeling function fitting


participant_first_aim <- strategy_data_clusters %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  summarise(
    first_trial_above7 = cutrial_no[which(aimdeviation_deg > 7)[1]],
    aim_at_first_trial = aimdeviation_deg[which(aimdeviation_deg > 7)[1]],
    strategy_type      = first(cluster),
    .groups = "drop"
  ) %>%
  mutate(strategy_type = case_when(
    strategy_type == 1 ~ "erratic",
    strategy_type == 2 ~ "delayed",
    strategy_type == 3 ~ "rapid",
    
  ))

View(participant_first_aim)



                                #----- rapid -----#
### perhaps fit a step function of above 7 deg early in aimdeviation_deg (within 10 trials)?
# - mean time course (based on actual data from unsupervised), small sd
# - mean aim(based on actual data), small sd too


meanaimRapid <- mean(participant_first_aim %>%
                       filter(strategy_type == "rapid") %>%
                       pull(aim_at_first_trial),
                     na.rm = TRUE
) #24.83

sdaimRapid <- sd(participant_first_aim %>%
                   filter(strategy_type == "rapid") %>%
                   pull(aim_at_first_trial),
                 na.rm = TRUE
) #27.21

meanchangeRapid <- mean(participant_first_aim %>%
                          filter(strategy_type == "rapid") %>%
                          pull(first_trial_above7),
                        na.rm = TRUE
) #6.13

sdchangeRapid <- sd(participant_first_aim %>%
                      filter(strategy_type == "rapid") %>%
                      pull(first_trial_above7),
                    na.rm = TRUE
) #3.72

                               #----- delayed -----#

# Same as rapid but t0 should be above 10

meanaimDelayed <- mean(participant_first_aim %>%
                       filter(strategy_type == "delayed") %>%
                       pull(aim_at_first_trial),
                     na.rm = TRUE
) #16.73

sdaimDelayed <- sd(participant_first_aim %>%
                   filter(strategy_type == "delayed") %>%
                   pull(aim_at_first_trial),
                 na.rm = TRUE
) #9.31

meanchangeDelayed <- mean(participant_first_aim %>%
                          filter(strategy_type == "delayed") %>%
                          pull(first_trial_above7),
                        na.rm = TRUE
) #trial 32.14

sdchangeDelayed <- sd(participant_first_aim %>%
                      filter(strategy_type == "delayed") %>%
                      pull(first_trial_above7),
                    na.rm = TRUE
) #12.90


                           #----- erratic -----#
#there will be flips in signs

meanaimErratic <- mean(participant_first_aim %>%
                         filter(strategy_type == "erratic") %>%
                         pull(aim_at_first_trial),
                       na.rm = TRUE
) # 100.33

sdaimErratic <- sd(participant_first_aim %>%
                     filter(strategy_type == "erratic") %>%
                     pull(aim_at_first_trial),
                   na.rm = TRUE
) # 31.63

meanchangeErratic<- mean(participant_first_aim %>%
                            filter(strategy_type == "erratic") %>%
                            pull(first_trial_above7),
                          na.rm = TRUE
) # 3.67

sdchangeErratic <- sd(participant_first_aim %>%
                        filter(strategy_type == "erratic") %>%
                        pull(first_trial_above7),
                      na.rm = TRUE
) #1.53


#a point where they learned. it (one parameter) - time change
#second p arameter, is how mucnh they learned, like s tep. function
#increase weight,if. sd before step is. super high
#have aligned


fit_step_model_onset <- function(df, threshold = 7) {
  # Find first trial where aim deviation exceeds threshold
  first_jump <- which(df$aimdeviation_deg >= threshold)[1]
  
  if (is.na(first_jump)) {
    return(tibble(t0 = NA, step_size = NA))
  }
  
  # Step size: difference between trial before and after the jump
  # (optional, you can define it as first jump minus baseline mean)
  baseline <- mean(df$aimdeviation_deg[1:(first_jump-1)], na.rm = TRUE)
  step_size <- df$aimdeviation_deg[first_jump] - baseline
  
  tibble(
    t0 = df$trial_after_rot[first_jump],
    step_size = step_size
  )
}

# Apply to your dataset
step_fits <- strategy_data %>%
  group_by(participant_id) %>%
  group_modify(~ fit_step_model_onset(.x))


early_sd <- strategy_data %>%
  filter(trial_after_rot <= 15) %>%
  group_by(participant_id) %>%
  summarise(sd_early = sd(aimdeviation_deg, na.rm = TRUE), .groups = "drop")

sign_flips_df <- strategy_data %>%
  group_by(participant_id) %>%
  arrange(trial_after_rot, .by_group = TRUE) %>%
  summarise(
    sign_flips = sum(lag(aimdeviation_deg >= -7, default = TRUE) & aimdeviation_deg < -7, na.rm = TRUE),
    .groups = "drop"
  )

classified <- step_fits %>%
  left_join(early_sd, by = "participant_id") %>%
  left_join(sign_flips_df, by = "participant_id") %>%  # sign_flips_df has participant_id and sign_flips
  mutate(
    model_class = case_when(
      sd_early > 10 & sign_flips > 3 ~ "erratic",  # only this single erratic condition
      t0 <= 10 ~ "rapid",
      t0 > 10  ~ "delayed",
      TRUE ~ "unclassified"
    )
  )

classified
table(classified$model_class)


library(dplyr)
library(ggplot2)

# compute the mean aim per trial
mean_plot_data <- plot_data %>%
  group_by(model_class, trial_after_rot) %>%
  summarise(mean_aim = mean(aimdeviation_deg), .groups = "drop")

ggplot() +
  # mean actual aiming
  geom_line(data = mean_plot_data, aes(x = trial_after_rot, y = mean_aim), color = "grey", size = 1.1) +
  # all individual predicted steps
  geom_line(data = plot_data, aes(x = trial_after_rot, y = predicted_step, group = participant_id),
            color = "red", alpha = 0.3) +
  facet_wrap(~model_class) +
  labs(
    x = "Trial after rotation",
    y = "Aim deviation (deg)",
    title = "Informed Step Model Fits"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),     
  )




#assess agreement
all_labels <- classified %>%
  select(participant_id, step_model = model_class) %>%
  inner_join(rf_labels, by = "participant_id") %>%
  inner_join(unsupervised_labels, by = "participant_id") %>%
  mutate(
    step_model = factor(step_model),
    predicted_label = factor(predicted_label),
    unsupervised_cluster = factor(unsupervised_cluster)
  )

all_labels

table(all_labels$step_model, all_labels$predicted_label)


chisq.test(all_labels$step_model, all_labels$predicted_label)
#X-squared = 91.73, df = 4, p-value < 2.2e-16
chisq.test(all_labels$step_model, all_labels$unsupervised_cluster)
#X-squared = 43.336, df = 4, p-value = 8.811e-09



