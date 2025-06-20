getStep <- function () {
  strat_data <- read.csv("data/strategy_only_participants.csv")

aligned_trials <- subset(strat_data, 
                         (group == "Group 1" & cutrial_no >= 81 & cutrial_no <= 88) |
                           (group == "Group 2" & cutrial_no >= 101 & cutrial_no <= 112))



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

print(result_table)
}


plotStep <- function () {
  library(tidyr)
  result_table$aligned <- 0
  
  df_steps <- result_table %>%
    rowwise() %>%
    mutate(
      trials = list(-8:50),
      aim_deviation = list(pmin(ifelse(-8:50 < cutrial_no, 0, aimdeviation_deg), 60))
    ) %>%
    unnest(c(trials, aim_deviation))
  
  # Plot
  ggplot(df_steps, aes(x = trials, y = aim_deviation, color = factor(rotation))) +
    geom_line(aes(group = participant_id), size = 0.8) +
    geom_vline(data = result_table, aes(xintercept = cutrial_no), linetype = "dashed", color = "NA") +
    labs(
      x = "Trial",
      y = "Aim Deviation (deg)",
      color = "Rotation",
      title = "Strategy Onset of Each Participant"
    ) +
    geom_vline(aes(xintercept = 0), linetype = "dashed", color = "grey60") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(),
    ) 
} 
  
  
  
  mean_transitions <- result_table %>%
    group_by(rotation) %>%
    summarise(
      mean_cut = round(mean(cutrial_no)),
      mean_aim = mean(aimdeviation_deg),
      .groups = "drop"
    )
  
  # Step 2: Build stepwise data: 0 before cut, step to mean_aim after
  mean_step_data <- mean_transitions %>%
    rowwise() %>%
    mutate(
      trial = list(-8:50),
      aim_deviation = list(ifelse(-8:50 < mean_cut, 0, mean_aim))
    ) %>%
    unnest(c(trial, aim_deviation))
  
  
  ggplot(mean_step_data, aes(x = trial, y = aim_deviation, color = factor(rotation))) +
    geom_line(size = 0.8) +
    geom_vline(aes(xintercept = 0), linetype = "dashed", color = "grey60") +  # Rotation start
    labs(
      x = "Trial",
      y = "Mean Aim Deviation (deg)",
      color = "Rotation Group",
      title = "Average Strategy Onset per Rotation Group"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),  # Remove all grid lines
      axis.line = element_line(),    # Add axis lines
      legend.position = "right"
    )
  
  
  #log reg
  grouped_strategy_data <- total_group_data %>%
    mutate(group = ifelse(participant_id %in% strategy_ids, "Yes", "No"))
  
  grouped_strategy_data <- grouped_strategy_data %>%
    mutate(group = ifelse(participant_id == "4eeaee", "Yes", group)) 
  
  logit_data <- grouped_strategy_data %>%
    distinct(participant_id, rotation, group) %>%
    mutate(group = factor(group, levels = c("No", "Yes")),
           rotation = factor(rotation))
  
  model <- glm(group ~ rotation, data = logit_data, family = binomial)
  summary(model)

  
  #aov for mean of last 8 rotated trials
  plot_mean_aim_data <- grouped_strategy_data %>%
    filter(cutrial_no %in% c(201:208, 225:232)) %>%
    group_by(rotation, participant_id, group) %>%
    summarise(mean_aim = mean(aimdeviation_deg, na.rm = TRUE), .groups = "drop")
  
  
   plot_mean_aim_data$rotation <- factor(plot_mean_aim_data$rotation)
  
  anova_result <- aov(mean_aim ~ rotation, data = plot_mean_aim_data)
  summary(anova_result)
  TukeyHSD(anova_result)
  

  
#Is the average trial number where participants start using a strategy different across the rotation groups?
  summary(aov(cutrial_no ~ factor(rotation), data = result_table))
  
  
#Descriptive Stats for aiming deviation of strategy-users (last 16 rotated trials)


########T-tests
#does aligned differ from 0?
first_aligned <- subset(strat_data$aimdeviation_deg, 
                        (strat_data$cutrial_no >= 1 & strat_data$cutrial_no <= 24))

t.test(first_aligned, mu=0)

#does rotated differ from 0?
first_rotated <-  subset(strat_data, 
                         (group == "Group 1" & cutrial_no >= 89 & cutrial_no <= 139) |
                           (group == "Group 2" & cutrial_no >= 113 & cutrial_no <= 163))

t.test(first_rotated$aimdeviation_deg, mu=0)
 
#does aligned differ from rotated?


last_aligned <-  subset(strat_data, 
                                      (group == "Group 1" & cutrial_no >= 81 & cutrial_no <= 88) |
                                        (group == "Group 2" & cutrial_no >= 105 & cutrial_no <= 112))

last_rotated <-  subset(strat_data, 
                         (group == "Group 1" & cutrial_no >= 89 & cutrial_no <= 96) |
                           (group == "Group 2" & cutrial_no >= 113 & cutrial_no <= 120))


t.test(last_aligned$aimdeviation_deg,last_rotated$aimdeviation_deg)
