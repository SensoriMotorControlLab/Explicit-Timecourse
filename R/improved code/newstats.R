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

meanStep <- function () { 
  mean_transitions <- result_table %>%
    group_by(rotation) %>%
    summarise(
      mean_cut = round(mean(cutrial_no)),
      mean_aim = mean(aimdeviation_deg),
      .groups = "drop"
    )
  
  mean_step_data <- mean_transitions %>%
    rowwise() %>%
    mutate(
      trial = list(-8:50),
      aim_deviation = list(ifelse(-8:50 < mean_cut, 0, mean_aim))
    ) %>%
    unnest(c(trial, aim_deviation))
}  

#log reg
logAnalysis <- function () {  
  grouped_strategy_data <- total_learners_data %>%
    mutate(group = ifelse(participant_id %in% strategy_ids, "Yes", "No"))
  
  grouped_strategy_data <- grouped_strategy_data %>%
    mutate(group = ifelse(participant_id == "4eeaee", "Yes", group)) 
  
  logit_data <- grouped_strategy_data %>%
    distinct(participant_id, rotation, group) %>%
    mutate(group = factor(group, levels = c("No", "Yes")),
           rotation = factor(rotation))
  
  model <- glm(group ~ rotation, data = logit_data, family = binomial)
  summary(model)
}

#aov for mean of last 16 rotated trials
aimAOV <- function () {
  plot_mean_aim_data <- grouped_strategy_data %>%
    filter(cutrial_no %in% c(193:208, 217:232)) %>%
    group_by(rotation, participant_id, group) %>%
    summarise(mean_aim = mean(aimdeviation_deg, na.rm = TRUE), .groups = "drop")
  
  plot_mean_aim_data$rotation <- factor(plot_mean_aim_data$rotation)
  
  anova_result <- aov(mean_aim ~ rotation, data = plot_mean_aim_data)
  summary(anova_result)
  TukeyHSD(anova_result)
}

aimvarAOV <- function () {
  plot_mean_aim_data <- grouped_strategy_data %>%
    filter(cutrial_no %in% c(193:208, 217:232)) %>%
    group_by(rotation, participant_id, group) %>%
    summarise(sd = sd(aimdeviation_deg, na.rm = TRUE), .groups = "drop")
  
  plot_mean_aim_data$rotation <- factor(plot_mean_aim_data$rotation)
  
  anova_result <- aov(sd ~ rotation, data = plot_mean_aim_data)
  summary(anova_result)
  TukeyHSD(anova_result)
}


#Is the average trial number where participants start using a strategy different across the rotation groups?
summary(aov(cutrial_no ~ factor(rotation), data = result_table))


#Descriptive Stats for aiming deviation of strategy-users (last 16 rotated trials)


########T-tests


#does final aligned differ from 0 (or -5 to 5 threshold)?
zeroT <- function () {
  results <- list()
  last_aligned <- strat_data %>%
    filter(trial_type.x == "aligned") %>% 
    group_by(participant_id) %>%      
    arrange(cutrial_no, .by_group = TRUE) %>%  
    slice_tail(n = 16) %>%            # take last 16 per participant
    summarise(mean_aim = mean(aimdeviation_deg, na.rm = TRUE))
  
  results[["aligned"]] <- t.test(last_aligned$mean_aim, mu = -1)
  
  
  #does first rotated differ from 0?
  first_rotated <-  subset(strat_data, 
                           (group == "Group 1" & cutrial_no >= 89 & cutrial_no <= 139) |
                             (group == "Group 2" & cutrial_no >= 113 & cutrial_no <= 163))
  
  first_rotated <- strat_data %>%
    filter(trial_type.x == "rotated") %>% 
    group_by(participant_id) %>%      
    arrange(cutrial_no, .by_group = TRUE) %>%  
    slice_head(n = 16) %>%            
    summarise(mean_aim = mean(aimdeviation_deg, na.rm = TRUE))
  
  results[["rotated"]] <- t.test(first_rotated$mean_aim, mu=-0.88)
  
  return(results)
}


#does rotated differ from ideal angle?

idealT <- function() {
  
  # get final 8 ALIGNED means per participant
  final_aligned <- strat_data %>%
    filter(trial_type.x == "aligned") %>%
    group_by(participant_id, rotation) %>%
    arrange(cutrial_no, .by_group = TRUE) %>%
    slice_tail(n = 8) %>%
    summarise(mean_aligned = mean(aimdeviation_deg, na.rm = TRUE),
              .groups = "drop")
  
  # get final 8 ROTATED means per participant
  final_rotated <- strat_data %>%
    filter(trial_type.x == "rotated") %>%
    group_by(participant_id, rotation) %>%
    arrange(cutrial_no, .by_group = TRUE) %>%
    slice_tail(n = 8) %>%
    summarise(mean_rotated = mean(aimdeviation_deg, na.rm = TRUE),
              .groups = "drop")
  

  combined <- inner_join(final_aligned, final_rotated,
                         by = c("participant_id", "rotation"))
  
#paired t. test
  results <- combined %>%
    group_by(rotation) %>%
    summarise(
      ttest = list(t.test(mean_rotated, mean_aligned, paired = TRUE)),
      .groups = "drop"
    )
  
  return(results)
}

results <- idealT()

for (i in 1:nrow(results)) {
  cat("\n\n===== Rotation", results$rotation[i], "=====\n")
  print(results$ttest[[i]])
}

