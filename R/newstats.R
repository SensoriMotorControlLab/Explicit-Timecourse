library(tidyr)
#load data
strat_data <- read.csv("data/strategy_only_participants.csv")


getStep <- function(strat_data) {
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
    mutate(cutrial_no = row_number())
  
  first_step_over_10 <- first_50_rotated %>%
    filter(aimdeviation_deg > 10) %>%
    group_by(participant_id) %>%
    slice_min(order_by = cutrial_no, n = 1)
  
  result_table <- dplyr::select(first_step_over_10,
                                participant_id, rotation, cutrial_no, aimdeviation_deg)
  
  return(result_table)
}



meanStep <- function(result_table = getStep(strat_data)) {
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
  
  return(mean_step_data)
}

meanStep(result_table)


#log reg
logAnalysis <- function () {  
  strategy_ids <- ci_compare$participant_id[ci_compare$strategy %in% c("Yes")]
  
  
  total_learners_data <- total_learners_data %>%
    mutate(strategy = ifelse(participant_id %in% strategy_ids, "Yes", "No"))
  
  total_learners_data$strategy <- ifelse(total_learners_data$strategy == "Yes", 1,
                                         ifelse(total_learners_data$strategy == "No", 0, NA))
  
   grouped_strategy_data <- total_learners_data %>%
    mutate(group = ifelse(participant_id %in% strategy_ids, "Yes", "No"))
  
  
  logit_data <- grouped_strategy_data %>%
    distinct(participant_id, rotation, group) %>%
    mutate(group = factor(group, levels = c("No", "Yes")),
           rotation = factor(rotation))
  
  model <- glm(group ~ rotation, data = logit_data, family = binomial)
  summary(model)
}

#aov for mean of last 16 rotated trials
aimAOV <- function () {
  strategy_ids <- ci_compare$participant_id[ci_compare$strategy %in% c("Yes")]
  
  
  total_learners_data <- total_learners_data %>%
    mutate(strategy = ifelse(participant_id %in% strategy_ids, "Yes", "No"))
  
  total_learners_data$strategy <- ifelse(total_learners_data$strategy == "Yes", 1,
                                         ifelse(total_learners_data$strategy == "No", 0, NA))
  
  grouped_strategy_data <- total_learners_data %>%
    mutate(group = ifelse(participant_id %in% strategy_ids, "Yes", "No"))
  
  
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
  strategy_ids <- ci_compare$participant_id[ci_compare$strategy %in% c("Yes")]
  
  
  total_learners_data <- total_learners_data %>%
    mutate(strategy = ifelse(participant_id %in% strategy_ids, "Yes", "No"))
  
  total_learners_data$strategy <- ifelse(total_learners_data$strategy == "Yes", 1,
                                         ifelse(total_learners_data$strategy == "No", 0, NA))
  
  grouped_strategy_data <- total_learners_data %>%
    mutate(group = ifelse(participant_id %in% strategy_ids, "Yes", "No"))
  
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
startTrialAOV <- function(result_table) {
  anova_result <- aov(cutrial_no ~ factor(rotation), data = result_table)
  return(summary(anova_result))
}

#Descriptive Stats for aiming deviation of strategy-users (last 16 rotated trials)


########T-tests


#does final aligned and first rotated differ from 0 (or -5 to 5 threshold)?
zeroT <- function(strat_data) {
  results <- list()
  
  last_aligned <- strat_data %>%
    filter(trial_type.x == "aligned") %>%
    group_by(participant_id) %>%
    arrange(cutrial_no, .by_group = TRUE) %>%
    slice_tail(n = 16) %>%
    summarise(mean_aim = mean(aimdeviation_deg, na.rm = TRUE))
  
  results[["aligned"]] <- t.test(last_aligned$mean_aim, mu = -1)

  first_rotated <- strat_data %>%
    filter(trial_type.x == "rotated") %>%
    group_by(participant_id) %>%
    arrange(cutrial_no, .by_group = TRUE) %>%
    slice_head(n = 16) %>%
    summarise(mean_aim = mean(aimdeviation_deg, na.rm = TRUE))
  
  results[["rotated"]] <- t.test(first_rotated$mean_aim, mu = 0)
  
  return(results)
}

#does rotated differ from ideal angle?

  idealT <- function(strat_data) {
    
    final_aligned <- strat_data %>%
      filter(trial_type.x == "aligned") %>%
      group_by(participant_id, rotation) %>%
      arrange(cutrial_no, .by_group = TRUE) %>%
      slice_tail(n = 8) %>%
      summarise(mean_aligned = mean(aimdeviation_deg, na.rm = TRUE), .groups = "drop")
    
    
    final_rotated <- strat_data %>%
      filter(trial_type.x == "rotated") %>%
      group_by(participant_id, rotation) %>%
      arrange(cutrial_no, .by_group = TRUE) %>%
      slice_tail(n = 8) %>%
      summarise(mean_rotated = mean(aimdeviation_deg, na.rm = TRUE), .groups = "drop")
    
    combined <- inner_join(final_aligned, final_rotated,
                           by = c("participant_id", "rotation"))
  
    results <- combined %>%
      group_by(rotation) %>%
      summarise(
        t_stat = t.test(mean_rotated, mean_aligned, paired = TRUE)$statistic,
        df = t.test(mean_rotated, mean_aligned, paired = TRUE)$parameter,
        p_value = t.test(mean_rotated, mean_aligned, paired = TRUE)$p.value,
        .groups = "drop"
      )
    
    return(results)
  }