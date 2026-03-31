library(tidyr)
#load data
#strat_data <- read.csv("data/strategy_only_participants.csv")


getStep <- function() {
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
    mutate(cutrial_no = row_number())
  
  first_step_over_10 <- first_50_rotated %>%
    filter(aimdeviation_deg > 10) %>%
    group_by(participant_id) %>%
    slice_min(order_by = cutrial_no, n = 1)
  
  result_table <- dplyr::select(first_step_over_10,
                                participant_id, rotation, cutrial_no, aimdeviation_deg)
  
  return(result_table)
}
#result_table <- getStep(strat_data)


meanStep <- function(result_table = getStep()) { #this is fine
  strat_data <- read.csv("data/strategy_only_participants.csv")
  mean_transitions <- result_table %>%
    group_by(rotation, participant_id) %>%
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
  
  View(meanStep())
  return(mean_step_data)
}



#log reg
logAnalysis <- function () {  
  #LOAD CI COMPARE HERE FROM GET STRATEGIES
  total_learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors =FALSE)
   strategy_df <- getStrategies()
   
   strategy_ids <- strategy_df$participant_id[strategy_df$strategy %in% c("Yes")]
  
  
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
  total_learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors =FALSE)
  ci_compare <- getStrategies()
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
  total_learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors =FALSE)
  strategy_df <- getStrategies()
  strategy_ids <- strategy_df$participant_id[strategy_df$strategy %in% c("Yes")]
  
  
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
startTrialAOV <- function() {
  result_table <- getStep()
  anova_result <- aov(cutrial_no ~ factor(rotation), data = result_table)
  return(summary(anova_result))
}

#Descriptive Stats for aiming deviation of strategy-users (last 16 rotated trials)


########T-tests


#does final aligned and first rotated differ from 0 (or -5 to 5 threshold)?
zeroT <- function() {
  strat_data <- read.csv("data/strategy_only_participants.csv")
  results <- list()
  
  last_aligned <- strat_data %>%
    filter(trial_type.x == "aligned") %>%
    group_by(participant_id) %>%
    arrange(cutrial_no, .by_group = TRUE) %>%
    slice_tail(n = 16) %>%
    summarise(mean_aim = mean(aimdeviation_deg, na.rm = TRUE))
  
  results[["aligned"]] <- t.test(last_aligned$mean_aim, mu = 0, alternative = "less")

  first_rotated <- strat_data %>%
    filter(trial_type.x == "rotated") %>%
    group_by(participant_id) %>%
    arrange(cutrial_no, .by_group = TRUE) %>%
    slice_head(n = 16) %>%
    summarise(mean_aim = mean(aimdeviation_deg, na.rm = TRUE))
  
  results[["rotated"]] <- t.test(first_rotated$mean_aim, mu = 0, alternative = "greater")
  
  return(results)
}

#does rotated differ from ideal angle?

idealT <- function() {
  strat_data <- read.csv("data/strategy_only_participants.csv")
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




## clusters

rotationEffect <- function () {
  proportions_plot <- pcaBar()

  
  proportions_plot$cluster_label <- droplevels(proportions_plot$cluster_label)
  
  table_data <- xtabs(n_participants ~ rotation + cluster_label, 
                      data = proportions_plot)
  
  #monte carlo simulation?? bc some cells are less than 5 and fishers is hard to use on a big contigency table
  
  fisher.test(table_data, simulate.p.value = TRUE, B = 100000)

} # p-value = 0.00018

#more specific than just cluster type: stats that are good to have

## does learning phase length differ by rotation group?

lengthStats <- function () {
  strat_data <- read.csv("data/strategy_only_participants.csv")
  model_df <- xgRun()
  rotation_lookup <- unique(strat_data[, c("participant_id", "rotation")])
  model_df <- merge(model_df, rotation_lookup, by = "participant_id", all.x = TRUE)
  
  model_df$learning_length <- model_df$pred_end - model_df$pred_start
  anova_result <- aov(learning_length ~ factor(rotation), data = model_df)
  summary(anova_result)
  
  
  summary_df <- model_df %>%
    group_by(rotation.x) %>%
    summarise(
      mean_length = mean(learning_length, na.rm = TRUE),
      se_length = sd(learning_length, na.rm = TRUE)/sqrt(n())
    )
  
  # Bar plot with error bars
  ggplot(summary_df, aes(x = rotation.x, y = mean_length, fill = rotation.x)) +
    geom_bar(stat = "identity", color = "black", width = 0.6) +
    geom_errorbar(aes(ymin = mean_length - se_length, ymax = mean_length + se_length),
                  width = 0.2) +
    labs(
      title = "Learning Phase Length by Rotation Group",
      x = "Rotation Group",
      y = "Mean Learning Length (trials)"
    ) +
    coord_cartesian(ylim = c(0, 35)) +
    theme_minimal() +
    theme(legend.position = "none")
  
} #yes p =0.038







## does learning phase sd differ by rotation group?
varStats <- function () {
  strat_data <- read.csv("data/strategy_only_participants.csv")
  model_df <- xgRun()
  rotation_lookup <- unique(strat_data[, c("participant_id", "rotation.x")])
  model_df <- merge(model_df, rotation_lookup, by = "participant_id", all.x = TRUE)
  

  anova_result <- aov(sd_dev ~ factor(rotation), data = model_df)
  summary(anova_result)
} #yes p<0.001


