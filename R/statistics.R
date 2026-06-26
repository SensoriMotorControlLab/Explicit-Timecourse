#does aim magnitude differ across rotation groups?
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



#does final aligned and first rotated differ from 0 (or -5 to 5 threshold)?
zeroT <- function() {
  strat_data <- read.csv("data/strategy_only_participants.csv")
  results <- list()
  
  last_aligned <- strat_data %>%
    filter(trial_type == "aligned") %>%
    group_by(participant_id) %>%
    arrange(cutrial_no, .by_group = TRUE) %>%
    slice_tail(n = 16) %>%
    summarise(mean_aim = mean(aimdeviation_deg, na.rm = TRUE))
  
  results[["aligned"]] <- t.test(last_aligned$mean_aim, mu = 0, alternative = "less")

  first_rotated <- strat_data %>%
    filter(trial_type == "rotated") %>%
    group_by(participant_id) %>%
    arrange(cutrial_no, .by_group = TRUE) %>%
    slice_head(n = 16) %>%
    summarise(mean_aim = mean(aimdeviation_deg, na.rm = TRUE))
  
  results[["rotated"]] <- t.test(first_rotated$mean_aim, mu = 0, alternative = "greater")
  
  return(results)
}

#does rotated aim differ from ideal angle?
idealT <- function() {
  strat_data <- read.csv("data/strategy_only_participants.csv")
  
  final_rotated <- strat_data %>%
    filter(trial_type == "rotated") %>%
    group_by(participant_id, rotation) %>%
    arrange(cutrial_no, .by_group = TRUE) %>%
    slice_tail(n = 8) %>%
    summarise(
      mean_rotated = mean(aimdeviation_deg, na.rm = TRUE),
      .groups = "drop"
    )
  
  results <- final_rotated %>%
    group_by(rotation) %>%
    group_modify(~ {
      rt <- unique(.y$rotation)   
      
      t_res <- t.test(.x$mean_rotated, mu = rt)
      
      tibble(
        t_stat = t_res$statistic,
        df = t_res$parameter,
        p_value = t_res$p.value
      )
    })
  
  results
}



###2 way anovas comparing rotation & strategy use
twoAnova <-function () {
  strategy_df <- getStrategies()

  # We select just participant_id, rotation, and strategy from strategy_df
  strategy_data <- total_learners_data %>%
    left_join(
      strategy_df %>% select(participant_id, rotation, strategy), 
      by = c("participant_id", "rotation")
    )
  
  # 2. Filter for rotated trials and grab the last 8 trials
  final_trials_data <- strategy_data %>%
    filter(trial_type == "rotated") %>%
    group_by(participant_id, rotation, strategy) %>%   
    slice_tail(n = 8) %>%
    ungroup()
  
  anova_ready_data <- final_trials_data %>%
    group_by(participant_id, rotation, strategy) %>%
    summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE),
              .groups = 'drop')
  

  anova_ready_data$rotation <- factor(anova_ready_data$rotation)
  anova_ready_data$strategy <- factor(anova_ready_data$strategy)
  

  print(table(anova_ready_data$rotation, anova_ready_data$strategy))
  cat("\n")
  

  res_aov <- aov(mean_reach_dev ~ rotation * strategy, data = anova_ready_data)
  
  print("--- ANOVA RESULTS ---")
  print(summary(res_aov))
  return(res_aov)
}


postHoc <- function () {
 library(emmeans)
res_aov <-  twoAnova()
emmeans(res_aov, pairwise ~ strategy | rotation)

}  
  
#does final reach differ by strategy type
aovClusterData <- function() {
  pca_df <- runHClust()
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  
  rotated <- strategy_data %>%
    filter(trial_type == "rotated") %>%
    group_by(participant_id) %>%
    arrange(cutrial_no) %>%
    slice_tail(n = 16) %>%
    ungroup() %>%
    left_join(
      pca_df %>% select(participant_id, cluster_label),
      by = "participant_id"
    )
  
  rotated
}

cluster_data <- aovClusterData()

cluster_anova_df <- cluster_data %>%
  group_by(participant_id, cluster_label) %>%
  summarise(
    final_reach = mean(reachdeviation_deg, na.rm = TRUE), #/aimdev
    .groups = "drop"
  )

print("--- Phenotype Cluster ANOVA ---")
print(summary(aov(final_reach ~ cluster_label, data = cluster_anova_df)))

tukey <- aov(final_reach ~ cluster_label, data = cluster_anova_df)
TukeyHSD(tukey)

