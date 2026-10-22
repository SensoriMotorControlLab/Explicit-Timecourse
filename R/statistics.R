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




#is cluster faster?

get_reach_match_trial <- function(tolerance = 2.0) {
  pca_df <- runHClust()
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  
  rotated_full <- strategy_data %>%
    filter(trial_type == "rotated") %>%
    left_join(
      pca_df %>% select(participant_id, cluster_label),
      by = "participant_id"
    ) %>%
    # Create relative trial index within the rotated phase (1 to 120)
    group_by(participant_id) %>%
    arrange(cutrial_no) %>%
    mutate(rotated_trial_no = row_number()) %>%
    ungroup()
  
  final_means <- rotated_full %>%
    group_by(participant_id) %>%
    arrange(rotated_trial_no) %>%
    slice_tail(n = 16) %>%
    summarise(
      final_reach = mean(reachdeviation_deg, na.rm = TRUE),
      .groups = "drop"
    )
  
  match_trials <- rotated_full %>%
    left_join(final_means, by = "participant_id") %>%
    group_by(participant_id, cluster_label) %>%
    arrange(rotated_trial_no) %>%
    filter(abs(reachdeviation_deg - final_reach) <= tolerance | reachdeviation_deg >= final_reach) %>%
    slice_head(n = 1) %>% 
    summarise(
      first_match_trial = rotated_trial_no,
      final_reach = first(final_reach),
      .groups = "drop"
    )
  
  return(match_trials)
}


match_df_cluster <- get_reach_match_trial(tolerance = 2.0)

print("--- ANOVA: Trial to Reach Asymptote by Cluster ---")
match_anova <- aov(first_match_trial ~ cluster_label, data = match_df_cluster)
print(summary(match_anova))
print(TukeyHSD(match_anova))


match_df_cluster_plot <- match_df_cluster %>%
  mutate(
    cluster_label = case_when(
      cluster_label == "3" ~ "One-Step",
      cluster_label == "2" ~ "Incremental",
      cluster_label == "1" ~ "Exploratory",
      TRUE ~ as.character(cluster_label)
    ),
    cluster_label = factor(cluster_label, levels = c("One-Step", "Incremental", "Exploratory"))
  )

cluster_colors <- c(
  "One-Step"    = "orchid",
  "Incremental" = "cyan",
  "Exploratory" = "#EAA178"
)

ggplot(match_df_cluster_plot, aes(x = cluster_label, y = first_match_trial, fill = cluster_label)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.5) +
  geom_jitter(width = 0.15, size = 2.5, alpha = 0.7, color = "black") +
  stat_summary(
    fun = mean, 
    geom = "point", 
    shape = 18, 
    size = 4, 
    color = "darkred"
  ) +
  scale_fill_manual(values = cluster_colors) +
  scale_y_continuous(limits = c(1, 120), breaks = seq(0, 120, by = 20)) +
  labs(
    x = "Phenotype Cluster",
    y = "Trial to Reach Asymptote (Rotated Phase Trial)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text = element_text(color = "black")
  )



# is startegy use faster?

strategy_time <- function(tolerance = 2.0) {
  strategy_df <- getStrategies() 
  strategy_data <- read.csv("data/total_group_data.csv")
  
  rotated_full <- strategy_data %>%
    filter(trial_type == "rotated") %>%
    left_join(
      strategy_df %>% select(participant_id, strategy),
      by = "participant_id"
    ) %>%
    group_by(participant_id) %>%
    arrange(cutrial_no) %>%
    mutate(rotated_trial_no = row_number()) %>%
    ungroup()
  
  final_means <- rotated_full %>%
    group_by(participant_id) %>%
    arrange(rotated_trial_no) %>%
    slice_tail(n = 16) %>%
    summarise(
      final_reach = mean(reachdeviation_deg, na.rm = TRUE),
      .groups = "drop"
    )
  
  match_trials <- rotated_full %>%
    left_join(final_means, by = "participant_id") %>%
    group_by(participant_id, strategy) %>%
    arrange(rotated_trial_no) %>%
    filter(abs(reachdeviation_deg - final_reach) <= tolerance | reachdeviation_deg >= final_reach) %>%
    slice_head(n = 1) %>% 
    summarise(
      first_match_trial = rotated_trial_no,
      final_reach = first(final_reach),
      .groups = "drop"
    )
  
  return(match_trials)
}

# Run Strategy extraction & stats
strategy_df_match <- strategy_time(tolerance = 2.0)

print("--- ANOVA: Trial to Reach Asymptote by Strategy ---")
strategy_anova <- aov(first_match_trial ~ strategy, data = strategy_df_match)
print(summary(strategy_anova))

# Plot
plot_match_trials_boxplot <- function(strategy_df) {
  plot_df <- strategy_df %>%
    mutate(
      strategy = ifelse(is.na(strategy), "No", strategy),
      strategy = factor(strategy, levels = c("No", "Yes"))
    )
  
  ggplot(plot_df, aes(x = strategy, y = first_match_trial, fill = strategy)) +
    geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.5) +
    geom_jitter(width = 0.15, size = 2.5, alpha = 0.7, color = "black") +
    stat_summary(
      fun = mean, 
      geom = "point", 
      shape = 18, 
      size = 4, 
      color = "darkred"
    ) +
    scale_fill_manual(values = c("No" = "#56B4E9", "Yes" = "#E69F00")) +
    scale_y_continuous(limits = c(1, 120), breaks = seq(0, 120, by = 20)) +
    labs(
      title = "",
      x = "Strategy Used",
      y = "Trial to Reach Asymptote (Rotated Phase Trial)"
    ) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "none",
      axis.text = element_text(color = "black")
    )
}

plot_match_trials_boxplot(strategy_df_match)