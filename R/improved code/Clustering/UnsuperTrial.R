### Unsupervised learning with trial by trial features

library(zoo)
strategy_data <- strat_data
trial_features <- function(strategy_data, trial_type = "rotated", max_trial = 32) {
  strategy_data <- strategy_data %>%
    filter(trial_type.x == trial_type) %>%
    group_by(participant_id) %>%
    arrange(cutrial_no, .by_group = TRUE) %>%
    mutate(trial_after_rot = row_number() - 1) %>%
    ungroup()
  
  trial_features <- strategy_data %>%
    filter(trial_after_rot <= max_trial) %>%
    group_by(participant_id) %>%
    arrange(cutrial_no, .by_group = TRUE) %>%
    mutate(trial_of_change = { t <- which(aimdeviation_deg > 7 | aimdeviation_deg < -7)
    if (length(t) > 0) t[1] else NA }) %>%
    ungroup() %>%
    select(participant_id, trial_after_rot, aimdeviation_deg, trial_of_change)
  
  trial_summary <- trial_features %>%
    group_by(participant_id) %>%
    summarise(
      trial_of_change = ifelse(all(is.na(trial_of_change)), max_trial, mean(trial_of_change, na.rm = TRUE)),
      sd_aim = sd(aimdeviation_deg, na.rm = TRUE),
      mean_aim = mean(aimdeviation_deg, na.rm = TRUE),
      prop_negative = mean(aimdeviation_deg < -3, na.rm = TRUE)
    ) %>%
    mutate(trial_of_change = trial_of_change * 8)
  
  list(trial_features = trial_features, trial_summary = trial_summary)
}





pca_clustering <- function(trial_summary, n_clusters = 3) {
 
  
  trial_scaled <- trial_summary %>%
    select(mean_aim, sd_aim, trial_of_change, prop_negative) %>%
    scale(center = TRUE, scale = TRUE)
  
  pca <- prcomp(trial_scaled, center = TRUE, scale. = TRUE)
  trial_pca <- as.data.frame(pca$x[, 1:4])
  trial_pca$participant_id <- trial_summary$participant_id
  
  km <- kmeans(trial_pca[, 1:4], centers = n_clusters, nstart = 50)
  trial_pca$cluster <- factor(km$cluster)
  cluster_table <- trial_pca %>%
    group_by(cluster) %>%
    summarise(n = n(), .groups = "drop")
  
  list(trial_pca = trial_pca, cluster_table = cluster_table)
}


##plot
plot_strategy_clusters <- function(strategy_data, trial_pca) {
  
  strategy_data_clusters <- strategy_data %>%
    filter(trial_type.x == "rotated") %>%
    group_by(participant_id) %>%
    arrange(cutrial_no, .by_group = TRUE) %>%
    mutate(trial_after_rot = row_number() - 1) %>%
    ungroup() %>%
    left_join(trial_pca %>% select(participant_id, cluster), by = "participant_id")
  
  strategy_labels <- c(
    "1" = "Erratic strategy onset",
    "2" = "Rapid strategy onset",
    "3" = "Delayed strategy onset"
  )
  
  cluster_colors <- c(
    "1" = "#E64B35", 
    "2" = "#4DBBD5",  
    "3" = "#00A087"
  )
  
  ggplot(strategy_data_clusters, 
         aes(x = trial_after_rot, y = aimdeviation_deg, 
             group = participant_id, color = factor(cluster))) +
    geom_line(alpha = 0.6, linewidth = 0.7) +
    facet_wrap(~cluster, ncol = 1, labeller = as_labeller(strategy_labels)) +
    scale_color_manual(values = cluster_colors, guide = "none") +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      strip.text = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 11),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      panel.spacing = unit(1, "lines")
    ) +
    labs(
      title = "Trial-by-trial aiming deviation by strategy cluster",
      x = "Trial after rotation",
      y = "Aiming deviation (°)"
    )
}
