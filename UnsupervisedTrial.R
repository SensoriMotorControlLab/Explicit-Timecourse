### Unsupervised learning with trial by trial features
# once you get learning phase form xgboost, conduct kmeans on those.




library(zoo)

strategy_data <- strategy_data %>%
  filter(trial_type.x == "rotated") %>%
  
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%   
  mutate(trial_after_rot = row_number() - 1) %>%  
  ungroup()

trial_features <- strategy_data %>%
  filter(trial_after_rot <= 32) %>% 
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  mutate(
    trial_of_change = {
      t <- which(aimdeviation_deg > 7 | aimdeviation_deg < -7)
      if (length(t) > 0) t[1] else NA
    }
  ) %>%
  ungroup() %>%
  select(participant_id, trial_after_rot, aimdeviation_deg, trial_of_change)


trial_summary <- trial_features %>%
  group_by(participant_id) %>%
  summarise(
    trial_of_change = ifelse(all(is.na(trial_of_change)), 32, mean(trial_of_change, na.rm = TRUE)),
    sd_aim   = sd(aimdeviation_deg, na.rm = TRUE),
    mean_aim = mean(aimdeviation_deg, na.rm = TRUE),
    prop_negative = mean(aimdeviation_deg < -3, na.rm = TRUE)
  ) %>%
  mutate(
    trial_of_change = trial_of_change * 8
  )




trial_scaled <- trial_summary %>%
  select(mean_aim, sd_aim, trial_of_change, prop_negative) %>%
  
  scale(center = TRUE, scale = TRUE)


pca <- prcomp(trial_scaled, center = TRUE, scale. = TRUE)
trial_pca <- as.data.frame(pca$x[, 1:4])
trial_pca$participant_id <- trial_summary$participant_id

km <- kmeans(trial_pca[, 1:4], centers = 3, nstart = 50)
trial_pca$cluster <- factor(km$cluster)


strategy_data_clusters <- strategy_data %>%
  filter(trial_type.x == "rotated") %>%
  left_join(trial_pca %>% select(participant_id, cluster), by = "participant_id")

table <- strategy_data_clusters %>%
  select(participant_id, cluster) %>%
  distinct()
strategy_labels <- c(
  "1" = "Erratic strategy onset",
  "2" = "Rapid strategy onset",
  "3" = "Delayed strategy onset"
)

# Define custom colors for each cluster
cluster_colors <- c(
  "1" = "#E64B35",  # red
  "2" = "#4DBBD5",  # blue
  "3" = "#00A087"   # green
)

ggplot(strategy_data_clusters, 
       aes(x = trial_after_rot, y = aimdeviation_deg, 
           group = participant_id, color = factor(cluster))) +
  geom_line(alpha = 0.6, linewidth = 0.7) +
  facet_wrap(~cluster, ncol = 1, labeller = as_labeller(strategy_labels)) +
  scale_color_manual(values = cluster_colors, guide = "none") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),          # remove grid lines
    strip.text = element_text(size = 14, face = "bold"),  # facet titles
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "",
    x = "Trial after rotation",
    y = "Aiming deviation (°)"
  )