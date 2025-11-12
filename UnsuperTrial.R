### Unsupervised learning with trial by trial features

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

table %>%
  count(cluster)


strategy_data_clusters <- strategy_data %>%
  filter(trial_type.x == "rotated") %>%
  left_join(trial_pca %>% select(participant_id, cluster), by = "participant_id")

table <- strategy_data_clusters %>%
  select(participant_id, cluster) %>%
  distinct()

ggplot(strategy_data_clusters, aes(x = trial_after_rot, y = aimdeviation_deg, group = participant_id)) +
  geom_line(alpha = 0.6) +  
  facet_wrap(~cluster, ncol = 1) + 
  theme_minimal() +
  labs(
    title = "Trial-by-trial aiming deviation by cluster",
    x = "Trial after rotation",
    y = "Aiming deviation (degrees)"
  )