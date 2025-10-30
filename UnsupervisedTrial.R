### Unsupervised learning with trial by trial features

library(zoo)


trial_features <- strategy_data %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  mutate(
    mean_aim = rollapply(aimdeviation_deg, width = 8, FUN = mean, align = "right", fill = NA),
    sd_aim   = rollapply(aimdeviation_deg, width = 8, FUN = sd, align = "right", fill = NA),
    trial_of_change = which(aimdeviation_deg > 7 | aimdeviation_deg < -7)[1],
  ) %>%
  ungroup() %>%
  select(participant_id, trial_after_rot, mean_aim, sd_aim, trial_of_change)

trial_summary <- trial_features %>%
  group_by(participant_id) %>%
  summarise(across(c(mean_aim, sd_aim, trial_of_change), mean, na.rm = TRUE))

# pca
trial_scaled <- scale(trial_summary %>% select(-participant_id))
pca <- prcomp(trial_scaled, center = TRUE, scale. = TRUE)
summary(pca)

trial_pca <- as.data.frame(pca$x[, 1:3]) #3 features
trial_pca$participant_id <- trial_summary$participant_id

km <- kmeans(trial_pca[, 1:3], centers = 3, nstart = 50)
clusters <- km$cluster
trial_pca$cluster <- factor(clusters)

##plot
pca_df$participant_id <- trial_summary$participant_id

ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster, label = participant_id)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, size = 3) +
  theme_minimal()

#------#
strategy_data_clusters <- strategy_data %>%
  filter(trial_type.x == "rotated") %>%
  left_join(
    trial_summary %>% mutate(cluster = factor(clusters)),
    by = "participant_id"
  )

ggplot(strategy_data_clusters, aes(x = trial_after_rot, y = aimdeviation_deg, group = participant_id)) +
  geom_line(alpha = 0.6) +  # line per participant
  facet_wrap(~cluster, ncol = 1) +  # separate panels by cluster
  theme_minimal() +
  labs(
    title = "Aim patterns by cluster from trial-by-trial kmeans",
    x = "Trial after rotation",
    y = "Aiming deviation (degrees)"
  )
