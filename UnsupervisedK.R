library(dplyr)
library(tidyr)
library(ggplot2)
library(factoextra)


##separate into blocks

strategy_data <- strategy_data %>%
  filter(trial_type.x == "rotated") %>%
  
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%   # optional: ensure trials are in order
  mutate(trial_after_rot = row_number() - 1) %>%  # starts at 0
  ungroup()

block_data <- strategy_data %>%
  filter(trial_type.x == "rotated") %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  mutate(
    trial_after_rot = row_number() - 1,          # 0–119
    block = (trial_after_rot %/% 8) + 1          # creates 15 blocks of 8 trials
  ) %>%
  ungroup()

block_features <- block_data %>%
  group_by(participant_id, block) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  mutate(change_in_aim = aimdeviation_deg - lag(aimdeviation_deg)) %>%
  summarise(
    mean_aim     = mean(aimdeviation_deg, na.rm = TRUE),
    sd_aim       = sd(aimdeviation_deg, na.rm = TRUE),
    mean_change  = mean(change_in_aim, na.rm = TRUE),
 -   .groups = "drop"
  )


block_wide <- block_features %>%
  pivot_wider(
    id_cols = participant_id,
    names_from = block,
    values_from = c(mean_aim, sd_aim, mean_change),
    names_glue = "{.value}_block_{block}"
  )


block_scaled <- scale(block_wide %>% select(-participant_id))
set.seed(123)
km <- kmeans(block_scaled, centers = 3, nstart = 50)
block_wide$cluster <- factor(km$cluster)


#see clusters
*

participants_by_cluster <- block_wide %>%
  group_by(cluster) %>%
  summarise(participant_ids = list(participant_id), .groups = "drop")

list <-  participants_by_cluster %>%
  unnest(cols = participant_ids) %>%
  arrange(cluster)


###plot raw aiming

strategy_data_clustered <- block_data %>%
  left_join(block_wide %>% select(participant_id, cluster), by = "participant_id") %>%
  mutate(cluster = factor(cluster))  

ggplot(strategy_data_clustered, 
       aes(x = trial_after_rot, y = aimdeviation_deg, group = participant_id, color = cluster)) +
  geom_line(alpha = 0.3, size = 0.6) +      
  facet_wrap(~ cluster, ncol = 1) +          
  theme_minimal(base_size = 13) +
  coord_cartesian(ylim = c(-90, 90)) + 
  scale_color_brewer(palette = "Set2") +    
  labs(title = "Raw Trial-by-Trial Aiming per Cluster",
       x = "Trial (after rotation onset)",
       y = "Aiming Deviation (°)",
       color = "Cluster") +
  theme(panel.grid = element_blank())

