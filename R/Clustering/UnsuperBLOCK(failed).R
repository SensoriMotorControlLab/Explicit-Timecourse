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
    mean_aim = mean(aimdeviation_deg, na.rm = TRUE),
    sd_aim = sd(aimdeviation_deg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(participant_id) %>%
  mutate(
    block_of_change = which(mean_aim > 7 | mean_aim < -7)[1]
  ) %>%
  ungroup()

block_wide <- block_features %>%
  pivot_wider(
    id_cols = participant_id,
    names_from = block,
    values_from = c(mean_aim, sd_aim, block_of_change)
  )

#theres some blocks with no changes, indicating stability so change NA to 0
block_scaled <- block_wide %>%
  select(-participant_id) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>%
  select(where(~ sd(.) != 0)) %>%   # remove columns with zero variance
  scale()

set.seed(123)
km <- kmeans(block_scaled, centers = 3, nstart = 50)
block_wide$cluster <- factor(km$cluster)


strategy_data %>%
  filter(trial_type.x == "rotated") %>%
  left_join(block_wide %>% select(participant_id, cluster), by = "participant_id") %>%
  ggplot(aes(x = trial_after_rot, y = aimdeviation_deg, group = participant_id)) +
  geom_line(alpha = 0.6) +
  facet_wrap(~cluster, ncol = 1) +
  theme_minimal() +
  labs(title = "Aiming Cluster: Block-level",
       x = "Trial after rotation",
       y = "Aiming deviation (deg)")

#see clusters

participants_by_cluster <- block_wide %>%
  group_by(cluster) %>%
  summarise(participant_ids = list(participant_id), .groups = "drop")

list <-  participants_by_cluster %>%
  unnest(cols = participant_ids) %>%
  arrange(cluster)

