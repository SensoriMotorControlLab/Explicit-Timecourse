strategy_data <- strategy_data %>%
  filter(trial_type.x == "rotated") %>%
  
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%   
  mutate(trial_after_rot = row_number() - 1) %>%  # starts at 0
  ungroup()

block_data <- strategy_data %>%
  filter(trial_type.x == "rotated") %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  mutate(
    trial_after_rot = row_number() - 1,      
    block = (trial_after_rot %/% 8) + 1    #15 blocks of 8 trials
  ) %>%
  ungroup()

block_features <- block_data %>%
  group_by(participant_id, block) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  mutate(change_in_aim = aimdeviation_deg - lag(aimdeviation_deg)) %>%
  summarise(
    mean_aim    = mean(aimdeviation_deg, na.rm = TRUE),
    sd_aim      = sd(aimdeviation_deg, na.rm = TRUE),
    mean_change = mean(change_in_aim, na.rm = TRUE),
    n_negative  = sum(aimdeviation_deg < 0, na.rm = TRUE), 
    .groups = "drop"
  )


block12_features <- block_features %>%
  filter(block %in% c(1, 2)) %>%
  group_by(participant_id) %>%
  summarise(
    mean_aim_block1  = mean(mean_aim[block == 1], na.rm = TRUE),
    sd_aim_block1    = mean(sd_aim[block == 1], na.rm = TRUE),
    mean_change_block1 = mean(mean_change[block == 1], na.rm = TRUE),
    n_negative_block1  = sum(n_negative[block == 1], na.rm = TRUE),
    
    mean_aim_block2  = mean(mean_aim[block == 2], na.rm = TRUE),
    sd_aim_block2    = mean(sd_aim[block == 2], na.rm = TRUE),
    mean_change_block2 = mean(mean_change[block == 2], na.rm = TRUE),
    n_negative_block2  = sum(n_negative[block == 2], na.rm = TRUE),
    .groups = "drop"
  )


block12_features <- block12_features %>%
  mutate(
    cluster_label = case_when(
      # Erratic-onset: mean (>7 or <-7) in block1 or block2, high SD, at least one negative in block1 
      ((mean_aim_block1 > 5 | mean_aim_block1 < -5 | mean_aim_block2 > 7 | mean_aim_block2 < -7) &
         (sd_aim_block1 >= 15 | sd_aim_block2 >= 15) &
         (n_negative_block1 > 0 | n_negative_block2 > 0)) ~ "Erratic-onset",
      
      # Rapid-onset:  mean (>7), low SD (<8)
      mean_aim_block1 > 5 & mean_aim_block2 > 5 & sd_aim_block1 < 8 & sd_aim_block2 < 8 ~ "Rapid-onset",
      
      # Delayed-onset: near-zero mean (-2 to 2) in both blocks, low SD (<8)
      mean_aim_block1 >= -4.5 & mean_aim_block1 <= 4.5 &
        mean_aim_block2 >= -2 & mean_aim_block2 <= 2 &
        sd_aim_block1 < 8 & sd_aim_block2 < 8 ~ "Delayed-onset",
    
    )
  )


strategy_data_clustered <- block_data %>%
  left_join(block12_features %>% select(participant_id, cluster_label), by = "participant_id")

ggplot(strategy_data_clustered, 
       aes(x = trial_after_rot, y = aimdeviation_deg, group = participant_id, color = cluster_label)) +
  geom_line(alpha = 0.3) +
  facet_wrap(~ cluster_label, ncol = 1) +
  theme_minimal() +
  coord_cartesian(ylim = c(-90, 90)) +
  labs(title = "Trial-by-Trial Aiming by Behavioral Cluster",
       x = "Trial (after rotation onset)",
       y = "Aiming Deviation (°)",
       color = "Cluster Type")


participants_by_cluster <- strategy_data_clustered %>%
  group_by(cluster_label) %>%
  summarise(participant_ids = list(unique(participant_id)), .groups = "drop")

