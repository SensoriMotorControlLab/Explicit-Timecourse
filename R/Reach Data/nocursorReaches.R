##look at washout

setUpFive <- function () {
  learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors = FALSE)
  pca_df <- runHClust()
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  strategy_df <- getStrategies()
  
  no_participants <- strategy_df %>% filter(strategy == "No") %>% pull(participant_id)
  
  # No strategy 
  rotated_no_strategy <- learners_data %>%
    filter(trial_type == "rotated", participant_id %in% no_participants) %>%
    group_by(participant_id) %>%
    slice_tail(n = 8) %>%
    ungroup()
  
  nocursor_no_strategy <- learners_data %>%
    filter(trial_type == "nocursor", participant_id %in% no_participants) %>%
    group_by(participant_id) %>%
    slice_tail(n = 24) %>%
    ungroup()
  
  no_strategy_final <- bind_rows(rotated_no_strategy, nocursor_no_strategy) %>%
    mutate(group = "Non-strategy learners")
  
  # Strategy 
  rotated_strategy <- strategy_data %>%
    filter(trial_type == "rotated") %>%
    group_by(participant_id) %>%
    slice_tail(n = 8) %>%
    ungroup()
  
  nocursor_strategy <- strategy_data %>%
    filter(trial_type == "nocursor") %>%
    group_by(participant_id) %>%
    slice_tail(n = 24) %>%
    ungroup()
  
  strategy_final <- bind_rows(rotated_strategy, nocursor_strategy) %>%
    filter(participant_id %in% pca_df$participant_id) %>%    
    left_join(pca_df %>% select(participant_id, cluster_label), by = "participant_id") 
  
  combined <- bind_rows(no_strategy_final, strategy_final)
  
  return(combined)
}

getClusterData <- function() {
  pca_df <- runHClust()
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  
  baseline <- strategy_data %>%
    filter(trial_type == "nocursor", task_idx == 9) %>%
    group_by(participant_id) %>%
    summarise(baseline_reach = mean(reachdeviation_deg, na.rm = TRUE), .groups = "drop")
  
  washout <- strategy_data %>%
    filter(trial_type == "nocursor") %>%
    group_by(participant_id) %>%
    slice_tail(n = 24) %>%
    ungroup() %>%
    left_join(pca_df %>% select(participant_id, cluster_label), by = "participant_id")
  
  rotated <- strategy_data %>%
    filter(trial_type == "rotated") %>%
    group_by(participant_id) %>%
    slice_tail(n = 8) %>%
    ungroup() %>%
    left_join(pca_df %>% select(participant_id, cluster_label), by = "participant_id")
  
  bind_rows(washout, rotated) %>%
    left_join(baseline, by = "participant_id") %>%
    mutate(
      reach_norm = case_when(
        trial_type == "nocursor" ~ reachdeviation_deg - baseline_reach,
        trial_type == "rotated"  ~ reachdeviation_deg,
        TRUE                     ~ reachdeviation_deg
      )
    ) %>%
    group_by(participant_id, trial_type) %>%
    mutate(
      mean_reach = mean(reach_norm, na.rm = TRUE),
      sd_reach   = sd(reach_norm, na.rm = TRUE)
    ) %>%
    filter(between(reach_norm, mean_reach - 3 * sd_reach, mean_reach + 3 * sd_reach)) %>%
    ungroup() %>%
    select(-mean_reach, -sd_reach)
}

getStrategyData <- function() {
  learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors = FALSE)
  combined_df <- setUpFive()
  
  baseline <- learners_data %>%
    filter(trial_type == "nocursor", task_idx == 9) %>%
    group_by(participant_id) %>%
    summarise(baseline_reach = mean(reachdeviation_deg, na.rm = TRUE), .groups = "drop")
  
  combined_df %>%
    left_join(baseline, by = "participant_id") %>%
    mutate(
      reach_norm = case_when(
        trial_type == "nocursor" ~ reachdeviation_deg - baseline_reach,
        trial_type == "rotated"  ~ reachdeviation_deg,
        TRUE                     ~ reachdeviation_deg
      )
    ) %>%    
    group_by(participant_id, trial_type) %>%
    mutate(
      mean_reach = mean(reach_norm, na.rm = TRUE),
      sd_reach   = sd(reach_norm, na.rm = TRUE)
    ) %>%
    filter(between(reach_norm, mean_reach - 3 * sd_reach, mean_reach + 3 * sd_reach)) %>%
    ungroup() %>%
    select(-mean_reach, -sd_reach)
}

getRotationData <- function() {
  learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors = FALSE)
  combined_df <- setUpFive()
  
  baseline <- learners_data %>%
    filter(trial_type == "nocursor", task_idx == 9) %>%
    group_by(participant_id) %>%
    summarise(baseline_reach = mean(reachdeviation_deg, na.rm = TRUE), .groups = "drop")
  
  combined_df %>%
    left_join(baseline, by = "participant_id") %>%
    mutate(
      reach_norm = case_when(
        trial_type == "nocursor" ~ reachdeviation_deg - baseline_reach,
        trial_type == "rotated"  ~ reachdeviation_deg,
        TRUE                     ~ reachdeviation_deg
      )
    ) %>%
    group_by(participant_id, trial_type, rotation) %>%
    mutate(
      mean_reach = mean(reach_norm, na.rm = TRUE),
      sd_reach   = sd(reach_norm, na.rm = TRUE)
    ) %>%
    filter(between(reach_norm, mean_reach - 3 * sd_reach, mean_reach + 3 * sd_reach)) %>%
    ungroup() %>%
    select(-mean_reach, -sd_reach)
}


clusterAfterPlot <- function () {
cluster_data <- getClusterData()

cluster_plot_df <- cluster_data %>%
  group_by(cutrial_no, cluster_label) %>%
  summarise(
    mean_curve = mean(reach_norm, na.rm = TRUE),
    sd_reach   = sd(reach_norm, na.rm = TRUE),
    n = n(),
    se = sd_reach / sqrt(n),
    ci_lower = mean_curve - 1.96 * se,
    ci_upper = mean_curve + 1.96 * se,
    rel_trial = first(cutrial_no) - 232,
    .groups = "drop"
  )

p_cluster <- ggplot(cluster_plot_df, aes(x = rel_trial, y = mean_curve, color = cluster_label, fill = cluster_label)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  coord_cartesian(ylim = c(-10, 60)) +
  scale_color_manual(values = c("1" = "#EAA178", "2" = "cyan", "3" = "orchid"), labels = c("1" = "Exploratory", "2" = "Multi-Step", "3" = "One-Step")) +
  scale_fill_manual(values = c("1" = "#EAA178", "2" = "cyan", "3" = "orchid"), labels = c("1" = "Exploratory", "2" = "Multi-Step", "3" = "One-Step")) +
  labs(x = "Trial", y = "Baseline-Normalized Reach Deviation (°)", color = "Phenotype", fill = "Phenotype") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), 
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 12),  
    axis.text = element_text(size = 12, color = "black"),   
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 12)                 
  )

print(p_cluster)
}


clusterAfterStats <- function () {
  cluster_data <- getClusterData()
  cluster_anova_df <- cluster_data %>%
  filter(trial_type == "nocursor", cutrial_no %in% 233:240) %>%
  group_by(participant_id, cluster_label) %>%
  summarise(aftereffect = mean(reach_norm, na.rm = TRUE), .groups = "drop")

  print("--- Phenotype Cluster ANOVA ---")
  print(summary(aov(aftereffect ~ cluster_label, data = cluster_anova_df)))
}

clusterAfterBox <- function () {
cluster_anova_df <- clusterAfterStats()
cluster_summary <- cluster_anova_df %>%
  group_by(cluster_label) %>%
  summarise(
    mean_reach = mean(aftereffect, na.rm = TRUE),
    n = n(),
    sd_reach = sd(aftereffect, na.rm = TRUE),
    se_reach = sd_reach / sqrt(n),
    ci_margin = qt(0.975, df = n - 1) * se_reach,
    ci_lower = mean_reach - ci_margin,
    ci_upper = mean_reach + ci_margin,
    .groups = "drop"
  )

p_cluster_box <- ggplot() +
  geom_crossbar(data = cluster_summary, aes(x = cluster_label, y = mean_reach, ymin = ci_lower, ymax = ci_upper, fill = cluster_label), alpha = 0.6, width = 0.4, color = NA) +
  geom_errorbar(data = cluster_summary, aes(x = cluster_label, ymin = mean_reach, ymax = mean_reach, color = cluster_label), linewidth = 1, width = 0.4) +
  # annotate("segment", x = 1, xend = 2, y = 28, yend = 28, color = "black", linewidth = 0.6) +
  # annotate("segment", x = 1, xend = 1, y = 28, yend = 27, color = "black", linewidth = 0.6) +
  # annotate("segment", x = 2, xend = 2, y = 28, yend = 27, color = "black", linewidth = 0.6) +
  # annotate("text", x = 1.5, y = 28.5, label = "*", size = 6, color = "black", vjust = 0.4) +
  scale_fill_manual(values = c("1" = "#EAA178", "2" = "cyan", "3" = "orchid")) +
  scale_color_manual(values = c("1" = "#B3653B", "2" = "#008294", "3" = "#8E3E8C")) +
  scale_x_discrete(breaks = c("1", "2", "3"), labels = c("Exploratory", "Multi-Step", "One-Step")) +
  coord_cartesian(ylim = c(-5, 30)) + 
  labs(x = "", y = "Baseline-Normalized Reach Deviation (°)") +
  theme_classic(base_size = 12) +
  theme(
    panel.grid = element_blank(), 
    legend.position = "none",
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 12),  
    axis.text = element_text(size = 12, color = "black")
  )

print(p_cluster_box)
}
# ==============================================================================
# 3. STRATEGY VS NON-STRATEGY: ANALYSIS & FIGURES
# ==============================================================================

stratAfterPlot <- function () {
strategy_data_clean <- getStrategyData()

strategy_plot_df <- strategy_data_clean %>%
  group_by(cutrial_no, group) %>%
  summarise(
    mean_curve = mean(reach_norm, na.rm = TRUE), 
    sd_reach   = sd(reach_norm, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_reach / sqrt(n),
    ci_lower = mean_curve - 1.96 * se,
    ci_upper = mean_curve + 1.96 * se,
    rel_trial = cutrial_no - 232,
    group = factor(group)
  )

p_strategy <- ggplot(strategy_plot_df, aes(x = rel_trial, y = mean_curve, color = group, fill = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  coord_cartesian(ylim = c(-10, 60)) +
  scale_color_manual(values = c("Group 2" = "#9AD647", "Non-strategy learners" = "#C2B9CC"), labels = c("Group 2" = "Strategy", "Non-strategy learners" = "No strategy")) +
  scale_fill_manual(values = c("Group 2" = "#9AD647", "Non-strategy learners" = "#C2B9CC"), labels = c("Group 2" = "Strategy", "Non-strategy learners" = "No strategy")) +
  labs(x = "Trial", y = "Baseline-Normalized Reach Deviation (°)", color = "Strategy Use", fill = "Strategy Use") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), 
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 12),  
    axis.text = element_text(size = 12, color = "black"),   
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 12)                 
  )

print(p_strategy)
}

stratAfterStats <- function () {
  strategy_data_clean <- getStrategyData()
  strategy_anova_df <- strategy_data_clean %>%
  filter(trial_type == "nocursor", cutrial_no %in% 233:240) %>%
  group_by(participant_id, group) %>%
  summarise(aftereffect = mean(reach_norm, na.rm = TRUE), .groups = "drop") 

  print("--- Strategy t-tests ---")
  print(t.test(aftereffect ~ group, data = strategy_anova_df))
  print(t.test(strategy_anova_df$aftereffect[strategy_anova_df$group == "Non-strategy learners" | strategy_anova_df$group == "Group 2"], mu = 0))
}


stratAfterBox <- function () {
  strategy_anova_df <- stratAfterStats()
  strategy_summary <- strategy_anova_df %>%
  group_by(group) %>%
  summarise(
    mean_reach = mean(aftereffect, na.rm = TRUE),
    n = n(),
    sd_reach = sd(aftereffect, na.rm = TRUE),
    se_reach = sd_reach / sqrt(n),
    ci_margin = qt(0.975, df = n - 1) * se_reach,
    ci_lower = mean_reach - ci_margin,
    ci_upper = mean_reach + ci_margin,
    .groups = "drop"
  )

p_strategy_box <- ggplot() +
  geom_crossbar(data = strategy_summary, aes(x = group, y = mean_reach, ymin = ci_lower, ymax = ci_upper, fill = group), alpha = 0.6, width = 0.4, color = NA) +
  geom_errorbar(data = strategy_summary, aes(x = group, ymin = mean_reach, ymax = mean_reach, color = group), linewidth = 1, width = 0.4) +
  scale_fill_manual(values = c("Group 2" = "#9AD647", "Non-strategy learners" = "#C2B9CC")) +
  scale_color_manual(values = c("Group 2" = "#659728", "Non-strategy learners" = "#8C8296")) +
  scale_x_discrete(breaks = c("Group 2", "Non-strategy learners"), labels = c("Strategy Users", "Non-Strategy Users")) +
  coord_cartesian(ylim = c(-5, 30)) +
  labs(x = "", y = "Baseline-Normalized Reach Deviation (°)") +
  theme_classic(base_size = 12) +
  theme(
    panel.grid = element_blank(), 
    legend.position = "none",
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 12),  
    axis.text = element_text(size = 12, color = "black")
  )

  print(p_strategy_box)
}
# ==============================================================================
# 4. ROTATION GROUPS: ANALYSIS & FIGURES
# ==============================================================================

plotRotAfter <- function () {
rotation_data_clean <- getRotationData()
rotation_data_clean$rotation <- as.factor(rotation_data_clean$rotation)

rotation_plot_df <- rotation_data_clean %>%
  group_by(cutrial_no, rotation) %>%
  summarise(
    mean_curve = mean(reach_norm, na.rm = TRUE), 
    sd_reach   = sd(reach_norm, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_reach / sqrt(n),
    ci_lower = mean_curve - 1.96 * se,
    ci_upper = mean_curve + 1.96 * se,
    rel_trial = cutrial_no - 232
  )

p_rotation <- ggplot(rotation_plot_df, aes(x = rel_trial, y = mean_curve, color = rotation, fill = rotation)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  coord_cartesian(ylim = c(-10, 60)) +
  scale_color_manual(values = c("60"="#12086f", "50"="#2835af", "40"="#4261ee", "30"="#4895ef", "20"="#4cc9f0")) +
  scale_fill_manual(values = c("60"="#12086f", "50"="#2835af", "40"="#4261ee", "30"="#4895ef", "20"="#4cc9f0")) +
  labs(x = "Trial", y = "Baseline-Normalized Reach Deviation (°)", color = "Rotation", fill = "Rotation") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), 
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 12),  
    axis.text = element_text(size = 12, color = "black"),   
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 12)                 
  )

print(p_rotation)
}


rotAfterStats <- function () {
  rotation_data_clean <- getRotationData()
  rotation_data_clean$rotation <- as.factor(rotation_data_clean$rotation)
  
  rotation_anova_df <- rotation_data_clean %>%
  filter(trial_type == "nocursor", cutrial_no %in% 233:240) %>%
  group_by(participant_id, rotation) %>%
  summarise(aftereffect = mean(reach_norm, na.rm = TRUE), .groups = "drop") 

  print("--- Rotation ANOVA ---")
  print(summary(aov(aftereffect ~ rotation, data = rotation_anova_df)))

 
}


rotAfterBox <- function () {
  rotation_anova_df <- rotAfterStats()
  
  rotation_summary <- rotation_anova_df %>%
    group_by(rotation) %>%
    summarise(
      mean_reach = mean(aftereffect, na.rm = TRUE),
      n = n(),
      sd_reach = sd(aftereffect, na.rm = TRUE),
      se_reach = sd_reach / sqrt(n),
      ci_margin = qt(0.975, df = n - 1) * se_reach,
      ci_lower = mean_reach - ci_margin,
      ci_upper = mean_reach + ci_margin,
      .groups = "drop"
    )
  p_rotation_box <- ggplot() +
  geom_crossbar(data = rotation_summary, aes(x = rotation, y = mean_reach, ymin = ci_lower, ymax = ci_upper, fill = rotation), alpha = 0.6, width = 0.4, color = NA) +
  geom_errorbar(data = rotation_summary, aes(x = rotation, ymin = mean_reach, ymax = mean_reach, color = rotation), linewidth = 1, width = 0.4) +
  scale_fill_manual(values = c("20" = "#4cc9f0", "30" = "#4895ef", "40" = "#4261ee", "50" = "#2835af", "60" = "#12086f")) +
  scale_color_manual(values = c("20" = "#1D96BD", "30" = "#2063B3", "40" = "#1E36A6", "50" = "#161F69", "60" = "#0A0440")) +
  coord_cartesian(ylim = c(-5, 30)) +
  labs(x = "Rotation Group", y = "Baseline-Normalized Reach Deviation (°)") +
  theme_classic(base_size = 12) +
  theme(
    panel.grid = element_blank(), 
    legend.position = "none",
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 12),  
    axis.text = element_text(size = 12, color = "black")
  )

  print(p_rotation_box)
}

