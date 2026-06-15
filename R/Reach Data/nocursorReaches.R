##look at washout


setUpFive <- function () {
  
  learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors = FALSE)
  pca_df <- runHClust()
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  ci_result <- getCI()
  CI_df <- ci_result$CI
  strategy_df <- getStrategies()
  
  yes_participants <- strategy_df %>% filter(strategy == "Yes") %>% pull(participant_id)
  no_participants  <- strategy_df %>% filter(strategy == "No") %>% pull(participant_id)
  
  
  
  
  #no strategy 
  rotated_no_strategy <- learners_data %>%
    filter(trial_type == "rotated",
           participant_id %in% no_participants) %>%
    group_by(participant_id) %>%
    slice_tail(n = 8) %>%
    ungroup()
  
  nocursor_no_strategy <- learners_data %>%
    filter(trial_type == "nocursor",
           participant_id %in% no_participants) %>%
    group_by(participant_id) %>%
    slice_tail(n = 24) %>%
    ungroup()
  
  no_strategy_final <- bind_rows(rotated_no_strategy, nocursor_no_strategy) %>%
    mutate(group = "Non-strategy learners")
  
  #strategy 
  
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
    # 1. Keep only participants in your PCA analysis
    filter(participant_id %in% pca_df$participant_id) %>%  
    
    # 2. Join the cluster labels
    left_join(
      pca_df %>% select(participant_id, cluster_label),
      by = "participant_id"
    ) 
    
  
  combined <- bind_rows(
    #nonlearners_final,
    no_strategy_final,
    strategy_final
  )
  
  # 1. Update the cleaning step to KEEP cluster_label
  combined_clean <- combined %>%
    group_by(participant_id, trial_type) %>%
    mutate(
      mean_reach = mean(reachdeviation_deg, na.rm = TRUE),
      sd_reach   = sd(reachdeviation_deg, na.rm = TRUE)
    ) %>%
    filter(
      between(reachdeviation_deg,
              mean_reach - 3 * sd_reach,
              mean_reach + 3 * sd_reach)
    ) %>%
    ungroup() %>%
    select(-mean_reach, -sd_reach) 
  
  
  return(combined_clean)
}
getClusterData <- function() {
  
  pca_df <- runHClust()
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  
  washout <- strategy_data %>%
    filter(trial_type == "nocursor") %>%
    group_by(participant_id) %>%
    slice_tail(n = 24) %>%
    ungroup() %>%
    left_join(pca_df %>% select(participant_id, cluster_label),
              by = "participant_id")
  
  rotated <- strategy_data %>%
    filter(trial_type == "rotated") %>%
    group_by(participant_id) %>%
    slice_tail(n = 8) %>%
    ungroup() %>%
    left_join(pca_df %>% select(participant_id, cluster_label),
              by = "participant_id")
  
  bind_rows(washout, rotated) %>%
    group_by(participant_id, trial_type) %>%
    mutate(
      mean_reach = mean(reachdeviation_deg, na.rm = TRUE),
      sd_reach   = sd(reachdeviation_deg, na.rm = TRUE)
    ) %>%
    filter(between(reachdeviation_deg,
                   mean_reach - 3*sd_reach,
                   mean_reach + 3*sd_reach)) %>%
    ungroup()
}

cluster_data <- getClusterData()
cluster_plot_df <- cluster_data %>%
  group_by(cutrial_no, cluster_label) %>%
  summarise(
    mean_reach = mean(reachdeviation_deg / rotation),
    sd_reach   = sd(reachdeviation_deg / rotation),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_reach / sqrt(n),
    ci_lower = mean_reach - 1.96 * se,
    ci_upper = mean_reach + 1.96 * se,
    rel_trial = cutrial_no - 232,
    cluster_label = factor(cluster_label)
  )


p_cluster <- ggplot(cluster_plot_df,
                    aes(x = rel_trial,
                        y = mean_reach,
                        color = cluster_label,
                        fill = cluster_label)) +
  
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
              alpha = 0.2, color = NA) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  
  coord_cartesian(ylim = c(-0.5, 1)) +
  
  scale_color_manual(
    values = c("1" = "#EAA178",
               "2" = "cyan",
               "3" = "orchid"),
    labels = c("1" = "Exploratory",
               "2" = "Multi-Step",
               "3" = "One-Step")
  ) +
  
  scale_fill_manual(
    values = c("1" = "#EAA178",
               "2" = "cyan",
               "3" = "orchid"),
    labels = c("1" = "Exploratory",
               "2" = "Multi-Step",
               "3" = "One-Step")
  ) +
  
  labs(
    x = "Trial",
    y = "Normalized Reach Deviation",
    color = "Phenotype",
    fill  = "Phenotype"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

print(p_cluster)


##anova
cluster_anova_df <- cluster_data %>%
  filter(trial_type == "nocursor",
         cutrial_no %in% 234:241) %>%
  mutate(norm_reach = reachdeviation_deg/rotation) %>%
  group_by(participant_id, cluster_label) %>%
  summarise(aftereffect = mean(norm_reach), .groups = "drop")

model_cluster <- aov(aftereffect ~ cluster_label,
                     data = cluster_anova_df)

summary(model_cluster)


#t-test per strategy
model_group2 <- t.test(
  cluster_anova_df$aftereffect[cluster_anova_df$cluster_label == "3"],
  mu = 0
)

print(model_group2)



########NON STRATEGY VS STRAT #############

getStrategyData <- function() {
  
  combined <- setUpFive()
  
  combined %>%
    group_by(participant_id, trial_type) %>%
    mutate(
      mean_reach = mean(reachdeviation_deg, na.rm = TRUE),
      sd_reach   = sd(reachdeviation_deg, na.rm = TRUE)
    ) %>%
    filter(between(reachdeviation_deg,
                   mean_reach - 3*sd_reach,
                   mean_reach + 3*sd_reach)) %>%
    ungroup()
}
strategy_data <- getStrategyData()

strategy_plot_df <- strategy_data %>%
  group_by(cutrial_no, group) %>%
  summarise(
    mean_reach = mean(reachdeviation_deg / rotation),
    sd_reach   = sd(reachdeviation_deg / rotation),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_reach / sqrt(n),
    ci_lower = mean_reach - 1.96 * se,
    ci_upper = mean_reach + 1.96 * se,
    rel_trial = cutrial_no - 232,
    group = factor(group)
  )

###plot

p_strategy <- ggplot(strategy_plot_df,
                     aes(x = rel_trial,
                         y = mean_reach,
                         color = group,
                         fill = group)) +
  
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
              alpha = 0.2, color = NA) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  
  coord_cartesian(ylim = c(-0.5, 1)) +
  
  scale_color_manual(
    values = c("Group 2" = "#2F4858",
               "Non-strategy learners" = "#BC4749"),
    labels = c("Group 2" = "Strategy",
               "Non-strategy learners" = "No strategy")
  ) +
  
  scale_fill_manual(
    values = c("Group 2" = "#2F4858",
               "Non-strategy learners" = "#BC4749"),
    labels = c("Group 2" = "Strategy",
               "Non-strategy learners" = "No strategy")
  ) +
  
  labs(
    x = "Trial",
    y = "Normalized Reach Deviation",
    color = "Strategy Use",
    fill  = "Strategy Use"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

print(p_strategy)



strategy_anova_df <- strategy_data %>%
  filter(trial_type == "nocursor",
         cutrial_no %in% 234:241) %>%
  mutate(norm_reach = reachdeviation_deg/rotation) %>%
  group_by(participant_id, group) %>%
  summarise(aftereffect = mean(norm_reach), .groups = "drop")

model_strategy <- t.test(aftereffect ~ group,
                      data = strategy_anova_df)

print(model_strategy)

model_group2 <- t.test(
  strategy_anova_df$aftereffect[strategy_anova_df$group == "Group 2"],
  mu = 0
)

print(model_group2)
