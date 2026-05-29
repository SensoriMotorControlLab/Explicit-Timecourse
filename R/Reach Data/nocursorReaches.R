##look at washout


setUpFive <- function () {
  
  nonlearners_data <- read.csv("data/nonlearners_data.csv", stringsAsFactors = FALSE)
  learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors = FALSE)
  pca_df <- kPCA()
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  ci_result <- getCI()
  CI_df <- ci_result$CI
  strategy_df <- getStrategies()
  
  yes_participants <- strategy_df %>% filter(strategy == "Yes") %>% pull(participant_id)
  no_participants  <- strategy_df %>% filter(strategy == "No") %>% pull(participant_id)
  
  
  
  ##washout for non learners
  rotated_nonlearners <- nonlearners_data %>%
    filter(trial_type == "rotated") %>%
    group_by(participant_id) %>%
    slice_tail(n = 8) %>%
    ungroup()
  
  nocursor_nonlearners <- nonlearners_data %>%
    filter(trial_type == "nocursor") %>%
    group_by(participant_id) %>%
    slice_tail(n = 24) %>%
    ungroup()
  
  nonlearners_final <- bind_rows(rotated_nonlearners, nocursor_nonlearners) %>%
    mutate(group = "Non-learners")
  
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
    filter(participant_id %in% pca_df$participant_id) %>%  
    left_join(
      pca_df %>% select(participant_id, cluster),
      by = "participant_id"
    )
  
  combined <- bind_rows(
    #nonlearners_final,
    no_strategy_final,
    strategy_final
  )
  
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

##T test to 0 is not significant (p=0.73) so non learners essentially 
# have no aftereffects. We won't include in ANOVA

findAll <- function () {
  combined_clean <- setUpFive()
  
  anova_df <- combined_clean %>%
    filter(trial_type == "nocursor",
           cutrial_no %in% 234:241,
            group == "Group 2"
    ) %>% 
    group_by(participant_id,group, cluster,rotation) %>%
    summarise(
      aftereffect = mean(reachdeviation_deg),
      .groups = "drop"
    )
  
  
  anova_df <- anova_df %>%
    mutate(rotation = as.factor(rotation))
  anova_df$cluster <- factor(anova_df$cluster)
  anova_df$group <- factor(anova_df$group)
 
   anova_model <- aov(
    aftereffect ~ cluster,
    data = anova_df
  )
  
  summary(anova_model)
  
  t.test(anova_df$aftereffect, mu=0)
  
  
}

fiveANOVA <- function () {
  nonLearners_data <- read.csv("data/nonlearners_data.csv", stringsAsFactors = FALSE)
  combined_clean <- setUpFive()
  
  anova_df <- combined_clean %>%
    filter(trial_type == "nocursor",
           cutrial_no %in% 234:256,
          group != "Non-learners"
           ) %>% 
    group_by(participant_id, group, rotation) %>%
    summarise(
      aftereffect = mean(reachdeviation_deg),
      .groups = "drop"
    )
  anova_df <- anova_df %>%
    mutate(rotation = as.factor(rotation))
  
  group_means <- anova_df %>%
    group_by(group) %>%
    summarise(mean_aftereffect = mean(aftereffect, na.rm = TRUE))
  
  
  anova_model <- aov(
    aftereffect ~ group * rotation + Error(participant_id/rotation),
    data = anova_df
  )
  
  
  summary(anova_model)
  
 # TukeyHSD(anova_model, "rotation") #"group"
  #Group = between-subject factor 
  #Rotation = within-subject factor
  
}


bfAnova <- function () {
  nonLearners_data <- read.csv("data/nonlearners_data.csv", stringsAsFactors = FALSE)
  combined_clean <- setUpFive()
  
  anova_df <- combined_clean %>%
    filter(trial_type == "nocursor",
           cutrial_no %in% 234:256,
           group != "Non-learners"
    ) %>% 
    group_by(participant_id, group, rotation) %>%
    summarise(
      aftereffect = mean(reachdeviation_deg),
      .groups = "drop"
    )
  
  anova_df <- anova_df %>%
    mutate(
      group = as.factor(group),
      participant_id = as.factor(participant_id),
      rotation = as.factor(rotation)
    )
 
  bf_model <- anovaBF(
    aftereffect ~ group + rotation,     
    data = anova_df,
    whichRandom = "participant_id",     
    iterations = 2000
  )
  
  bf_model
}




# getWashoutLeaners <- function () {
#   learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors =FALSE)
#   
#   rotated <- learners_data %>%
#     filter(trial_type == "rotated") %>%
#     group_by(participant_id) %>%
#     slice_tail(n = 8) %>%
#     ungroup()
#   
#     
#   washout <- learners_data %>%
#     filter(trial_type == "nocursor") %>%
#     group_by(participant_id) %>%
#     slice_tail(n = 24) %>%
#     ungroup()
#   
#   total <- bind_rows(rotated, washout) %>%
#     group_by(rotation) %>%
#     mutate(
#       mean_dev = mean(reachdeviation_deg, na.rm = TRUE),
#       sd_dev = sd(reachdeviation_deg, na.rm = TRUE)
#     ) %>%
#     filter(
#       reachdeviation_deg >= (mean_dev - 4 * sd_dev),
#       reachdeviation_deg <= (mean_dev + 4 * sd_dev)
#     ) %>%
#     select(-mean_dev, -sd_dev) %>%
#     ungroup()
# 
#   summary_df <- total %>%
#     group_by(cutrial_no) %>%
#     summarise(
#       mean_reach = mean(reachdeviation_deg, na.rm = TRUE),
#       ci_lower = Reach::getConfidenceInterval(reachdeviation_deg)[1],
#       ci_upper = Reach::getConfidenceInterval(reachdeviation_deg)[2],
#       .groups = "drop"
#     ) %>%
#     arrange(cutrial_no) %>%
#     mutate(trial_index = row_number()) %>%
#     ungroup()
#   
# 
#   ggplot(summary_df, aes(x = cutrial_no, y = mean_reach, 
#                         )) +
#     geom_line(size = 1.2) +
#     geom_vline(xintercept = 233, , linetype="dashed", colour="black") +
#     geom_hline(yintercept = 0, linetype="dashed", colour="blue") +
#    # geom_line(aes(y = fit), colour = "red", size = 1.2) +
#     geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2,color = NA) +
#     coord_cartesian(ylim = c(-15, 70)) +
#     #coord_cartesian(xlim = c(224, 237)) +
#     labs(
#       x = "Trial",
#       y = "Reach Deviation",
#       title = "Washout of all participants (Last 24 Trials)"
#     ) +
#     theme(  panel.grid.major = element_blank(),
#             panel.grid.minor = element_blank(),
#             panel.background = element_blank()
#           )
# }  
#   
#   
# getWashoutStrategy <- function() {
#   
#   learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors = FALSE)
#   ci_result <- getCI()
#   CI_df <- ci_result$CI
#   strategy_df <- getStrategies()
#   
#   
#   # strategy_df has participant_id and strategy
#   yes_participants <- strategy_df %>% filter(strategy == "Yes") %>% pull(participant_id)
#   no_participants  <- strategy_df %>% filter(strategy == "No") %>% pull(participant_id)
#   
#   
# 
#   washout <- learners_data %>%
#     filter(trial_type == "nocursor") %>% 
#         #  !rotation %in% c(20, 30)) %>% 
#     group_by(participant_id) %>%
#     slice_tail(n = 24) %>%
#     ungroup() %>%
#     mutate(strategy = case_when(
#       participant_id %in% yes_participants ~ "Yes",
#       participant_id %in% no_participants  ~ "No"
#     ))
# 
#   rotated <- learners_data %>%
#     filter(trial_type == "rotated") %>% 
#        #    !rotation %in% c(20, 30)) %>% 
#     group_by(participant_id) %>%
#     slice_tail(n = 8) %>%
#     ungroup() %>%
#     mutate(strategy = case_when(
#       participant_id %in% yes_participants ~ "Yes",
#       participant_id %in% no_participants  ~ "No"
#     ))
#   
#   total <- bind_rows(washout,rotated)
#   
#   total_clean <- total %>%
#     group_by(rotation) %>%  # group by rotation
#     mutate(
#       mean_reach = mean(reachdeviation_deg, na.rm = TRUE),
#       sd_reach   = sd(reachdeviation_deg, na.rm = TRUE)
#     ) %>%
#     ungroup() %>%
#     filter(
#       reachdeviation_deg >= (mean_reach - 3*sd_reach) &
#         reachdeviation_deg <= (mean_reach + 3*sd_reach)
#     ) %>%
#     select(-mean_reach, -sd_reach) 
#   
#   summary_df <-   total_clean %>%
#     group_by(cutrial_no, strategy, rotation) %>%
#     summarise(
#       mean_reach = mean(reachdeviation_deg, na.rm = TRUE),  # use reach deviation from learners_data
#       sd_reach   = sd(reachdeviation_deg, na.rm = TRUE),
#       n          = n(),
#       .groups = "drop"
#     ) %>%
#     mutate(
#       se = sd_reach / sqrt(n),
#       ci_lower = mean_reach - 1.96 * se,
#       ci_upper = mean_reach + 1.96 * se
#     )
#   
#  p <- ggplot(summary_df, aes(x = cutrial_no, y = mean_reach,
#                          color = strategy, fill = strategy)) +
#     geom_line(size = 1.2) +
#     geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
#     geom_vline(xintercept = 233, linetype = "dashed", colour = "black") +
#     geom_hline(yintercept = 0, linetype = "dashed", colour = "blue") +
#     coord_cartesian(ylim = c(-15, 70)) +
#     scale_color_manual(values = c("Yes" = "hotpink", "No" = "#3A86C8")) +
#     scale_fill_manual(values = c("Yes" = "hotpink", "No" = "#3A86C8")) +
#     labs(
#       x     = "Trial",
#       y     = "Reach Deviation (°)",
#       title = "No-Cursor Trials: Strategy vs. No Strategy",
#       color = "Strategy Use",
#       fill  = "Strategy Use"
#     ) +
#     facet_wrap(~ rotation) + 
#     theme_minimal() +
#     theme(legend.position = "top")
#  
#  print(p)
# }
# 
# washoutStats <- function () {
#   learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors = FALSE)
#   strategy_df <- getStrategies()
# 
#   yes_participants <- strategy_df %>% filter(strategy == "Yes") %>% pull(participant_id)
#   no_participants  <- strategy_df %>% filter(strategy == "No") %>% pull(participant_id)
# 
# washout_first <- learners_data %>%
#   group_by(rotation) %>%
#   mutate(
#     mean_reach = mean(reachdeviation_deg, na.rm = TRUE),
#     sd_reach   = sd(reachdeviation_deg, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   filter(
#     reachdeviation_deg >= (mean_reach - 3*sd_reach) &
#       reachdeviation_deg <= (mean_reach + 3*sd_reach)
#   ) %>%
#   select(-mean_reach, -sd_reach)
# 
# washout_clean <- washout_first %>%
#   filter(trial_type == "nocursor",
#          cutrial_no == 233) %>%
#   mutate(strategy = case_when(
#     participant_id %in% yes_participants ~ "Yes",
#     participant_id %in% no_participants  ~ "No"
#   ))
# 
# 
# washout_clean$strategy <- as.factor(washout_clean$strategy)
# washout_clean$rotation <- as.factor(washout_clean$rotation)
# 
# bf_model <- anovaBF(
#   reachdeviation_deg ~ strategy*rotation,
#   data = washout_clean
# )
# bf_model
# 
# 
# }
# 
# 
# 
getWashoutCluster <- function() {

  pca_df <- kPCA()
  strategy_data <- read.csv("data/strategy_only_participants.csv")


  washout <- strategy_data %>%
    filter(trial_type == "nocursor") %>%
    group_by(participant_id) %>%
    slice_tail(n = 24) %>%
    ungroup() %>%
    left_join(pca_df %>% select(participant_id, cluster),
              by = "participant_id")

  rotated <- strategy_data %>%
    filter(trial_type == "rotated") %>%
    group_by(participant_id) %>%
    slice_tail(n = 8) %>%
    ungroup()  %>%
    left_join(pca_df %>% select(participant_id, cluster),
              by = "participant_id")


  total <- bind_rows(washout,rotated)

  total_clean <- total %>%
    group_by(participant_id, rotation) %>%  
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
 
  
  summary_df <-   total_clean %>%
    group_by(cutrial_no, cluster) %>%
    summarise(
      mean_reach = mean(reachdeviation_deg, na.rm = TRUE),  # use reach deviation from learners_data
      sd_reach   = sd(reachdeviation_deg, na.rm = TRUE),
      n          = n(),
      .groups = "drop"
    ) %>%
    mutate(
      se = sd_reach / sqrt(n),
      ci_lower = mean_reach - 1.96 * se,
      ci_upper = mean_reach + 1.96 * se
    )
  summary_df <- summary_df %>%
    mutate(cluster = factor(cluster))
  
  summary_df <- summary_df %>%
    mutate(rel_trial = cutrial_no - 232)

  p <-ggplot(summary_df, aes(x = rel_trial, y = mean_reach,
                         color = cluster, fill = cluster)) +
    geom_line(size = 1.2) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey") +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
    coord_cartesian(ylim = c(-15, 70)) +
    scale_color_manual(values = c("1" = "hotpink", "2" = "#a2bffe", "3" = "#e89c7b"),
                       labels = c(
                         "1" = "Exploratory",
                         "2" = "Stepwise",
                         "3" = "Gradual"
                       )) +
    scale_fill_manual(values = c("1" = "hotpink", "2" = "#a2bffe", "3" = "#e89c7b"),
                      labels = c(
                        "1" = "Exploratory",
                        "2" = "Stepwise",
                        "3" = "Gradual"
                      )) +
    labs(
      x     = "Trial",
      y     = "Reach Deviation (°)",
      title = "",
      color = "Phenotype",
      fill  = "Phenotype"
    ) +
    theme_minimal() +
    theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black")) 
  print(p)
  
  
  
  anova_df <- combined_clean %>%
    filter(trial_type == "nocursor",
    ) %>% 
    group_by(participant_id,group, rotation) %>%
    summarise(
      aftereffect = mean(reachdeviation_deg),
      .groups = "drop"
    )
  
  summary_df <- summary_df %>%
    mutate(rel_trial = cutrial_no - 232)
  
}


washoutClusterStats <- function () {
  strategy_data <- read.csv("data/strategy_only_participants.csv")

  washout_clean <- strategy_data %>%
    group_by(participant_id) %>%
    mutate(
      mean_reach = mean(reachdeviation_deg, na.rm = TRUE),
      sd_reach   = sd(reachdeviation_deg, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(
      reachdeviation_deg >= (mean_reach - 3*sd_reach) &
        reachdeviation_deg <= (mean_reach + 3*sd_reach)
    )

  washout_first <- washout_clean %>%
    filter(trial_type == "nocursor",
           cutrial_no == 234:241) %>%
    left_join(pca_df %>% select(participant_id, cluster),
              by = "participant_id")

  anova_model <- aov(reachdeviation_deg ~ cluster, data = washout_first)
  summary(anova_model)

  washout_first$cluster <- as.factor(washout_first$cluster)
  washout_first$rotation <- as.factor(washout_first$rotation)

  
  bf_full <- anovaBF(
    reachdeviation_deg ~ cluster*rotation,
    data = washout_first
  )

  bf_full
}

# 
# strategyWashout <- function () {
#     
#     learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors = FALSE)
#     ci_result <- getCI()
#     CI_df <- ci_result$CI
#     strategy_df <- getStrategies()
#     
#     
#     # strategy_df has participant_id and strategy
#     yes_participants <- strategy_df %>% filter(strategy == "Yes") %>% pull(participant_id)
#     no_participants  <- strategy_df %>% filter(strategy == "No") %>% pull(participant_id)
#     
#     
#     
#     washout <- learners_data %>%
#       filter(trial_type == "nocursor") %>% 
#       #  !rotation %in% c(20, 30)) %>% 
#       group_by(participant_id) %>%
#       slice_tail(n = 24) %>%
#       ungroup() %>%
#       mutate(strategy = case_when(
#         participant_id %in% yes_participants ~ "Yes",
#         participant_id %in% no_participants  ~ "No"
#       ))
#     
#     rotated <- learners_data %>%
#       filter(trial_type == "rotated") %>% 
#       #    !rotation %in% c(20, 30)) %>% 
#       group_by(participant_id) %>%
#       slice_tail(n = 8) %>%
#       ungroup() %>%
#       mutate(strategy = case_when(
#         participant_id %in% yes_participants ~ "Yes",
#         participant_id %in% no_participants  ~ "No"
#       ))
#     
#     total <- bind_rows(washout,rotated)
#     
#     total_clean <- total %>%
#       group_by(rotation) %>%  # group by rotation
#       mutate(
#         mean_reach = mean(reachdeviation_deg, na.rm = TRUE),
#         sd_reach   = sd(reachdeviation_deg, na.rm = TRUE)
#       ) %>%
#       ungroup() %>%
#       filter(
#         reachdeviation_deg >= (mean_reach - 3*sd_reach) &
#           reachdeviation_deg <= (mean_reach + 3*sd_reach)
#       ) %>%
#       select(-mean_reach, -sd_reach) 
#     
#     summary_df <-   total_clean %>%
#       group_by(cutrial_no, strategy, rotation) %>%
#       summarise(
#         mean_reach = mean(reachdeviation_deg, na.rm = TRUE),  # use reach deviation from learners_data
#         sd_reach   = sd(reachdeviation_deg, na.rm = TRUE),
#         n          = n(),
#         .groups = "drop"
#       ) %>%
#       mutate(
#         se = sd_reach / sqrt(n),
#         ci_lower = mean_reach - 1.96 * se,
#         ci_upper = mean_reach + 1.96 * se
#       )
# 
# rotation_colors <- c(
#   "20"  = "#a2bffe",
#   "30" = "hotpink",
#   "40" = "#e89c7b",
#   "50" = "#87ae73",
#   "60" = "#999999"
# )
# 
# summary_df <- summary_df %>%
#   mutate(strategy = factor(strategy, levels = c("Yes", "No")),
#          rotation = factor(rotation))  # ensure rotation is factor
# 
# p <- ggplot(summary_df, aes(x = cutrial_no, y = mean_reach,
#                             color = rotation, fill = rotation)) +
#   geom_line(size = 1.2) +
#   geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
#   geom_vline(xintercept = 233, linetype = "dashed", colour = "black") +
#   geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
#   coord_cartesian(ylim = c(-20, 70), xlim= c(224,243)) +
#   scale_color_manual(values = rotation_colors, name = "Rotation (°)") +
#   scale_fill_manual(values = rotation_colors, name = "Rotation (°)") +
#   scale_color_manual(
#     values = rotation_colors,
#     name = "Rotation (°)",
#     labels = c("20° (n = 10/30)", "30° (n = 21/17)", "40° (n = 23/17)", "50° (n = 28/11)", "60° (n = 32/6)")  # <- your custom labels
#   ) +
#   scale_fill_manual(
#     values = rotation_colors,
#     name = "Rotation (°)",
#     labels = c("20° (n = 10/30)", "30° (n = 21/17)", "40° (n = 23/17)", "50° (n = 28/11)", "60° (n = 32/6)") 
#   ) +
#   labs(
#     x     = "Trial",
#     y     = "Reach Deviation (°)",
#     title = ""
#   ) +
#   facet_wrap(~ strategy, nrow = 1) +  
#   theme_minimal() +
#   theme(legend.position = "right") +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_blank(),
#     axis.line = element_line(),
#     axis.text.x  = element_text(size = 24),
#     axis.text.y  = element_text(size = 24),
#     axis.title.x = element_text(size = 17),
#     axis.title.y = element_text(size = 17),
#     legend.title = element_text(size = 18),
#     legend.text  = element_text(size = 17),
#     strip.text = element_text(size = 17)
#   )
# 
# p
# }


combined <- setUpFive()


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


summary_df <- combined_clean %>%
  group_by(cutrial_no, group) %>%
  summarise(
    mean_reach = mean(reachdeviation_deg, na.rm = TRUE),
    sd_reach   = sd(reachdeviation_deg, na.rm = TRUE),
    n          = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_reach / sqrt(n),
    ci_lower = mean_reach - 1.96 * se,
    ci_upper = mean_reach + 1.96 * se,
    rel_trial = cutrial_no - 232,
    group = factor(group)
  )
summary_df <- summary_df %>%
  mutate(group = factor(group))

summary_df <- summary_df %>%
  mutate(rel_trial = cutrial_no - 232)

p <-ggplot(summary_df, aes(x = rel_trial, y = mean_reach,
                           color = group, fill = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  coord_cartesian(ylim = c(-15, 70)) +
  scale_color_manual(
    values = c(
      "Group 2" = "#2F4858",
      "Non-strategy learners" = "#BC4749"
    ),
    labels = c(
      "Group 2" = "Strategy",
      "Non-strategy learners" = "No strategy"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Group 2" = "#2F4858",
      "Non-strategy learners" = "#BC4749"
    ),
    labels = c(
      "Group 2" = "Strategy",
      "Non-strategy learners" = "No strategy"
    )
  ) + 
  labs(
    x     = "Trial",
    y     = "Reach Deviation (°)",
    title = "",
    color = "Strategy Use",
    fill  = "Strategy Use"
  ) +
  theme_minimal() +
  theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black")) 
print(p)


