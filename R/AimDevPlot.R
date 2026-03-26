library(ggplot2)

library(dplyr)
library(ggplot2)
library(Reach)


setupAIM <- function() {
  strat_data <- read.csv("data/strategy_only_participants.csv")
  
  clean_data <- strat_data %>%
    filter(trial_type.x %in% c("aligned", "rotated")) %>%
    group_by(participant_id, rotation) %>%
    # find the first rotated trial
    mutate(rotation_onset = min(cutrial_no[trial_type.x == "rotated"])) %>%
    mutate(norm_trial = cutrial_no - rotation_onset) %>%
    ungroup() %>%
    group_by(participant_id) %>%
    # compute baseline stats for trials before rotation
    mutate(
      mean_baseline = mean(aimdeviation_deg[norm_trial < 0], na.rm = TRUE),
      sd_baseline   = sd(aimdeviation_deg[norm_trial < 0], na.rm = TRUE),
      mean_rotated  = mean(aimdeviation_deg[norm_trial >= 0], na.rm = TRUE),
      sd_rotated    = sd(aimdeviation_deg[norm_trial >= 0], na.rm = TRUE)
    ) %>%
    # filter outliers separately for pre- and post-rotation
    filter(
      (norm_trial < 0 &
         aimdeviation_deg >= mean_baseline - 0.5 * sd_baseline &
         aimdeviation_deg <= mean_baseline + 0.5 * sd_baseline) |
        (norm_trial >= 0 &
           aimdeviation_deg >= mean_rotated - 4 * sd_rotated &
           aimdeviation_deg <= mean_rotated + 4 * sd_rotated)
    )%>%
    ungroup() %>%
    select(-mean_baseline, -sd_baseline, -mean_rotated, -sd_rotated)
}

summarizeAIM <- function() {
  clean_data <- setupAIM()
  
  summary_data <- clean_data %>%
    group_by(rotation, norm_trial) %>%
    summarise(
      mean_aim = mean(aimdeviation_deg, na.rm = TRUE),
      ci = list(Reach::getConfidenceInterval(aimdeviation_deg)),
      .groups = "drop"
    ) %>%
    mutate(
      ci_lower = sapply(ci, `[`, 1),
      ci_upper = sapply(ci, `[`, 2)
    ) %>%
    select(-ci)
  
  return(summary_data)
}


plotAIM <- function() {
  clean_data <- setupAIM()
  summary_data <- summarizeAIM()
  hline_data <- data.frame(rotation = c(20, 30, 40, 50, 60),
                           yintercept = c(20, 30, 40, 50, 60))
  
  ggplot() +
    geom_line(
      data = clean_data,
      aes(x = norm_trial, y = aimdeviation_deg, group = participant_id),
      color = "grey70", alpha = 0.4, linewidth = 0.4
    ) +
    
    geom_line(
      data = summary_data,
      aes(x = norm_trial, y = mean_aim, color = factor(rotation)),
      size = 1
    ) +
    coord_cartesian(xlim = c(-8, 120)) +
    geom_hline(
      data = hline_data,
      aes(yintercept = yintercept),
      color = "grey",linetype="dashed", linewidth = 0.6
    ) +
    
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    
    facet_wrap(
      ~ rotation,
      ncol = 3,
      labeller = as_labeller(c(
        "20" = "20° Rotation (n = 9)",
        "30" = "30° Rotation (n = 21)",
        "40" = "40° Rotation (n = 22)",
        "50" = "50° Rotation (n = 28)",
        "60" = "60° Rotation (n = 32)"
      ))
    ) +
    
    scale_y_continuous(limits = c(-15, 60)) +
    
    scale_color_manual(values = c(
      "60"="grey4", "50"="#87ae73","40"="#e89c7b",
      "30"="hotpink", "20"="#a2bffe"
    )) +
    
    labs(
      x = "Trial Number",
      y = "Aim deviation (°)",
      color = "Rotation (°)"
    ) +
    
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      strip.background = element_blank(),
      strip.text = element_text(size = 14, face = "bold"),
      axis.line = element_line(color = "black"),
      axis.text.x  = element_text(size = 15),
      axis.text.y  = element_text(size = 15),
      axis.title.x = element_text(size = 17),
      axis.title.y = element_text(size = 17),
      legend.title = element_text(size = 18),
      legend.text  = element_text(size = 17) 
    )
}




plotAIM2 <- function() {
  strat_data <- read.csv("data/strategy_only_participants.csv")
  
  clean_data <- setupAIM()
  summary_data <- summarizeAIM()
  hline_data <- data.frame(rotation = c(20, 30, 40, 50, 60),
                           yintercept = c(20, 30, 40, 50, 60))
  rotation_levels <- sort(unique(summary_data$rotation))
 
   
  ggplot(summary_data, aes(
    x = norm_trial,
    y = mean_aim,
    color = factor(rotation),
    fill = factor(rotation)
  )) +
    geom_hline(
      yintercept = rotation_levels,
      color = "grey85", linetype = "solid", linewidth = 0.5
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                alpha = 0.2, color = NA) +
    coord_cartesian(xlim = c(-24, 120)) +
    scale_color_manual(
      values = c(
        "60"="#5f6182", "50"="#9fcbe6","40"="#af89b6",
        "30"="#fbc5b0", "20"="#e7c485"
      ),
      breaks = c("60","50","40","30","20"),
      labels = c(
        "60° (n = 9)",
        "50° (n = 21)",
        "40° (n = 22)",
        "30° (n = 28)",
        "20° (n = 32)"
      ) 
    ) +
    scale_fill_manual(
      values = c(
        "60"="grey1", "50"="#9fcbe6","40"="#B19CD7",
        "30"="#fcb0c6", "20"="#e7c485"
      ),
      breaks = c("60","50","40","30","20"),
      labels = c(
        "60° (n = 9)",
        "50° (n = 21)",
        "40° (n = 22)",
        "30° (n = 28)",
        "20° (n = 32)"
      )
      
    )+
    
    labs(
      x = "Trial Number",
      y = "Aim deviation (°)",
      color = "Rotation (°)",
      fill = "Rotation (°)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(),
      axis.text.x  = element_text(size = 24),
      axis.text.y  = element_text(size = 24),
      axis.title.x = element_text(size = 17),
      axis.title.y = element_text(size = 17),
      legend.title = element_text(size = 18),
      legend.text  = element_text(size = 17) 
    )
}





## non-startegy
non <- function () {
  total_learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors = FALSE)
  
  strategy_df <- getStrategies()
  
  no_strategy_ids <- strategy_df %>%
    filter(strategy == "No") %>%
    pull(participant_id)
    
  clean_data <- total_learners_data %>%
    filter(participant_id %in% no_strategy_ids) %>%
    group_by(participant_id) %>%
    arrange(cutrial_no, .by_group = TRUE) %>%
    filter(
      (trial_type == "aligned" & row_number() %in% tail(which(trial_type == "aligned"), 8)) |
        trial_type == "rotated"
    ) %>%
    ungroup()
    
  clean_data <- clean_data %>%
    group_by(participant_id) %>%
    arrange(cutrial_no, .by_group = TRUE) %>%
    mutate(cutrial_no = row_number() - 9) %>%  # makes aligned = -8:-1, rotated = 1:120
    ungroup()
  
  summary_data <- clean_data %>%
    group_by(rotation, cutrial_no) %>%
    summarise(
      mean_aim = mean(aimdeviation_deg, na.rm = TRUE),
      ci = list(Reach::getConfidenceInterval(aimdeviation_deg)),
      .groups = "drop"
    ) 
  

  mean_df <- clean_data %>%
    group_by(rotation, cutrial_no) %>%
    summarise(
      mean_aim = mean(aimdeviation_deg, na.rm = TRUE),
      .groups = "drop"
    )
  
  ggplot(mean_df, aes(x = cutrial_no, y = mean_aim)) +
    geom_line(size = 1, color = "salmon") +
    geom_vline(xintercept = 0, linetype = "dashed") +
   # geom_hline(yintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = 5, linetype = "dashed", col="grey2") +
    coord_cartesian(ylim = c(-20, 80), xlim= c(-8,120)) +
    facet_wrap(~rotation) +
    labs(
      x = "Trial",
      y = "Mean Aim Deviation (deg)",
      title = ""
    ) +
    theme_classic() +
    theme(
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = 14, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(),
      axis.text.x  = element_text(size = 17),
      axis.text.y  = element_text(size = 17),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15)
    )
}

ci <- function () {
  total_learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors = FALSE)
  
  no_strategy_ids <- non()
# Step 1: align trials relative to rotation onset
plot_df <- total_learners_data %>%
  filter(participant_id %in% no_strategy_ids) %>%
  mutate(
    rotation_onset = ifelse(rotation %in% c(20,30,40), 90, 113),
    rel_trial = cutrial_no - rotation_onset
  ) %>%
  filter(rel_trial >= -8)

# Step 2: compute mean across all participants and all rotations
overall_mean <- plot_df %>%
  group_by(rel_trial) %>%
  summarise(
    mean_aim = mean(aimdeviation_deg, na.rm = TRUE),
    .groups = "drop"
  )


# Step 3: plot
overall_ci <-clean_data %>%
  group_by(cutrial_no) %>%
  summarise(
    mean_aim = mean(aimdeviation_deg, na.rm = TRUE),
    se = sd(aimdeviation_deg, na.rm = TRUE) / sqrt(n()),      # standard error
    lower = mean_aim - 1.96 * se,                             # 95% CI
    upper = mean_aim + 1.96 * se,
    .groups = "drop"
  )

# Step 2: plot with CI ribbon

ggplot(overall_ci, aes(x = cutrial_no, y = mean_aim)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "red") +
  geom_line(size = 1.5, color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed") +
 # geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 5, linetype = "dashed", col="grey2") +
  coord_cartesian(ylim = c(-20, 80), xlim=c(-8,120)) +
  labs(
    x = "Trial (relative to rotation onset)",
    y = "Mean Aim Deviation (deg)",
    title = ""
  ) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(),
    axis.text.x  = element_text(size = 21),
    axis.text.y  = element_text(size = 21),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17)
  )
}







