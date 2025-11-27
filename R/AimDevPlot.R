library(ggplot2)
strat_data <- read.csv("data/strategy_only_participants.csv")

library(dplyr)
library(ggplot2)
library(Reach)


setupAIM <- function(strat_data) {
  clean_data <- strat_data %>%
    filter(trial_type.x %in% c("aligned", "rotated")) %>%
    group_by(participant_id, rotation) %>%
    mutate(rotation_onset = min(cutrial_no[trial_type.x == "rotated"])) %>%
    mutate(norm_trial = cutrial_no - rotation_onset) %>%
    ungroup() %>%
    group_by(participant_id) %>%
    mutate(
      mean_p = mean(aimdeviation_deg, na.rm = TRUE),
      sd_p   = sd(aimdeviation_deg, na.rm = TRUE)
    ) %>%
    #outliers
    filter(
      aimdeviation_deg <= mean_p + 3 * sd_p,
      aimdeviation_deg >= mean_p - 3 * sd_p
    ) %>%
    ungroup()
  
  return(clean_data)
}

summarizeAIM <- function(clean_data) {
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

plotAIM <- function(summary_data) {
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
    
    geom_hline(
      data = hline_data,
      aes(yintercept = yintercept),
      color = "grey80", linewidth = 0.6
    ) +
    
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    
    facet_wrap(
      ~ rotation,
      ncol = 2,
      labeller = as_labeller(c(
        "20" = "20° Rotation",
        "30" = "30° Rotation",
        "40" = "40° Rotation",
        "50" = "50° Rotation",
        "60" = "60° Rotation"
      ))
    ) +
    
    scale_y_continuous(limits = c(-15, 60)) +
    
    scale_color_manual(values = c(
      "20"="#B9D3EE","30"="#85adf3","40"="#87CEEB","50"="#4682B4","60"="#271716"
    )) +
    
    labs(
      x = "Trial (aligned to rotation onset)",
      y = "Aim deviation (°)",
      color = "Rotation (°)"
    ) +
    
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      strip.background = element_rect(fill = "grey95", color = NA),
      strip.text = element_text(size = 14, face = "bold"),
      axis.line = element_line(color = "black")
    )
}