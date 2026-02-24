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
           aimdeviation_deg >= mean_rotated - 3 * sd_rotated &
           aimdeviation_deg <= mean_rotated + 3 * sd_rotated)
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
        "20" = "20° Rotation (n = 8)",
        "30" = "30° Rotation (n = 19)",
        "40" = "40° Rotation (n = 27)",
        "50" = "50° Rotation (n = 32)",
        "60" = "60° Rotation (n = 40)"
      ))
    ) +
    
    scale_y_continuous(limits = c(-15, 60)) +
    
    scale_color_manual(values = c(
      "20"="#FFBBFF","30"="#DA70D6","40"="#AB82FF","50"="#5D4784","60"="#271716"
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
      strip.background = element_rect(fill = "grey95", color = NA),
      strip.text = element_text(size = 14, face = "bold"),
      axis.line = element_line(color = "black")
    )
}