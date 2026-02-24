
library(dplyr)
library(ggplot2)
library(Reach)

setupREACH <- function() {
  total_learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors =FALSE)
  
  clean_data <- total_learners_data %>%
    filter(trial_type %in% c("aligned", "rotated")) %>%
    group_by(participant_id, rotation.x) %>%
    mutate(rotation_onset = min(cutrial_no[trial_type == "rotated"])) %>%
    
   ##normalize trials bc two groups experience rotation at different times points
    mutate(norm_trial = cutrial_no - rotation_onset) %>%
    ungroup() %>%
    
    # remove outliers that are 3 +/- from sd
    group_by(rotation.x) %>%
    mutate(
      mean_rot = mean(reachdeviation_deg, na.rm = TRUE),
      sd_rot   = sd(reachdeviation_deg, na.rm = TRUE)
    ) %>%
    filter(
      reachdeviation_deg <= mean_rot + 3 * sd_rot,
      reachdeviation_deg >= mean_rot - 3 * sd_rot
    ) %>%
    ungroup()
  
  summary_data <- clean_data %>%
    group_by(rotation.x, norm_trial) %>%
    summarise(
      mean_reach = mean(reachdeviation_deg, na.rm = TRUE),
      ci = Reach::getConfidenceInterval(reachdeviation_deg),
      ci_lower = ci[1],
      ci_upper = ci[2],
      .groups = "drop"
    )
  
  return(summary_data)
}


plotREACH <- function() {
  total_learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors =FALSE)
  
  summary_data <- setupREACH()
  rotation_levels <- sort(unique(summary_data$rotation.x))
  
  ggplot(summary_data, aes(
    x = norm_trial,
    y = mean_reach,
    color = factor(rotation.x),
    fill = factor(rotation.x)
  )) +
    geom_hline(
      yintercept = rotation_levels,
      color = "grey85", linetype = "solid", linewidth = 0.5
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                alpha = 0.2, color = NA) +
    
    scale_color_manual(
      values = c(
        "20"="#B9D3EE","30"="#85adf3","40"="#87CEEB",
        "50"="#4682B4","60"="#271716"
      ),
      breaks = c("60","50","40","30","20"),
      labels = c(
        "60° (n = 48)",
        "50° (n = 46)",
        "40° (n = 46)",
        "30° (n = 37)",
        "20° (n = 35)"
      )
    ) +
    scale_fill_manual(
      values = c(
        "60"="#271716", "50"="#4682B4","40"="#87CEEB",
        "30"="#85adf3", "20"="#B9D3EE"
      ),
      breaks = c("60","50","40","30","20"),
      labels = c(
        "60° (n = 48)",
        "50° (n = 46)",
        "40° (n = 46)",
        "30° (n = 37)",
        "20° (n = 35)"
      )
    )+
    
    labs(
      x = "Trial Number",
      y = "Reach deviation (°)",
      color = "Rotation (°)",
      fill = "Rotation (°)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      legend.position = "right",
      axis.line = element_line(color = "black")
    )
}




