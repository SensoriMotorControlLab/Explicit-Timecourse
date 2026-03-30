
library(dplyr)
library(ggplot2)
library(Reach)

setupREACH <- function() {
  total_learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors =FALSE)
  
  clean_data <- total_learners_data %>%
    filter(trial_type %in% c("aligned", "rotated")) %>%
    group_by(participant_id, rotation) %>%
    mutate(rotation_onset = min(cutrial_no[trial_type == "rotated"])) %>%
    
   ##normalize trials bc two groups experience rotation at different times points
    mutate(norm_trial = cutrial_no - rotation_onset) %>%
    ungroup() %>%
    
    # remove outliers that are 3 +/- from sd
    group_by(rotation) %>%
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
    group_by(rotation, norm_trial) %>%
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
  rotation_levels <- sort(unique(summary_data$rotation))
  
  ggplot(summary_data, aes(
    x = norm_trial,
    y = mean_reach,
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
        "60"="#999999", "50"="#87ae73","40"="#e89c7b",
        "30"="hotpink", "20"="#a2bffe"
      ),
      breaks = c("60","50","40","30","20"),
      labels = c(
        "60° (n = 38 /32)",
        "50° (n = 39 /28)",
        "40° (n = 39 /23)",
        "30° (n = 38 /21)",
        "20° (n = 39 /10)"
      ) 
    ) +
    scale_fill_manual(
      values = c(
        "60"="#999999", "50"="#87ae73","40"="#e89c7b",
        "30"="hotpink", "20"="#a2bffe"
      ),
      breaks = c("60","50","40","30","20"),
      labels = c(
        "60° (n = 38 /32)",
        "50° (n = 39 /28)",
        "40° (n = 39 /23)",
        "30° (n = 38 /21)",
        "20° (n = 39 /10)"
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




