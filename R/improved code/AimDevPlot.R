setupAIM <- function(strat_data) {
  
  clean_data <- strat_data %>%
    filter(trial_type %in% c("aligned", "rotated")) %>%
    group_by(participant_id, rotation) %>%
    mutate(rotation_onset = min(cutrial_no[trial_type == "rotated"])) %>%
    mutate(norm_trial = cutrial_no - rotation_onset) %>%
    ungroup() %>%
    
    # REMOVE OUTLIERS PER PARTICIPANT 
    group_by(participant_id) %>%
    mutate(
      mean_p = mean(aimdeviation_deg, na.rm = TRUE),
      sd_p   = sd(aimdeviation_deg, na.rm = TRUE)
    ) %>%
    filter(
      aimdeviation_deg <= mean_p + 3 * sd_p,
      aimdeviation_deg >= mean_p - 3 * sd_p
    ) %>%
    ungroup()
  
  
  summary_data <- clean_data %>%
    group_by(rotation, norm_trial) %>%
    summarise(
      mean_aim = mean(aimdeviation_deg, na.rm = TRUE),
      ci = Reach::getConfidenceInterval(aimdeviation_deg),
      ci_lower = ci[1],
      ci_upper = ci[2],
      .groups = "drop"
    )
  
  return(summary_data)
}

plotAIM <- function(summary_data) {
  
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
    
    scale_color_manual(values = c(
      "20"="#B9D3EE","30"="#85adf3","40"="#87CEEB",
      "50"="#4682B4","60"="#271716"
    )) +
    scale_fill_manual(values = c(
      "20"="#B9D3EE","30"="#85adf3","40"="#87CEEB",
      "50"="#4682B4","60"="#271716"
    )) +
    
    labs(
      x = "Trial Number",
      y = "Aim deviation (°)",
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

plotAIM(summary_data)







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


