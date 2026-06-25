
library(tidyverse)


ntargets <- 8
gap <- 20
target_distance <- 8      # cm
target_radius <- 0.25     # 0.5 cm diameter

start_angle <- (180 - ((ntargets - 1) * gap)) / 2

# Target locations
targets_df <- tibble(x_idx = 0:(ntargets - 1)) %>%
  mutate(
    angle_deg = (x_idx * gap) + start_angle,
    angle_rad = angle_deg * pi / 180,
    target_x = target_distance * cos(angle_rad),
    target_y = target_distance * sin(angle_rad)
  )

# Stencil circle
stencil_circle <- tibble(theta = seq(0, 2*pi, length.out = 200)) %>%
  mutate(
    x = target_distance * cos(theta),
    y = target_distance * sin(theta)
  )
outer_stencil <- tibble(theta = seq(0, 2*pi, length.out = 200)) %>%
  mutate(
    x = 8.5 * cos(theta),
    y = 8.5 * sin(theta)
  )


start_circle <- tibble(theta = seq(0, 2*pi, length.out = 200)) %>%
  mutate(
    x = target_radius * cos(theta),
    y = target_radius * sin(theta)
  )

# Target circles (0.5 cm diameter)
target_circles <- targets_df %>%
  rowwise() %>%
  do({
    tibble(
      angle_deg = .$angle_deg,
      x = .$target_x + target_radius * cos(seq(0, 2*pi, length.out = 100)),
      y = .$target_y + target_radius * sin(seq(0, 2*pi, length.out = 100))
    )
  })
label_radius <- 9.25

labels_df <- targets_df %>%
  mutate(
    label_x = label_radius * cos(angle_rad),
    label_y = label_radius * sin(angle_rad)
  )


stencil_circle <- tibble(theta = seq(0, pi, length.out = 100)) %>%
  mutate(
    x = target_distance * cos(theta),
    y = target_distance * sin(theta)
  )

# Keeps full 0 to 2*pi for a full circle
outer_stencil <- tibble(theta = seq(0, 2*pi, length.out = 200)) %>%
  mutate(
    x = 8.5 * cos(theta),
    y = 8.5 * sin(theta)
  )

start_circle <- tibble(theta = seq(0, 2*pi, length.out = 200)) %>%
  mutate(
    x = target_radius * cos(theta),
    y = target_radius * sin(theta)
  )

# Target circles (0.5 cm diameter)
target_circles <- targets_df %>%
  rowwise() %>%
  do({
    tibble(
      angle_deg = .$angle_deg,
      x = .$target_x + target_radius * cos(seq(0, 2*pi, length.out = 100)),
      y = .$target_y + target_radius * sin(seq(0, 2*pi, length.out = 100))
    )
  })

label_radius = 9.25

labels_df <- targets_df %>%
  mutate(
    label_x = label_radius * cos(angle_rad),
    label_y = label_radius * sin(angle_rad)
  )

ggplot() +
  geom_hline(yintercept = 0, color = "gray90") +
  geom_vline(xintercept = 0, color = "gray90") +

  geom_path(
    data = stencil_circle,
    aes(x, y),
    color = "gray70",
    linetype = "dashed"
  ) +
  
  geom_segment(
    data = targets_df,
    aes(x = 0, y = 0, xend = target_x, yend = target_y),
    color = "gray85",
    linetype = "dotted"
  ) +
  
  geom_polygon(
    data = start_circle,
    aes(x, y),
    fill = "red",
    color = "red"
  ) +
  
  geom_polygon(
    data = target_circles,
    aes(x, y, group = angle_deg),
    fill = NA,
    color = "#009900",
    linewidth = 1
  ) +
  

  geom_path(
    data = outer_stencil,
    aes(x, y),
    color = "black",
    linewidth = 1
  ) +
  
  geom_text(
    data = labels_df,
    aes(x = label_x,
        y = label_y,
        label = paste0(angle_deg, "°")),
    size = 4
  ) +
  
  annotate("text", x = 1.45, y = 1.6, label = "r = 8.5 cm", size = 4, col="#909090") +
  
  
  coord_fixed(
    xlim = c(-9, 9),
    ylim = c(-9, 9)
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )




####
plotSchedule <- function() {
  
  
  schedule <- data.frame(
    phase = c(
      "Aligned", "No-cursor", "Aligned", "Error-clamp",
      "Aligned", "No-cursor", "Aligned", "No-cursor", 
      "Aligned", "Aligned", "Rotation", "No-cursor"
    ),
    trials = c(
      24, 16, 16, 8,
      8, 8, 8, 8, 8,
      8, 120, 24
    )
  )
  
  schedule <- schedule %>%
    mutate(
      condition = phase,
      row = 1
    ) %>%
    mutate(
      end = cumsum(trials),
      start = lag(end, default = 0),
      mid = (start + end) / 2,
      # Make text white on dark blue blocks, black on others for readability
      text_color = ifelse(phase == "No-cursor", "white", "black")
    )
  
  my_colors <- c(
    "Aligned" = "#c9dce6",
    "No-cursor" = "#243762",
    "Error-clamp" = "#d9dddc",
    "Rotation" = "#d8a1c4"
  )
  
  aligned_end <- 104
  aligned_mid <- aligned_end / 2
  rotation_mid <- 175.5
  
  ggplot(schedule) +
    # The Boxes (Y spans from 0.7 to 1.3)
    geom_rect(
      aes(
        xmin = start, xmax = end,
        ymin = 0.7, ymax = 1.3,
        fill = condition
      ),
      color = "black"
    ) +
    
    # Centered Trial Numbers
    geom_text(
      aes(
        x = mid, 
        y = 1.0,             # Dead center vertically between 0.7 and 1.3
        label = trials,
        color = text_color   # Dynamic color mapping for contrast
      ),
      vjust = 0.5,           # Hard vertical center anchor
      hjust = 0.5,           # Hard horizontal center anchor
      size = 3.5,
      show.legend = FALSE    # Hide text color from the legend
    ) +
    
    # Arrow reminder marker
    geom_vline(
      xintercept = aligned_end,
      linetype = "dashed",
      linewidth = 0.6
    ) +
    
    annotate(
      "text",
      x = 125,
      y = 1.55,
      label = "Aim Reminder",
      size = 3.2
    ) +
    
    
    # annotate(
    #   "text",
    #   x = aligned_mid,
    #   y = 0.45,
    #   label = "Aligned Phase",
    #   size = 4
    # ) +
    
    annotate(
      "text",
      x = 134,
      y = 1.37,
      label = "Rotation Onset",
      size = 3,
      col="#909090"
    ) +
    
    
    scale_fill_manual(values = my_colors) +
    scale_color_identity() +  
    
    coord_cartesian(ylim = c(0.35, 1.7)) +
    
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )
}



timecourseCartoon <- function() {
  
  par(mar = c(4.5, 4.5, 2, 9))
  
  plot(NULL, NULL,
       xlim = c(-20, 80), ylim = c(-5, 65),
       xlab = 'Trial', ylab = 'Rotation',
       bty = 'n', axes = FALSE) 
  
  timepoints = seq(0, 80, 0.1)
  implicit = 20 - (20 * (1 - .03)^timepoints)
  explicit = 39 - (39 * (1 - .35)^timepoints)
  adaptation = implicit + explicit
  
  # 2. Draw the lines
  lines(x = c(-20, timepoints + 1),
        y = c(0, implicit), 
        col = 'purple', lwd = 2)
  lines(x = c(-20, timepoints + 1),
        y = c(0, explicit), 
        col = 'blue', lwd = 2)
  lines(x = c(-20, timepoints + 1),
        y = c(0, adaptation),
        col = 'darkorange', lwd = 2)
  
  # Target line
  lines(x = c(-20, 0.5,   0.5, 80),
        y = c(0, 0,     60  , 60),
        col = 'black', lty = 2, lwd = 2)
  
  # 3. Add the axes
  axis(side = 1, at = c(-20, 0, 20, 40, 60, 80))
  axis(side = 2, at = c(0, 20, 40, 60))
  
  # 4. Add the legend
  legend(x = 87, y = 38,       # Use exact coordinates slightly past the max x-axis (80)
         legend = c("Implicit", "Explicit", "Adaptation"), 
         col = c("purple", "blue", "darkorange"), 
         lwd = 2,      
         bty = "n",
         xpd = TRUE)
}

# Clear any broken plots first
dev.off() 

# Run it!
timecourseCartoon()