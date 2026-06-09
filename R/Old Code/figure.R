setupFigureFile <- function(target='inline',width=8,height=6,dpi=300,filename) {
  
  if (target == 'pdf') {
    pdf(file   = filename, 
        width  = width, 
        height = height)
  }
  if (target == 'svg') {
    svglite::svglite( filename = filename,
                      width = width,
                      height = height,
                      fix_text_size = FALSE) 
    # fix_text_size messes up figures on my machine... 
    # maybe it's better on yours?
  }
  if (target == 'png') {
    png( filename = filename,
         width = width*dpi,
         height = height*dpi,
         res = dpi
    )
  }
  if (target == 'tiff') {
    tiff( filename = filename,
          compression = 'lzw',
          width = width*dpi,
          height = height*dpi,
          res = dpi
    )
  }
}


plotMeanAim <- function(target='inline', main=NULL) {
  
  setupFigureFile(target=target,
                  width = 3,
                  height=3,
                  dpi=300,
                  sprintf('doc/fig2c_learningrates.%s', target))
  
  grouped_strategy_data <- total_learners_data %>%
    mutate(group = ifelse(participant_id %in% strategy_ids, "Yes", "No"))
  
  grouped_strategy_data <- grouped_strategy_data %>%
    mutate(group = ifelse(participant_id == "4eeaee", "Yes", group))
  
  plot_mean_aim_data <- grouped_strategy_data %>%
    filter(cutrial_no %in% c(201:208, 225:232)) %>%
    group_by(rotation, participant_id, group) %>%
    summarise(mean_aim = mean(aimdeviation_deg, na.rm = TRUE), .groups = "drop")
  
  plot_mean_aim_data <- plot_mean_aim_data %>%
    mutate(fill_color = ifelse(group == "Yes", as.character(rotation), "white"))
  max_y <- max(plot_mean_aim_data$mean_aim, na.rm = TRUE)
  
  if(!is.null(main)) {
    title(main=main, adj=0, cex.main=2)
  }
  
  p <- ggplot(plot_mean_aim_data, aes(x = factor(rotation), y = mean_aim, color = factor(rotation))) +
    geom_point(aes(shape = group, fill = factor(rotation)), size = 6, stroke = 1.2) +
    
    scale_shape_manual(
      values = c("Yes" = 21, "No" = 4),
      name = "Correct aiming strategy"
    ) +
    
    scale_color_manual(
      values = c(
        "20" = "#b9d6e5",
        "30" = "#9fbdd8",
        "40" = "#848fbe",
        "50" = "#74599c",
        "60" = "#6b0077"
      ),
      name = "Rotation (degrees)"
    ) +
    
    scale_fill_manual(
      values = c(
        "20" = "#b9d6e5",
        "30" = "#9fbdd8",
        "40" = "#848fbe",
        "50" = "#74599c",
        "60" = "#6b0077"
      ),
      guide = "none"
    ) +
    
    guides(
      color = "none",
      fill = "none",
      shape = guide_legend(
        override.aes = list(
          shape = c(4, 21),                     # Yes = filled circle (21), No = x (4)
          color = c("#C7E5Be", "#165660"),      # outline color for both
          fill = c(NA, "#165660")))
    ) +
    
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      legend.position = "inside",
      legend.position.inside = c(0.05, 0.94),
      legend.justification = c(0, 1), 
      legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2),
      legend.text = element_text(size = 16),     # legend item labels
      legend.title = element_text(size = 17, face = "bold"),
    ) +
    theme(
      axis.title.x = element_text(size = 17),
      axis.title.y = element_text(size = 17),
      axis.text.x  = element_text(size = 16),
      axis.text.y  = element_text(size = 16),
      legend.title = element_text(size = 16),
      legend.text  = element_text(size = 15),
      plot.title   = element_text(size = 19, hjust = 0)
    ) + 
    labs(
      x = "",
      y = expression("")
    )
  
  if (target %in% c('pdf','svg','png','tiff')) {
    dev.off()
  }
  print(p)
} 



plotSteps <- function (target = "inline", main = NULL) {
  setupFigureFile(target=target,
                  width = 3,
                  height=3,
                  dpi=300,
                  sprintf('images/plotsteps.%s', target))
  
  df_steps <- result_table %>%
    rowwise() %>%  # Add the pipe before rowwise
    mutate(
      trials = list(-8:50),
      aim_deviation = list(pmin(ifelse(-8:50 < first_trial, 0, first_aimdev), 60))
    ) %>%
    unnest(c(trials, aim_deviation))
  
  p <- ggplot(df_steps, aes(x = trials, y = aim_deviation, color = factor(rotation))) +
    geom_line(aes(group = participant_id), size = 0.8) +
    geom_vline(data = result_table, aes(xintercept = cutrial_no), linetype = "dashed", color = NA) +  # use NA not "NA"
    labs(
      x = "",
      y = "",
      color = "Rotation",
      title = ""
    ) +
    geom_vline(aes(xintercept = 0), linetype = "dashed", color = "grey60") +
    scale_color_manual(values = c(
      "20" = "darkorange",  # orange
      "30" = "mediumorchid",  # sky blue
      "40" = "red2",  # green
      "50" = "purple4",  # yellow
      "60" = "violetred"   # red
    )) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line()
    ) +
    theme(
      axis.title.x = element_text(size = 17),
      axis.title.y = element_text(size = 17),
      axis.text.x  = element_text(size = 16),
      axis.text.y  = element_text(size = 16),
      legend.title = element_text(size = 17),
      legend.text  = element_text(size = 16),
      plot.title   = element_text(size = 19, hjust = 0),
      legend.position = "inside",
      legend.position.inside = c(0.08, 0.5))
  
  if (target %in% c('pdf','svg','png','tiff')) {
    dev.off()
  }
  print(p)
  
  
} 



plotProportion <- function () {
  
  plot_data_yes <- logit_data %>%
    group_by(rotation) %>%
    summarise(
      count_yes = sum(group == "Yes"),
      total = n(),
      proportion_yes = count_yes / total,
      .groups = "drop"
    )
  
  custom_colors <- c(
    "20" = "darkorange",  # orange
    "30" = "mediumorchid",  # sky blue
    "40" = "red2",  # green
    "50" = "purple4",  # yellow
    "60" = "violetred"   # red
  )
  
  library(scales)
  
  ggplot(plot_data_yes, aes(x = rotation, y = proportion_yes, fill = rotation)) +
    geom_col() +
    scale_fill_manual(values = custom_colors) +
    geom_text(aes(label = c("n=2","n=2","n=7","n=8","n=15")), 
              vjust = -0.5, size = 5.5) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      x = "",
      y = "",
      fill = "Rotation",
      title = ""
    ) +
    guides(fill = "none") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black")
    ) +
    theme(
      axis.title.x = element_text(size = 17),
      axis.title.y = element_text(size = 17),
      axis.text.x  = element_text(size = 16),
      axis.text.y  = element_text(size = 16),
      legend.title = element_text(size = 17),
      legend.text  = element_text(size = 16),
      plot.title   = element_text(size = 19, hjust = 0))
  
  
  
  
}



####SIMULATES HISTOGRAMS - FROM POSTER  

plot_step_histogram <- function(sim_data) {
  plot(NA,
        main = 'Stepwise Pattern',
       xlab = 'Trial', ylab = 'Aim Deviation (deg)',
       xlim = c(-8, 32), ylim = c(-15, 65),
       ax = FALSE, bty = 'n',
       cex.lab = 1,     # Axis titles (xlab, ylab)
       cex.axis = 3,  # Axis numbers
       cex.main = 1.5)
  
  img_info <- hist2d(x = sim_data[, c("time", "aimdeviation_deg")],
                     edges = list(seq(-8, 31.5, 1), seq(-15, 65, 2.5)))
  
  img <- log(img_info$freq2D + 1)
  
  image(x = img_info$x.edges,
        y = img_info$y.edges,
        col = colorRampPalette(c("white", "#FFB281", "#F5546E", "#7D1D67"))(100),
        z = img,
        add = TRUE)
  
  axis(side = 1, at = c(-8, 0, 8, 16, 24, 32))
  axis(side = 2, at = seq(-10, 60, 10))
  lines(x = c(-8, 0, 0, 32), y = c(-0.5, -0.5, 64.5, 64.5),
        col = 'navy', lty = 3, lwd = 2)
}

simulate_step_function_variable_jump <- function(n_participants = 50,
                                                 jump_range = c(5, 20),
                                                 pre_mean = 0, post_mean = 60,
                                                 pre_sd = 5, post_sd = 9) {
  df_list <- list()
  
  for (i in 1:n_participants) {
    jump_trial <- sample(jump_range[1]:jump_range[2], 1)
    pid <- paste0("sim_", i)
    
    aligned <- data.frame(participant_id = pid,
                          time = -8:-1,
                          aimdeviation_deg = rnorm(8, pre_mean, pre_sd))
    
    rotated <- data.frame(participant_id = pid,
                          time = 0:31)
    
    rotated$aimdeviation_deg <- ifelse(rotated$time < jump_trial,
                                       rnorm(32, pre_mean, pre_sd),
                                       rnorm(32, post_mean, post_sd))
    
    df_list[[i]] <- rbind(aligned, rotated)
  }
  
  df <- do.call(rbind, df_list)
  df$rotation_group <- "60"
  return(df)
}


step_data_var_jump <- simulate_step_function_variable_jump(n_participants = 80)
plot_step_histogram(step_data_var_jump)






simulate_exponential_learning <- function(n_participants = 50,
                                          start_mean = 0,
                                          end_mean = 60,
                                          sd = 10,
                                          n_pre = 8,
                                          n_post = 32,
                                          rate = 0.15) {
  df_list <- list()
  
  for (i in 1:n_participants) {
    pid <- paste0("sim_", i)
    
    # Aligned phase: stable baseline
    aligned <- data.frame(
      participant_id = pid,
      time = -n_pre:-1,
      aimdeviation_deg = rnorm(n_pre, start_mean, sd)
    )
    
    time_post <- 0:(n_post - 1)
    exp_vals <- end_mean * (1 - exp(-rate * time_post))
    exp_means <- start_mean + exp_vals  # shifted so start near start_mean
    
    rotated <- data.frame(
      participant_id = pid,
      time = time_post,
      aimdeviation_deg = rnorm(n_post, exp_means, sd)
    )
    
    df_list[[i]] <- rbind(aligned, rotated)
  }
  
  df <- do.call(rbind, df_list)
  df$rotation_group <- "60"
  return(df)
}


exp_data <- simulate_exponential_learning(n_participants = 80)
plot_step_histogram(exp_data)




#########FOR GRAD SEMINAR PRES
#- gradual -#


## 
pid <- "422c52"
df <- strategy_data[strategy_data$participant_id == pid, ]
aligned_trials <- tail(df$aimdeviation_deg[df$trial_type == "aligned"], 16)
rotated_trials <- head(df$aimdeviation_deg[df$trial_type == "rotated"], 100)
transition_aim <- c(aligned_trials, rotated_trials)
transition_trials <- 1:length(transition_aim)

x_aligned <- seq(-length(aligned_trials), -1, 1)
x_rotated <- seq(0, length(rotated_trials) - 1, 1)
transition_trials <- c(x_aligned, x_rotated)


plot(transition_trials, transition_aim, type = "l", lwd = 3,
     col = "#3dcad4", ylim = c(-15, 80),
     xlab = "", ylab = "",
     main = paste(""),
     bty = "n",       
     cex.axis = 1.5,   
     cex.lab = 1.5,    
     cex.main = 1.8)

abline(v = 0, col = "grey", lty = 2, lwd = 2)  
abline(h = 40, col = "grey", lty = 2, lwd = 2)

##

pid <- "1d818f"
df <- strategy_data[strategy_data$participant_id == pid, ]
aligned_trials <- tail(df$aimdeviation_deg[df$trial_type == "aligned"], 16)
rotated_trials <- head(df$aimdeviation_deg[df$trial_type == "rotated"], 100)
transition_aim <- c(aligned_trials, rotated_trials)
transition_trials <- 1:length(transition_aim)

x_aligned <- seq(-length(aligned_trials), -1, 1)
x_rotated <- seq(0, length(rotated_trials) - 1, 1)
transition_trials <- c(x_aligned, x_rotated)


plot(transition_trials, transition_aim, type = "l", lwd = 3,
     col = "#3dcad4", ylim = c(-15, 80),
     xlab = "", ylab = "",
     main = paste(""),
     bty = "n",       
     cex.axis = 1.5,   
     cex.lab = 1.5,    
     cex.main = 1.8)

abline(v = 0, col = "grey", lty = 2, lwd = 2)  
abline(h = 40, col = "grey", lty = 2, lwd = 2)



#-- step --#

 #1c10b9
pid <- "3091de" #2528e1, 
df <- total_group_data[total_group_data$participant_id == pid, ]
aligned_trials <- tail(df$aimdeviation_deg[df$trial_type == "aligned"], 16)
rotated_trials <- head(df$aimdeviation_deg[df$trial_type == "rotated"], 120)
transition_aim <- c(aligned_trials, rotated_trials)
transition_trials <- 1:length(transition_aim)

x_aligned <- seq(-length(aligned_trials), -1, 1)
x_rotated <- seq(0, length(rotated_trials) - 1, 1)
transition_trials <- c(x_aligned, x_rotated)


plot(transition_trials, transition_aim, type = "l", lwd = 3,
     col = "#d16483", ylim = c(-15, 80),
     xlab = "", ylab = "",
     main = paste(""),
     bty = "n",       
     cex.axis = 1.5,   
     cex.lab = 1.5,    
     cex.main = 1.8)
abline(v = 0, col = "grey", lty = 2, lwd = 2)  
abline(h = 20, col = "grey", lty = 2, lwd = 2)
abline(h = 0, col = "grey", lty = 2, lwd = 2)


####ba0a7c afaaf4

pid <- "654648"
df <- strategy_data[strategy_data$participant_id == pid, ]
aligned_trials <- tail(df$aimdeviation_deg[df$trial_type == "aligned"], 16)
rotated_trials <- head(df$aimdeviation_deg[df$trial_type == "rotated"], 100)
transition_aim <- c(aligned_trials, rotated_trials)
transition_trials <- 1:length(transition_aim)

x_aligned <- seq(-length(aligned_trials), -1, 1)
x_rotated <- seq(0, length(rotated_trials) - 1, 1)
transition_trials <- c(x_aligned, x_rotated)


plot(transition_trials, transition_aim, type = "l", lwd = 3,
     col = "#007F83", ylim = c(-15, 80),
     xlab = "", ylab = "",
     main = paste(""),
     bty = "n",       
     cex.axis = 1.5,   
     cex.lab = 1.5,    
     cex.main = 1.8)
abline(v = 0, col = "grey", lty = 2, lwd = 2)  



# 7eacfc, 94709f is a 50 degree

##--exploratory--##



## a7178b, 
pid <- "9d628e"
df <- strategy_data[strategy_data$participant_id == pid, ]
aligned_trials <- tail(df$aimdeviation_deg[df$trial_type == "aligned"], 16)
rotated_trials <- head(df$aimdeviation_deg[df$trial_type == "rotated"], 100)
transition_aim <- c(aligned_trials, rotated_trials)
transition_trials <- 1:length(transition_aim)

x_aligned <- seq(-length(aligned_trials), -1, 1)
x_rotated <- seq(0, length(rotated_trials) - 1, 1)
transition_trials <- c(x_aligned, x_rotated)


plot(transition_trials, transition_aim, type = "l", lwd = 3,
     col = "black", ylim = c(-25, 80),
     xlab = "", ylab = "",
     main = paste(""),
     bty = "n",       
     cex.axis = 1.5,   
     cex.lab = 1.5,    
     cex.main = 1.8)

abline(v = 0, col = "grey", lty = 2, lwd = 2)  # trial 9 = first rotated trial
abline(h = 60, col = "grey", lty = 2, lwd = 2)


##
pid <- "1cce80"
df <- strategy_data[strategy_data$participant_id == pid, ]
aligned_trials <- tail(df$aimdeviation_deg[df$trial_type == "aligned"], 16)
rotated_trials <- head(df$aimdeviation_deg[df$trial_type == "rotated"], 120)
transition_aim <- c(aligned_trials, rotated_trials)
transition_trials <- 1:length(transition_aim)

x_aligned <- seq(-length(aligned_trials), -1, 1)
x_rotated <- seq(0, length(rotated_trials) - 1, 1)
transition_trials <- c(x_aligned, x_rotated)


plot(transition_trials, transition_aim, type = "l", lwd = 3,
     col = "salmon", ylim = c(-15, 80),
     xlab = "", ylab = "",
     main = paste(""),
     bty = "n",       
     cex.axis = 1.5,   
     cex.lab = 1.5,    
     cex.main = 1.8)

abline(v = 0, col = "grey", lty = 2, lwd = 2)  # trial 9 = first rotated trial
abline(h = 20, col = "grey", lty = 2, lwd = 2)

####
plotSchedule <- function() {
  library(dplyr)
  library(ggplot2)
  
  schedule <- data.frame(
    phase = c(
      "Aligned",
      "No-cursor",
      "Aligned",
      "Error-clamp",
      "Aligned",
      "No-cursor",
      "Aligned",
      "No-cursor",
      "Aligned",
      "Aligned",
      "Rotation",
      "No-cursor"
    ),
    trials = c(
      24, 16, 16, 8,
      8, 8, 8, 8, 8,
      8, 120, 24
    )
  )
  
  schedule <- schedule %>%
    mutate(
      condition = case_when(
        phase == "Aligned" ~ "Aligned",
        phase == "No-cursor" ~ "No-cursor",
        phase == "Error-clamp" ~ "Error-clamp",
        phase == "Rotation" ~ "Rotation"
      ),
      row = 1
    ) %>%
    mutate(
      end = cumsum(trials),
      start = lag(end, default = 0),
      mid = (start + end) / 2
    )
  
  my_colors <- c(
    "Aligned" = "#c9dce6",
    "No-cursor" = "#243762",
    "Error-clamp" = "white",
    "Rotation" = "#d8a1c4"
  )
  
  # ---- phase boundary ----
  aligned_end <- 104
  
  aligned_mid <- aligned_end / 2
  rotation_mid <- 175.5
  
  ggplot(schedule) +
    
    geom_rect(
      aes(
        xmin = start,
        xmax = end,
        ymin = 0.7,
        ymax = 1.3,
        fill = condition
      ),
      color = "black"
    ) +
    
    geom_text(
      aes(x = mid, y = 1.3, label = trials),
      vjust = -0.3,
      size = 3.5
    ) +
    
    # Arrow reminder marker (at end of first aligned phase)
    geom_vline(
      xintercept = aligned_end,
      linetype = "dashed",
      linewidth = 0.6
    ) +
    
    annotate(
      "text",
      x = aligned_end,
      y = 1.55,
      label = "Arrow\nReminder",
      size = 3
    ) +
    
    # ---- phase labels ----
  annotate(
    "text",
    x = aligned_mid,
    y = 0.45,
    label = "Aligned Phase",
    size = 4
  ) +
    
    annotate(
      "text",
      x = rotation_mid,
      y = 0.45,
      label = "Rotation Phase",
      size = 4
    ) +
    
    scale_fill_manual(values = my_colors) +
    
    coord_cartesian(ylim = c(0.35, 1.7)) +
    
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )
}