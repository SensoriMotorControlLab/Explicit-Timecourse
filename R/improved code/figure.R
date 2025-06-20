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
  
  grouped_strategy_data <- total_group_data %>%
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
      aim_deviation = list(pmin(ifelse(-8:50 < cutrial_no, 0, aimdeviation_deg), 60))
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
      "20" = "blue",     # orange
      "30" = "mediumpurple",    # purple
      "40" = "lightsalmon2",            # blue
      "50" = "plum2",           # pinkish-purple
      "60" = "skyblue"          # light blue
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


###fit step or curved to steps per participant
last_8_aligned <- strat_data[
  (strat_data$cutrial_no %in% 81:88 & strat_data$group == 'Group 1') |
    (strat_data$cutrial_no %in% 105:112 & strat_data$group == 'Group 2'), ]

# First 50 rotated trials after rotation
first_50_rotated <- strat_data[
  (strat_data$cutrial_no %in% 89:138 & strat_data$group == 'Group 1') |
    (strat_data$cutrial_no %in% 113:162 & strat_data$group == 'Group 2'), ]

combined_data <- rbind(last_8_aligned, first_50_rotated)
combined_data$trial_relative <- NA

combined_data$trial_relative[combined_data$group == "Group 1"] <- combined_data$cutrial_no[combined_data$group == "Group 1"] - 88
combined_data$trial_relative[combined_data$group == "Group 2"] <- combined_data$cutrial_no[combined_data$group == "Group 2"] - 112


fit_participant_models <- function(df) {
  df <- df[order(df$trial_relative), ]
  
  # Remove missing data
  df <- df %>% filter(!is.na(aimdeviation_deg), !is.na(trial_relative))
  pid <- unique(df$participant_id)
  
  if (nrow(df) < 5) {
    return(tibble(
      participant_id = pid,
      step_aic = NA_real_,
      exp_aic = NA_real_,
      two_step_aic = NA_real_,
      first_step_trial = NA_real_,
      best_model = NA_character_
    ))
  }
  
  above_thresh <- which(df$trial_relative > 0 & df$aimdeviation_deg > 10)
  
  if (length(above_thresh) == 0) {
    first_step_trial <- NA_real_
  } else {
    first_step_trial <- df$trial_relative[above_thresh[1]]
  }
  
  # 1-step model
  step_model <- function(par, trial, aimdev) {
    t_step <- par[1]
    height <- par[2]
    noise_sd <- par[3]
    pred <- ifelse(trial >= t_step, height, 0)
    -sum(dnorm(aimdev, mean = pred, sd = noise_sd, log = TRUE))
  }
  
  # exponential model
  exp_model <- function(par, trial, aimdev) {
    asymptote <- par[1]
    rate <- par[2]
    noise_sd <- par[3]
    pred <- asymptote * (1 - exp(-rate * trial))
    -sum(dnorm(aimdev, mean = pred, sd = noise_sd, log = TRUE))
  }
  
  # 2-step model
  two_step_model <- function(par, trial, aimdev) {
    t_step1 <- par[1]
    t_step2 <- par[2]
    mean2 <- par[3]
    mean3 <- par[4]
    noise_sd <- par[5]
    
    # Penalize if step times are not ordered or out of bounds
    if (t_step2 <= t_step1 || t_step1 < 0 || t_step2 < 0) return(1e6)
    
    pred <- ifelse(trial < t_step1, 0,
                   ifelse(trial < t_step2, mean2, mean3))
    
    -sum(dnorm(aimdev, mean = pred, sd = noise_sd, log = TRUE))
  }
  
  # Fit 1-step model
  step_fit <- tryCatch({
    optim(par = c(max(first_step_trial, 0), 20, 5),
          fn = step_model,
          trial = df$trial_relative,
          aimdev = df$aimdeviation_deg,
          method = "L-BFGS-B",
          lower = c(0, -180, 1e-2),
          upper = c(max(df$trial_relative), 180, 50))
  }, error = function(e) NULL)
  
  # Fit exponential model
  exp_fit <- tryCatch({
    optim(par = c(20, 0.1, 5),
          fn = exp_model,
          trial = df$trial_relative,
          aimdev = df$aimdeviation_deg,
          method = "L-BFGS-B",
          lower = c(-180, 1e-3, 1e-2),
          upper = c(180, 2, 50))
  }, error = function(e) NULL)
  
  # Fit 2-step model
  two_step_fit <- tryCatch({
    optim(par = c(max(first_step_trial, 0), max(first_step_trial, 0) + 5, 15, 30, 5), 
          fn = two_step_model,
          trial = df$trial_relative,
          aimdev = df$aimdeviation_deg,
          method = "L-BFGS-B",
          lower = c(0, 0, -180, -180, 1e-2),
          upper = c(max(df$trial_relative), max(df$trial_relative), 180, 180, 50))
  }, error = function(e) NULL)
  
  # Handle cases where fits fail
  if (is.null(step_fit) || is.null(exp_fit) || is.null(two_step_fit)) {
    return(tibble(
      participant_id = pid,
      step_aic = if(!is.null(step_fit)) 2*length(step_fit$par) + 2*step_fit$value else NA_real_,
      exp_aic = if(!is.null(exp_fit)) 2*length(exp_fit$par) + 2*exp_fit$value else NA_real_,
      two_step_aic = if(!is.null(two_step_fit)) 2*length(two_step_fit$par) + 2*two_step_fit$value else NA_real_,
      first_step_trial = first_step_trial,
      best_model = NA_character_
    ))
  }
  
  # Calculate AIC for all models
  step_aic <- 2 * length(step_fit$par) + 2 * step_fit$value
  exp_aic <- 2 * length(exp_fit$par) + 2 * exp_fit$value
  two_step_aic <- 2 * length(two_step_fit$par) + 2 * two_step_fit$value
  
  # Determine best model by lowest AIC
  aic_values <- c(step = step_aic, exp = exp_aic, two_step = two_step_aic)
  best_model <- names(which.min(aic_values))
  
  tibble(
    participant_id = pid,
    step_aic = step_aic,
    exp_aic = exp_aic,
    two_step_aic = two_step_aic,
    first_step_trial = first_step_trial,
    best_model = best_model
  )
}






plotProportion <- function () {
  
  plot_data_yes <- logit_data %>%
    group_by(rotation) %>%
    summarise(
      count_yes = sum(strategy == "Yes"),
      total = n(),
      proportion_yes = count_yes / total,
      .groups = "drop"
    )
  
  custom_colors <- c(
    "20" = "lightsalmon",  # orange
    "30" = "mediumpurple",  # sky blue
    "40" = "blue",  # green
    "50" = "plum2",  # yellow
    "60" = "skyblue"   # red
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
  
  
  
par(cex.axis = 1.5)

plot_step_histogram <- function(sim_data) {
  plot(NA,
       # main = 'Step-like Explicit Learning (60°)',
       xlab = '', ylab = '',
       xlim = c(-8, 32), ylim = c(-15, 65),
       ax = FALSE, bty = 'n',
       cex.lab = 1.5,     # Axis titles (xlab, ylab)
       cex.axis = 4,  # Axis numbers
       cex.main = 2.2)
  
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
  
  
  