total <- read.csv('data/total_learners_data.csv')

fitExponential <- function() {
  
  total <- read.csv("data/total_learners_data.csv")
  strategy_df <- getStrategies()
  
  reach_rot <- total %>%
    left_join(
      strategy_df %>% select(participant_id, rotation, strategy),
      by = c("participant_id", "rotation")
    ) %>%
    filter(trial_type == "rotated") %>%
    group_by(participant_id, rotation, strategy) %>%
    arrange(trial_idx, .by_group = TRUE)
  
  exp_fits <- reach_rot %>%
    group_modify(~ {
      cat(
        "Fitting participant:", .y$participant_id,
        "| Rotation:", .y$rotation,
        "| Strategy:", .y$strategy, "\n"
      )
      flush.console()
      
      sig <- .x$reachdeviation_deg
      params <- Reach::exponentialFit(signal = sig)
      as.data.frame(as.list(params))
    }) %>%
    ungroup()
  
  return(exp_fits)
}

exp_fits <- fitExponential()

plotCurves <- function() {
  exp_fits <- fitExponential()
  max_trials <- max(reach_rot$trial_idx) - min(reach_rot$trial_idx)
  timepoints <- seq(0, max_trials, by = 0.5)

   fitted_curves <- exp_fits %>%
   group_by(participant_id, rotation, strategy) %>%
    reframe(
    time = timepoints,
    predicted_reach = {

      p_par <- c(lambda = lambda, N0 = N0)
      
      expfit <- Reach::exponentialModel(par = p_par, timepoints = timepoints)
      

      if (is.data.frame(expfit) || is.list(expfit)) {
        if ("signal" %in% names(expfit)) expfit$signal
        else if ("y" %in% names(expfit)) expfit$y
        else expfit[[2]]  
      } else {
        expfit
      }
      }
    )


  fitted_curves <- fitted_curves %>% filter(!is.na(strategy))

   group_curves_rot <- fitted_curves %>%
   group_by(strategy, rotation, time) %>%
   summarise(
    mean_reach = mean(predicted_reach/rotation, na.rm = TRUE),
    se_reach   = sd(predicted_reach/rotation, na.rm = TRUE) / sqrt(n()),
    .groups    = "drop"
  )


  ggplot(group_curves_rot, aes(x = time, y = mean_reach, color = strategy, fill = strategy)) +
  geom_ribbon(aes(ymin = mean_reach - se_reach, ymax = mean_reach + se_reach), 
              alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ paste0(rotation, "° Rotation"), scales = "fixed") +
  labs(
    title = "",
    x = "Trial",
    y = "Fitted Reach Deviation (deg)",
    color = "Strategy User",
    fill = "Strategy User"
  ) +
  scale_color_manual(values = c("No" = "#E41A1C", "Yes" = "#377EB8")) +
  scale_fill_manual(values = c("No" = "#E41A1C", "Yes" = "#377EB8")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  )
}


##stats on lambda and no for reaches
curveStats <- function() {
  
  reach_summary <- exp_fits %>%
    filter(strategy == "Yes", !is.na(N0), !is.na(lambda)) %>%
    mutate(
      rotation_num = as.numeric(as.character(rotation)),
      rotation = factor(rotation),
      normN0 = N0 / rotation_num
    )
  
  aov_N0 <- aov(normN0 ~ rotation, data = reach_summary)
  aov_lambda <- aov(lambda ~ rotation, data = reach_summary)
  
  cat("\n--- Normalized N0 by Rotation ---\n")
  print(summary(aov_N0))
  
  cat("\n--- Lambda by Rotation ---\n")
  print(summary(aov_lambda))
  
  invisible(list(
    data = reach_summary,
    N0_model = aov_N0,
    lambda_model = aov_lambda
  ))
}


##rotation & percentage of adaptation (aim and reach)





#CORRELATIONS




magnitudeCorr <- function () {
#get reach asymptote from fit
  reach_asymptote <- exp_fits %>%
  filter(strategy == "Yes") %>%
  mutate(
    rotation_num = as.numeric(as.character(rotation)),
    rotation = factor(rotation)
  ) %>%
  select(
    participant_id,
    rotation,
    rotation_num,
    reach_N0 = N0,
    reach_lambda = lambda
  )


#get magntiude of final aim
  aim_asymptote <- reach_rot %>%
  filter(strategy == "Yes") %>%
  group_by(participant_id, rotation) %>%
  slice_tail(n = 8) %>%
  summarise(
    final_aim_deg = mean(aimdeviation_deg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    rotation_num = as.numeric(as.character(rotation))
  )


asymptote_df <- reach_asymptote %>%
  left_join(
    aim_asymptote %>%
      select(participant_id, rotation_num, final_aim_deg),
    by = c("participant_id", "rotation_num")
  ) %>%
  mutate(
    reach_norm = reach_N0 / rotation_num,
    aim_norm   = final_aim_deg / rotation_num
  )


#normalized asymptotes
# find a shared range for both normalized variables
axis_min <- min(asymptote_df$aim_norm,
                asymptote_df$reach_norm,
                na.rm = TRUE)

axis_max <- max(asymptote_df$aim_norm,
                asymptote_df$reach_norm,
                na.rm = TRUE)

  p <- ggplot(
    asymptote_df,
    aes(x = aim_norm, y = reach_norm)
  ) +
   geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = "#6554C0",
    fill = "#B8B0E8",
    alpha = 0.28,
    linewidth = 1.1
  ) +
  geom_point(
    color = "orchid",
    alpha = 0.8,
    size = 2.8
  ) +
  scale_x_continuous(limits = c(axis_min, axis_max)) +
  scale_y_continuous(limits = c(axis_min, axis_max)) +
  labs(
    x = "Explicit asymptote / rotation",
    y = "Adaptation asymptote / rotation"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.title = element_text(size = 13, color = "black"),
    axis.text = element_text(size = 11, color = "black"),
    axis.line = element_line(linewidth = 0.6, color = "black"),
    axis.ticks = element_line(linewidth = 0.5, color = "black")
  )

  print(p)
#run correlation test
  cor.test(
  asymptote_df$aim_norm,
  asymptote_df$reach_norm,
  use = "complete.obs",
  method = "pearson"
  )
}

# does speed of adaptation correlate to speed of strategy stabilization?

speedCor <- function () {
  feature_results <- getFeatures()
  features <- feature_results$features



  rate_df <- reach_asymptote %>%
  left_join(explicit_speed, by = "participant_id") %>%
  left_join(
    features %>%
      select(participant_id, onset_trial, stable_trial),
    by = "participant_id"
  ) %>%
  mutate(rotation = factor(rotation))

  #first double check if rotation affects non-normalized reach lambda
  aov_rate <- aov(reach_lambda ~ rotation, data = rate_df)
  summary(aov_rate)
    #reach lamda not effected

  # see if stable trial is affected by rot size
  rate_df <- rate_df %>%
  mutate(rotation = factor(rotation))

  aov_rate <- aov(stable_trial.x ~ rotation, data = rate_df)
  summary(aov_rate)
    #stable trial not affected

  #plot
 p <- ggplot(
   rate_df,
   aes(x = stable_trial.x, y = reach_lambda)
   ) +
   geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = "#6554C0",
    fill = "#B8B0E8",
    alpha = 0.28,
    linewidth = 1.1
   ) +
   geom_point(
    color = "orchid",
    alpha = 0.8,
    size = 2.8
   ) +
    coord_cartesian(
    xlim = c(0, 115),
    ylim = c(0, 1)
    ) +
  labs(
    x = "Strategy stabilization trial",
    y = expression("Reach adaptation rate (" * lambda * ")")
   ) +
   theme_classic(base_size = 13) +
   theme(
    axis.title = element_text(size = 13, color = "black"),
    axis.text = element_text(size = 11, color = "black"),
    axis.line = element_line(linewidth = 0.6, color = "black"),
    axis.ticks = element_line(linewidth = 0.5, color = "black")
   )
  
 print(p)
 
#run correlation test
  cor.test(
  rate_df$stable_trial.x,
  rate_df$reach_lambda,
  use = "complete.obs",
  method = "pearson"
  )
}

