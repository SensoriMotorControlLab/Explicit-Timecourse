#plot model winners

models <- results_joined

oneSt <- models %>%
  filter(best_model == "one-step")


oneSt_data <- total_learners_data %>%
  filter(participant_id %in% oneSt$participant)


aligned_oneSt <- aligned %>%
  filter(participant_id %in% oneSt$participant)


first_steps <- aligned_oneSt %>%
  filter(trial_type == "rotated") %>%
  group_by(participant_id) %>%
  filter(aimdeviation_deg > 7) %>%
  slice_head(n = 1) %>%
  ungroup()


baseline <- aligned_oneSt %>%
  filter(trial_rel == 0,
         participant_id %in% first_steps$participant_id) %>%
  select(participant_id, baseline_value = aimdeviation_deg)


steps <- first_steps %>%
  select(participant_id,
         step_trial = trial_rel,
         step_value = aimdeviation_deg) %>%
  left_join(baseline, by = "participant_id")

step_segments <- steps %>%
  mutate(plateau_end = step_trial + 25)

step_segments <- steps %>%
  mutate(
    plateau_end = step_trial + 25
  )

mean_traj <- aligned_oneSt %>%
  group_by(trial_rel) %>%
  summarise(
    mean_aim = mean(aimdeviation_deg, na.rm = TRUE),
    sd = sd(aimdeviation_deg, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci_lower = mean_aim - 1.96 * se,
    ci_upper = mean_aim + 1.96 * se,
    .groups = "drop"
  )



ggplot() +
  geom_ribbon(
    data = mean_traj %>% filter(trial_rel >= -10, trial_rel <= 85),
    aes(x = trial_rel, ymin = ci_lower, ymax = ci_upper),
    fill = "lightblue", alpha = 0.3
  ) +
  geom_line(
    data = mean_traj %>% filter(trial_rel >= -8, trial_rel <= 85),
    aes(x = trial_rel, y = mean_aim),
    linewidth = 1.2, color="cadetblue"
  ) +
  
  geom_segment(
    data = step_segments,
    aes(x = 0, xend = step_trial,
        y = baseline_value, yend = baseline_value),
    color = "salmon", linewidth = 0.6, alpha = 0.35
  ) +
  
  geom_segment(
    data = step_segments,
    aes(x = step_trial, xend = step_trial,
        y = baseline_value, yend = step_value),
    color = "salmon", linewidth = 0.6, alpha = 0.35
  ) +
  
  geom_segment(
    data = step_segments,
    aes(x = step_trial, xend = plateau_end,
        y = step_value, yend = step_value),
    color = "salmon", linewidth = 0.6, alpha = 0.35
  ) +
  
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  
  labs(
    title = "",
    x = "Trials (0 = Rotation Onset)",
    y = "Aim Deviation (deg)"
  ) +
  coord_cartesian(ylim = c(-10, 65)) + 
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),     
  )


##########

aligned <- total_learners_data %>%
  filter(trial_type %in% c("aligned", "rotated")) %>%
  group_by(participant_id, rotation) %>%
  mutate(
    rotation_onset = min(cutrial_no[trial_type == "rotated"]),
    trial_rel = cutrial_no - rotation_onset
  ) %>%
  ungroup()


twoSt <- models %>%
  filter(best_model == "two-step") %>%
  filter(participant %in% aligned$participant_id) 


aligned_twoSt <- aligned %>%
  filter(participant_id %in% twoSt$participant)


two_steps <- aligned_twoSt %>%
  filter(trial_type == "rotated") %>%
  group_by(participant_id) %>%
  arrange(trial_rel) %>%
  summarise(
    step1_row = which(aimdeviation_deg > 7)[1],
    step1_trial = trial_rel[step1_row],
    step1_value = aimdeviation_deg[step1_row],
    step2_row = which(aimdeviation_deg > 30 & trial_rel > step1_trial)[1],
    step2_trial = trial_rel[step2_row],
    step2_value = aimdeviation_deg[step2_row],
    .groups = "drop"
  ) %>%
  filter(!is.na(step1_trial) & !is.na(step2_trial))

baseline <- aligned_twoSt %>%
  filter(trial_rel == 0,
         participant_id %in% two_steps$participant_id) %>%
  select(participant_id, baseline_value = aimdeviation_deg)



step_segments <- two_steps %>%
  left_join(baseline, by = "participant_id") %>%
  mutate(
    plateau1_end = step1_trial + 10,
    plateau2_end = step2_trial + 25
  )

meantwo_traj <- aligned_twoSt %>%
  filter(participant_id %in% two_steps$participant_id) %>%
  group_by(trial_rel) %>%
  summarise(
    mean_aim = mean(aimdeviation_deg, na.rm = TRUE),
    sd = sd(aimdeviation_deg, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci_lower = mean_aim - 1.96 * se,
    ci_upper = mean_aim + 1.96 * se,
    .groups = "drop"
  )

ggplot() +
  geom_ribbon(
    data = meantwo_traj %>% filter(trial_rel >= -10, trial_rel <= 110),
    aes(x = trial_rel, ymin = ci_lower, ymax = ci_upper),
    fill = "lightblue", alpha = 0.3
  ) +

  geom_line(
    data = meantwo_traj %>% filter(trial_rel >= -8, trial_rel <= 110),
    aes(x = trial_rel, y = mean_aim),
    linewidth = 1.2, color="cadetblue"
  ) +
  # Step 1 segments
  geom_segment(
    data = step_segments,
    aes(x = 0, xend = step1_trial, y = baseline_value, yend = baseline_value),
    color = "salmon", linewidth = 0.6, alpha = 0.35
  ) +
  geom_segment(
    data = step_segments,
    aes(x = step1_trial, xend = step1_trial, y = baseline_value, yend = step1_value),
    color = "salmon", linewidth = 0.6, alpha = 0.35
  ) +
  geom_segment(
    data = step_segments,
    aes(x = step1_trial, xend = plateau1_end, y = step1_value, yend = step1_value),
    color = "salmon", linewidth = 0.6, alpha = 0.35
  ) +
  # Step 2 segments
  geom_segment(
    data = step_segments,
    aes(x = step1_trial, xend = step2_trial, y = step1_value, yend = step1_value),
    color = "salmon", linewidth = 0.6, alpha = 0.35
  ) +
  geom_segment(
    data = step_segments,
    aes(x = step2_trial, xend = step2_trial, y = step1_value, yend = step2_value),
    color = "salmon", linewidth = 0.6, alpha = 0.35
  ) +
  geom_segment(
    data = step_segments,
    aes(x = step2_trial, xend = plateau2_end, y = step2_value, yend = step2_value),
    color = "salmon", linewidth = 0.6, alpha = 0.35
  ) +

  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  labs(
    title = "Two-Step Participants: Mean Aim Trajectory with Steps",
    x = "Trials (0 = Rotation Onset)",
    y = "Aim Deviation (deg)"
  ) +
  coord_cartesian(ylim = c(-10, 65)) + 
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())
        



#########

exp <- models %>%
  filter(best_model=="exponential")

baseline_exp <- aligned_exp %>%
  group_by(participant_id) %>%
  filter(trial_type == "aligned") %>%
  slice_tail(n = 10) %>%  # last 10 baseline trials
  mutate(trial_rel = row_number() - 11)  # gives -10 ... -1

aligned_exp <- aligned %>%
  filter(participant_id %in% exp$participant,
         trial_type %in% c("aligned", "rotated")) %>%
  group_by(participant_id) %>%
  mutate(trial_rel = cutrial_no - min(cutrial_no[trial_type == "rotated"])) %>%
  ungroup() %>%
  filter(trial_rel >= -10, trial_rel <= 85)   # restrict to window

# Participants to include
exp_participants <- exp$participant

# Get aligned trials for baseline (last 10 pre-rotation)
baseline_exp <- aligned %>%
  filter(participant_id %in% exp_participants, trial_type == "aligned") %>%
  group_by(participant_id) %>%
  slice_tail(n = 10) %>%
  mutate(trial_rel = row_number() - 11) %>%   # -10 .. -1
  ungroup()

# Get rotated trials relative to rotation onset
rotated_exp <- aligned %>%
  filter(participant_id %in% exp_participants, trial_type == "rotated") %>%
  group_by(participant_id) %>%
  mutate(trial_rel = cutrial_no - min(cutrial_no)) %>%  # rotation onset = 0
  ungroup()

# Combine baseline and rotated
aligned_exp <- bind_rows(baseline_exp, rotated_exp) %>%
  filter(trial_rel >= -10, trial_rel <= 85)   # restrict plotting window

# Mean trajectory
mean_exp_traj <- aligned_exp %>%
  group_by(trial_rel) %>%
  summarise(
    mean_aim = mean(aimdeviation_deg, na.rm = TRUE),
    sd = sd(aimdeviation_deg, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci_lower = mean_aim - 1.96 * se,
    ci_upper = mean_aim + 1.96 * se,
    .groups = "drop"
  )

# exponential per participant 
# y = baseline + amplitude * (1 - exp(-rate * t))
 exp_fits <- aligned_exp %>%
   group_by(participant_id) %>%
   group_modify(~{
     df <- .
     fit <- try(
       nlsLM(
         aimdeviation_deg ~ baseline + amplitude * (1 - exp(-rate * trial_rel)),
         data = df,
         start = list(baseline = 0, amplitude = 20, rate = 0.1),
         control = nls.lm.control(maxiter = 500)
       ),
       silent = TRUE
     )
     if(inherits(fit, "try-error")) return(tibble())
     
     t <- seq(min(df$trial_rel), max(df$trial_rel), 1)
     coef_fit <- coef(fit)
     tibble(trial_rel = t,
            predicted = coef_fit["baseline"] + coef_fit["amplitude"] * (1 - exp(-coef_fit["rate"] * t)))
   }) %>%
   ungroup()
 

ggplot() +
  geom_ribbon(
    data = mean_exp_traj %>% filter(trial_rel >= -10, trial_rel <= 85),
    aes(x = trial_rel, ymin = ci_lower, ymax = ci_upper),
    fill = "lightblue", alpha = 0.3
  ) +

  geom_line(
    data = mean_exp_traj %>% filter(trial_rel >= -10, trial_rel <= 85),
    aes(x = trial_rel, y = mean_aim),
    linewidth = 1.2, color = "cadetblue"
  ) +

  geom_line(
    data = exp_fits,
    aes(x = trial_rel, y = predicted, group = participant_id),
    color = "salmon", alpha = 0.3, linewidth = 1
  ) +
  geom_hline(yintercept = 0, linetype = "solid", linewidth = 0.4, alpha = 0, color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5,  color = "black") +
  labs(
    title = "Exponential Participants: Mean Aim Trajectory with Per-Participant Fits",
    x = "Trials (0 = Rotation Onset)",
    y = "Aim Deviation (deg)"
  ) +
  coord_cartesian(ylim = c(-10, 65)) + 
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())


###hmmm linear lines??
aligned_exp <- aligned %>%
  filter(participant_id %in% exp$participant, trial_type == "rotated")

exp_fits <- aligned_exp %>%
  group_by(participant_id) %>%
  group_modify(~{
    df <- .
    fit <- try(
      nlsLM(
        aimdeviation_deg ~ baseline + amplitude*(1 - exp(-rate*trial_rel)),
        data = df,
        start = list(baseline = 0, amplitude = 20, rate = 0.1),
        control = nls.lm.control(maxiter = 500)
      ),
      silent = TRUE
    )
    
    if(inherits(fit, "try-error")) return(tibble())  # skip failed fits
    
    t <- seq(min(df$trial_rel), max(df$trial_rel), 1)
    coef_fit <- coef(fit)
    tibble(trial_rel = t,
           predicted = coef_fit["baseline"] + coef_fit["amplitude"] * (1 - exp(-coef_fit["rate"]*t)),
           participant_id = unique(df$participant_id))
  }) %>%
  ungroup()


participant_labels <- tibble(
  participant_id = unique(aligned_exp$participant_id),
  label = paste("Participant", seq_along(unique(aligned_exp$participant_id)))
)

aligned_exp <- aligned_exp %>%
  left_join(participant_labels, by = "participant_id")

exp_fits <- exp_fits %>%
  left_join(participant_labels, by = "participant_id")


ggplot() +
  geom_line(data = aligned_exp, aes(x = trial_rel, y = aimdeviation_deg), color = "gray50", alpha = 0.5) +
  geom_line(data = exp_fits, aes(x = trial_rel, y = predicted), color = "firebrick", linewidth = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Exponential winner model Fits",
    x = "Trial Number",
    y = "Aim Deviation (deg)"
  ) +
  facet_wrap(~label, scales = "free_y") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())





##chi sq

tbl <- table(models$best_model, models$rotation)
tbl
chisq.test(tbl)

