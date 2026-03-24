library(dplyr)
library(ggplot2)
library(plotly)
library(zoo)
library(xgboost)

### -- Human classifier protocol -- ###

#1) mark learning onset
#2) mark learning stable/end
#3) discard single trials that are around 0-- they might not have wanted to deviate arrow

# strategy_data <- read.csv("data/strategy_only_participants.csv")
# random_ids_vec <- strategy_data %>%
#   distinct(participant_id) %>%
#   ungroup() %>%             
#   slice_sample(n = 75) %>%
#   pull(participant_id)
# 
# plot_rotated_phase_all_trials <- function(pid = "bd8518", data_file = "data/strategy_only_participants.csv") {
# 
#   strategy_data <- read.csv(data_file, stringsAsFactors = FALSE)
# 
#   strategy_data$trial_idx <- as.numeric(as.character(strategy_data$trial_idx))
#   strategy_data$aimdeviation_deg <- as.numeric(as.character(strategy_data$aimdeviation_deg))
# 
#   d <- strategy_data %>%
#     filter(participant_id == pid,
#            trial_type.x == "rotated")
# 
#   if (nrow(d) == 0) stop("No rotated phase data for this participant.")
# 
#   p <- ggplot(d, aes(
#     x = trial_idx,
#     y = aimdeviation_deg,
#     text = paste("Trial:", trial_idx, "<br>Aim:", round(aimdeviation_deg, 2))
#   )) +
#     geom_line(size = 1, alpha = 0.8, color = "steelblue") +  # line connecting trials
#     geom_point(size = 2, alpha = 0.9, color = "darkred") +    # points for each trial
#     scale_x_continuous(breaks = d$trial_idx) +               # force every trial on X-axis
#     labs(
#       title = paste("Rotated Phase - Participant", pid),
#       x = "Trial",
#       y = "Aim Deviation (deg)"
#     ) +
#     coord_cartesian(xlim = c(0, 120), ylim = c(-100, 100)) +
#     theme_minimal(base_size = 14)
# 
#   # 5️⃣ Make interactive
#   ggplotly(p, tooltip = "text")
# }
# 
# plot_rotated_phase_all_trials("bd8518")






### -- Learning Algorithm Protocol -- ###
# Learning = 1, Baseline = 0, anything in between is the learnind phase

# Model 1:  XGBoost Regressor
xgSetup <- function () {
annotations <- read.csv("~/Desktop//ElysaClassifier2.csv", stringsAsFactors = FALSE)
strategy_data <- read.csv("data/strategy_only_participants.csv")

strategy_data <- strategy_data %>% 
  filter(trial_type.x == "rotated")
strategy_data <- strategy_data %>% 
  filter(trial_type.x == "rotated", group == "Group 2")

annotated_pids <- annotations$pid  

strategy_annot <- strategy_data %>%
  filter(participant_id %in% annotated_pids) %>%
  left_join(annotations, by = c("participant_id" = "pid"))

final_stats <- strategy_annot %>%
  group_by(participant_id) %>%
  summarise(
    final_mean   = mean(tail(aimdeviation_deg, 16), na.rm = TRUE),
    final_median = median(tail(aimdeviation_deg, 16), na.rm = TRUE),
    .groups = "drop"
  )

strategy_annot <- strategy_annot %>%
  left_join(final_stats, by = "participant_id") %>%
  mutate(
    dist_final_mean   = abs(aimdeviation_deg - final_mean),
    dist_final_median = abs(aimdeviation_deg - final_median)
  )
return(strategy_annot)
}


# -----------------------------
# compute rolling features per participant
xgFeatures <- function () {
  strategy_annot <- xgSetup()

  strategy_annot <- strategy_annot %>%
  group_by(participant_id) %>%
  arrange(trial_idx, .by_group = TRUE) %>%
  mutate(
    roll_mean   = rollapply(aimdeviation_deg, width = 12, FUN = mean, fill = NA, align = "right", partial = TRUE),
    roll_sd     = rollapply(aimdeviation_deg, width = 12, FUN = sd, fill = NA, align = "right", partial = TRUE),
    roll_mad   = rollapply(aimdeviation_deg, width = 12, FUN = function(x) mad(x, constant = 1), fill = NA, align = "right")
  ) %>%
  ungroup()


# determine trial.start (first trial where abs deviation exceeds threshold)
start_sd_trials <- strategy_annot %>%
  group_by(participant_id) %>%
  filter(!is.na(roll_mean) & abs(roll_mean) > 5) %>%  
  slice(1) %>%
  ungroup() %>%
  select(participant_id, trial.start = trial_idx)

# -----------------------------
# determine trial.end using final SD stability
final_sd <- strategy_annot %>%
  group_by(participant_id) %>%
  summarise(
    final_sd = sd(tail(aimdeviation_deg, 16), na.rm = TRUE)
  )

delta <- 1.9  # tolerance for SD similarity
N     <- 4 # consecutive trials for stability

min_sd_trials <- strategy_annot %>%
  left_join(start_sd_trials, by = "participant_id") %>%
  left_join(final_sd, by = "participant_id") %>%
  group_by(participant_id) %>%
  filter(!is.na(roll_sd) & trial_idx >= trial.start) %>%
  mutate(
    stable = abs(roll_sd - final_sd) <= delta,
    stable_run = rollapply(stable, width = N, FUN = all, fill = NA, align = "left")
  ) %>%
  summarise(
    trial.end = case_when(
      # must be stable AND aims are positive at that window
      any(stable_run & roll_mean > 0, na.rm = TRUE) ~ 
        trial_idx[which(stable_run & roll_mean > 0)[1]],
      # fallback: closest SD match where aim is still positive
      any(roll_mean > 0, na.rm = TRUE) ~
        trial_idx[which.min(abs(roll_sd[roll_mean > 0] - final_sd[roll_mean > 0]))],
      # last resort: original behavior
      TRUE ~ trial_idx[which.min(abs(roll_sd - final_sd))]
    )
  ) %>%
  ungroup()

# -----------------------------
# Merge learning summary back into main dataframe
learning_summary <- start_sd_trials %>%
  left_join(min_sd_trials, by = "participant_id")

strategy_annot <- strategy_annot %>%
  left_join(learning_summary, by = "participant_id")

# -----------------------------
# Phase label (0 = baseline, 1 = learned, 0–1 = learning phase)
strategy_annot <- strategy_annot %>%
  group_by(participant_id) %>%
  mutate(
    phase_label = case_when(
      trial_idx < trial.start ~ 0,
      trial_idx >= trial.end ~ 1,
      TRUE ~ (trial_idx - trial.start) / (trial.end - trial.start)
    )
  ) %>%
  ungroup()
}

# -----------------------------
# participant-level feature aggregation for XGBoost

xgParticipant <- function() {
  strategy_annot <- xgFeatures()
  learning_summary <- xgFeatures()
  
participant_features <- strategy_annot %>%
  group_by(participant_id) %>%
  summarise(
    mean_dev         = mean(aimdeviation_deg, na.rm = TRUE),
    median_dev       = median(aimdeviation_deg, na.rm = TRUE),
    sd_dev           = sd(aimdeviation_deg, na.rm = TRUE),
    mean_roll_mean   = mean(roll_mean, na.rm = TRUE),
    sd_roll_mean     = sd(roll_mean, na.rm = TRUE),
    mean_roll_mad    = mean(roll_mad, na.rm = TRUE),
    .groups = "drop"
  )


  model_df <- participant_features %>%
   left_join(learning_summary, by = "participant_id")
}

# -----------------------------
# XGBoost model: trial.start

xgRun <- function () {
  model_df <- xgParticipant()
  
  features_start <- model_df %>% 
  select(mean_dev, sd_dev, mean_roll_mean, sd_roll_mean, median_dev, mean_roll_mad)

  label_start <- model_df$trial.start

  dtrain_start <- xgb.DMatrix(
    data = as.matrix(features_start),
    label = label_start
  )

  params <- list(
   objective = "reg:squarederror",
   max_depth = 4,
   eta = 0.1
  )

  model_start <- xgb.train(
    params = params,
    data = dtrain_start,
    nrounds = 500
  )

  model_df$pred_start <- predict(model_start, as.matrix(features_start))

# -----------------------------
# XGBoost model: trial.end
  features_end <- features_start  
  label_end <- model_df$trial.end

  dtrain_end <- xgb.DMatrix(
   data = as.matrix(features_end),
    label = label_end
  )

  model_end <- xgb.train(
   params = params,
   data = dtrain_end,
   nrounds = 500
  )

  model_df$pred_end <- predict(model_end, as.matrix(features_end))
  model_df$pred_end <- pmax(model_df$pred_end, model_df$pred_start + 3)

# -----------------------------
# Compare predicted vs actual
model_df <- model_df %>%
  group_by(participant_id) %>%
  summarise(
    across(everything(), first),
    .groups = "drop"
  )

  model_df <- model_df %>%
    group_by(participant_id) %>%
    summarise(
      across(everything(), first),
      .groups = "drop"
    )
  
return(model_df)
}

# random forest regression 
# take trial - end value (from 16 trials)- how much you deviate


plotStart <- function(target = "inline", main = NULL) {
  
  setupFigureFile(
    target = target,
    width = 3,
    height = 3,
    dpi = 300,
    sprintf("images/plotsteps.%s", target)
  )
  annotations <- xgSetup()
  model_df <- xgRun()
  correlation <- cor(model_df$starttrial, model_df$pred_start, use = "complete.obs")

  ggplot(model_df, aes(x = starttrial, y = pred_start, label = participant_id)) +
  geom_point(size = 3, alpha = 0.75, colour="#ff713d") +
  geom_abline(intercept = 0, slope = 1, 
              color = "grey", linetype = "solid", linewidth = 1) +
  geom_abline(intercept = 0, slope = correlation, 
              color = "#ff713d", linetype = "dashed", linewidth = 1) +
  labs(
    title = "",
    x = "Human Classifier Prediction (Trial)",
    y = "xgBoost Model Prediction (Trial)"
  ) +
  coord_cartesian(xlim = c(0, 125), ylim=c(0,125)) +
  annotate(
    "text", 
    x = max(model_df$starttrial)*0.2, 
    y = max(model_df$pred_start)*1.08, 
    label = correlation, 
    size = 7, 
    color = "black"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(),
    axis.text.x  = element_text(size = 21),
    axis.text.y  = element_text(size = 21),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17),
    legend.title = element_text(size = 17),
    legend.text  = element_text(size = 16),
    plot.title   = element_text(size = 19, hjust = 0),
    legend.position = "inside",
    legend.position.inside = c(0.08, 0.5)
  )


}

plotEnd <- function(target = "inline", main = NULL) {
    
    setupFigureFile(
      target = target,
      width = 6,
      height = 3,
      dpi = 300,
      sprintf("images/plotsteps.%s", target)
    )
  annotations <- xgSetup()
  model_df <- xgRun()
  correlation <- cor(model_df$endtrial, model_df$pred_end)
  
ggplot(model_df, aes(x = endtrial, y = pred_end, label = participant_id)) +
  geom_point(size = 3, alpha = 0.75, color="#5C1675") +
  geom_abline(intercept = 0, slope = 1, 
              color = "grey", linetype = "solid", linewidth = 1) +
  geom_abline(intercept = 0, slope = correlation, 
              color = "#5C1675", linetype = "dashed", linewidth = 1) +
  labs(
    title = "",
    x = "Human Classifier Prediction (Trial)",
    y = "xgBoost Model Prediction (Trial)"
  ) +
  coord_cartesian(xlim = c(0, 125), ylim=c(0,125)) +
  annotate(
    "text",
    x = 0.1 * 127,
    y = 0.9 * 100,
    label = correlation,
    size =4,
    color = "black"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(),
    axis.text.x  = element_text(size = 21),
    axis.text.y  = element_text(size = 21),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17),
    legend.title = element_text(size = 17),
    legend.text  = element_text(size = 16),
    plot.title   = element_text(size = 19, hjust = 0),
    legend.position = "inside",
    legend.position.inside = c(0.08, 0.5)
  )


}

