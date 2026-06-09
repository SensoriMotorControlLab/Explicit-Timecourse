getFeatures <- function() {
  
  model_df <- read.csv("data/LearningClassifier.csv") #pre xG driven timepoints are here too!
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  
  numeric_cols <- c(
    "learning_sd",
    "learning_length",
    "learning_abs_diff",
    "num_sign_flips_prop",
    "jump_ratio",
    "largest_jump_frac",
    "lin_r2",
    "smoothness"
  )
  
  features <- data.frame(
    participant_id = character(),
    learning_sd = numeric(),
    learning_length = numeric(),
    learning_abs_diff = numeric(),
    num_sign_flips_prop = numeric(),
    jump_ratio = numeric(),
    largest_jump_frac = numeric(),
    lin_r2 = numeric(),
    smoothness = numeric(),
    # onset_trial = numeric(),
    # stable_trial = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(nrow(model_df))) {
    
    id <- model_df$participant_id[i]
    onset <- model_df$trial.start[i]
    stable <- model_df$trial.end[i]
    
    ##some participants have immediate step, but I cannot find sd from less than 3 values 
    if ((stable - onset) < 3) {
      stable <- onset + 3
    }
    
    if (is.na(onset) || is.na(stable)) next
    
    d_all <- subset(strategy_data, participant_id == id)
    d_rot <- subset(strategy_data,
                    participant_id == id &
                      trial_type == "rotated")
    
    if (nrow(d_rot) < 2 || nrow(d_all) < 2) next
    
    aim <- d_rot$aimdeviation_deg
    trials <- d_rot$trial_idx
    
    # baseline (fixed trial window)
    baseline_aim <- (
      d_all$aimdeviation_deg[
        d_all$cutrial_no >= 105 &
          d_all$cutrial_no <= 112
      ]
    ) 
    # / rotation
    
    if (length(baseline_aim) < 2) next
    
    baseline_mean <- mean(baseline_aim, na.rm = TRUE)
    
    # learning window
    learning_idx <- which(trials >= onset & trials <= stable)
    learning_aim <- aim[learning_idx]
    
    if (length(learning_aim) < 2) next
    
    # align metadata
    onset_trial <- onset
    stable_trial <- stable
    
    # ===== FEATURES =====
    
    learning_centered <- learning_aim - baseline_mean
    
    learning_sd <- sd(learning_centered)
    learning_length <- length(learning_aim)
    
    diffs <- diff(learning_aim)
    
    learning_abs_diff <- mean(abs(diffs))
    
    num_sign_flips <- sum(diff(sign(diffs)) != 0)
    num_sign_flips_prop <- num_sign_flips / learning_length
    
    # jump features
    max_jump <- max(abs(diffs))
    mean_jump <- mean(abs(diffs))
    
    jump_ratio <- max_jump / (mean_jump + 1e-6)
    
    largest_jump_frac <- max(abs(diffs)) /
      (sum(abs(diffs)) + 1e-6)
    
    smoothness <- sum(abs(diffs)) /
      (max(abs(diffs)) * length(diffs) + 1e-6)
    
    # linear trend
    t <- seq_along(learning_aim)
    lin_r2 <- summary(lm(learning_aim ~ t))$r.squared
    
    # NA safety
    learning_sd <- ifelse(is.na(learning_sd), 0, learning_sd)
    learning_abs_diff <- ifelse(is.na(learning_abs_diff), 0, learning_abs_diff)
    num_sign_flips <- ifelse(is.na(num_sign_flips), 0, num_sign_flips)
    
    ## add amount (magnitude) of stabilization at end how much does it stabilize is key 
    
    
    
    features <- rbind(
      features,
      data.frame(
        participant_id = id,
        learning_sd = learning_sd,
        learning_abs_diff = learning_abs_diff,
        learning_length = learning_length,
        num_sign_flips_prop = num_sign_flips_prop,
        jump_ratio = jump_ratio,
        largest_jump_frac = largest_jump_frac,
        lin_r2 = lin_r2,
        smoothness = smoothness,
        # onset_trial = onset_trial,
        # stable_trial = stable_trial,
        stringsAsFactors = FALSE
      )
    )
  }
  
  features_z <- features %>%
    mutate(across(all_of(numeric_cols), ~ as.numeric(scale(.)))) %>%
    ungroup()
  
  return(list(
    features_z = features_z,
    kmeans_input = features_z[, numeric_cols]
  ))
  
 
}



testFeatures <- function () {
  pca <- prcomp(k_input, center = TRUE, scale. = FALSE)
  var_explained <- (pca$sdev^2) / sum(pca$sdev^2)
  abs_loadings <- abs(pca$rotation)
  
  # weighted contribution per feature per PC
  weighted <- abs_loadings %*% var_explained
  
  feature_importance <- data.frame(
    feature = rownames(pca$rotation),
    importance = as.numeric(weighted)
  )
  
  feature_importance <- feature_importance[order(-feature_importance$importance), ]
  
}

feature_table <- data.frame(
  Feature = c(
    "Aiming Variability",
    "Aiming Phase Length",
    "Absolute Trial Differences",
    "Directional Sign Flips",
    "Abruptness index",
    "Largest Step",
    "Linearity of Adaptation",
    "Smoothness"
  ),
  Description = c(
    "Baseline-corrected SD of aiming deviation",
    "Number of learning trials",
    "Mean absolute trial-to-trial change",
    "Proportion of directional sign changes",
    "Max jump / mean jump magnitude",
    "Largest jump contribution",
    "Linear trend R²",
    "Trajectory smoothness index"
  ),
  Interpretation = c(
    "Variability of aiming behavior",
    "Time required to reach a stable strategy",
    "Magnitude of behavioral adjustments",
    "Instability in movement direction",
    "Presence of abrupt strategy transitions",
    "Dominance of a single large adjustment",
    "Degree of linearity in adaptation trajectory",
    "Smoothness versus abruptness of trajectory"
  )
)

library(gt)

feature_table %>%
  gt() %>%
  
  tab_header(
    title = "Summary of Extracted Learning Features"
  ) %>%
  
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  
  tab_options(
    table.border.top.style = "solid",
    table.border.top.width = px(0.8),
    
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(0.8),
    
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.width = px(0.6),
    
    data_row.padding = px(4),
    table.font.size = px(11)
  ) %>%
  
  cols_align(
    align = "left",
    columns = everything()
  )


plotScree <- function () {
  extract <- getFeaturesFromModel() 
  features_df <- extract$features_df        
  k_input     <- extract$kmeans_input  
  k_input <- scale(k_input)
  # k_input <- dfn[, vars] 
  
  pca <- prcomp(k_input, center = TRUE, scale. = FALSE)
  
  cum_var <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
  
  # scree-style plot
  plot(cum_var,
       type = "b",
       pch = 19,
       col = "black",
       xlab = "Principal Components",
       ylab = "Cumulative Variance Explained",
       main = "",
       bty = "n",
       xaxs = "i",            
       xlim = c(0.8, 8.1),
       ylim = c(0,1)
  )
  
  axis(1, at = 1:8)
  
  abline(h = 0.80, col = "grey", lty = 2, lwd = 1)
  
  points(1:3, cum_var[1:3], col = "#f52f57", pch = 19)
  
  # var_each <- pca$sdev^2
  # screeplot(pca, type="lines")
  # points(var_each, col="red")
}