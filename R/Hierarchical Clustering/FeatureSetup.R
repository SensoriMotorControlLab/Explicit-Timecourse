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
    "Largest Step Index",
    "Linearity of Adaptation",
    "Smoothness Index"
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
feature_table |> 
  gt() |> 
  tab_header(
    title = md("**Feature Summary**"),
    subtitle = ""
  ) |> 
  cols_label(
    Feature = md("**Feature Name**"),
    Description = md("**Description / Calculation**"),
    Interpretation = md("**Behavioral Interpretation**")
  ) |> 
  tab_options(
    table.width = pct(100),
    column_labels.background.color = "#f4f4f4",
    table.font.size = "14px"
  )


plotScree <- function () {
  model_df <- read.csv("data/LearningClassifier.csv") #for pre-xG boost labels
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  extract <- getFeatures() 
  features_z <- extract$features_z
  
  k_input <- extract$kmeans_input
  
  pca <- prcomp(k_input, center = FALSE, scale. = FALSE)
  
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



##now determine whether features differ with rotation

kruskalFeatures <- function () {
featuresFile <- read.csv("data/StandardizedFeatures.csv")
strategy_data <- read.csv("data/strategy_only_participants.csv")

strategy_unique <- strategy_data %>%
  group_by(participant_id) %>%
  summarize(rotation = first(rotation), .groups = "drop")

df_merged <- featuresFile %>%
  left_join(strategy_unique, by = "participant_id")


feature_names <- c(
  "learning_sd",
  "learning_length",
  "learning_abs_diff",
  "num_sign_flips_prop",
  "jump_ratio",
  "largest_jump_frac",
  "lin_r2",
  "smoothness"
)

kw_results <- data.frame()

for(f in feature_names){
  
  form <- as.formula(
    paste(f, "~ factor(rotation)")
  )
  
  test <- kruskal.test(form, data = df_merged)
  
  kw_results <- rbind(
    kw_results,
    data.frame(
      feature = f,
      H = unname(test$statistic),
      p = test$p.value
    )
  )
}

return(list(
  data = df_merged,
  stats = kw_results
))
}



## plot box plots per rotation ##
plotBox <- function() {
  
  out <- kruskalFeatures()
  df_merged <- out$data
  
  
  rot_cols <- c(
    "20" = "#4cc9f0",
    "30" = "#4895ef",
    "40" = "#4261ee",
    "50" = "#2835af",
    "60" = "#12086f"
  )
  
  ggplot(df_merged,
         aes(x = factor(rotation),
             y = smoothness,
             fill = factor(rotation))) +
    
    geom_boxplot(
      alpha = 0.75,
      outlier.shape = NA,
      width = 0.6,
      color = "grey25"
    ) +
    
    geom_jitter(
      width = 0.12,
      alpha = 0.35,
      size = 1,
      color = "black"
    ) +
    coord_cartesian(ylim = c(-2, 5)) +
    scale_y_continuous(breaks = c(-2, 0, 2, 4)) +
    scale_fill_manual(values = rot_cols) +
    
    labs(
      x = "Rotation magnitude (°)",
      y = "Smoothness Index"
    ) +
    
    theme_classic(base_size = 12) +
    
    theme(
      legend.position = "none",
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black")
    ) +
    annotate("text",
             x = 3, y = 5,
             label = "",
             size = 8)
}


loadingTable <- function () {
  
  model_df <- read.csv("data/LearningClassifier.csv") #for pre-xG boost labels
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  extract <- getFeatures() 
  features_z <- extract$features_z
  
  k_input <- extract$kmeans_input
  
  pca <- prcomp(k_input, center = FALSE, scale. = FALSE)
  pca$rotation <- pca$rotation*-1
  pca$x<- pca$x*-1
  
  
  features <- c(
    "Aiming Variability",
    "Aiming Phase Length",
    "Absolute Trial Differences",
    "Directional Sign Flips",
    "Abruptness Index",
    "Largest Step Index",
    "Linearity of Adaptation",
    "Smoothness Index"
  )
  
  # extract PCA loadings
  rot <- as.data.frame(pca$rotation)
  
  rot_table <- rot[, 1:3] %>%
    mutate(Feature = features) %>%
    select(Feature, PC1, PC2, PC3)
  
  # gt table
  rot_table %>%
    gt() %>%
    fmt_number(
      columns = c(PC1, PC2, PC3),
      decimals = 2
    ) %>%
    cols_label(
      Feature = "Feature",
      PC1 = "PC1",
      PC2 = "PC2",
      PC3 = "PC3"
    ) %>%
    tab_header(
      title = "Principal Component Analysis (PCA) Loadings",
      subtitle = ""
    ) %>%
    tab_options(
      table.font.size = px(12),
      column_labels.font.weight = "bold"
    )
  
  
}


