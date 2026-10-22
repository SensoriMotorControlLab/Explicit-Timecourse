getFeatures <- function() {
  
  strategy_data <- read.csv(
    "data/strategy_only_participants.csv",
    stringsAsFactors = FALSE
  )
  
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
    onset_trial = numeric(),
    stable_trial = numeric(),
    learning_sd = numeric(),
    learning_length = numeric(),
    learning_abs_diff = numeric(),
    num_sign_flips_prop = numeric(),
    jump_ratio = numeric(),
    largest_jump_frac = numeric(),
    lin_r2 = numeric(),
    smoothness = numeric(),
    stringsAsFactors = FALSE
  )
  
  
  
  ids <- unique(strategy_data$participant_id)
  
  for (i in seq_along(ids)) {
    
    id <- ids[i]
    
    d_all <- strategy_data %>%
      filter(participant_id == id) %>%
      arrange(cutrial_no)
    
    d_rot <- strategy_data %>%
      filter(
        participant_id == id,
        trial_type == "rotated"
      ) %>%
      arrange(cutrial_no)
    
    
    # need enough rotated trials for stabilization calculation
    if (nrow(d_rot) < 10 || nrow(d_all) < 2) next
    
    
    aim <- d_rot$aimdeviation_deg
    
    # rotation-relative trial number
    trials <- d_rot$trial_idx
    
    
    # ----------------------------------------------------------
    # STRATEGY ONSET
    # First rotated trial where aim > 5 degrees
    # ----------------------------------------------------------
    
    onset_idx <- which(aim > 5.75)[1]
    
    if (is.na(onset_idx)) next
    
    onset <- trials[onset_idx]
    

# ----------------------------------------------------------
# 85% of final strategy + mean absolute change < 5°
# Eight-trial moving window
# ----------------------------------------------------------

  window <- 8
  strategy_threshold <- 0.85
  change_threshold <- 5

  n <- length(aim)

  if (n < window) next

  # final strategy: median of final 8 rotated trials
  final_strategy <- median(tail(aim, 8), na.rm = TRUE)

  # Candidate windows beginning at onset
  if (onset_idx > n - window + 1) next

  starts <- seq.int(onset_idx, n - window + 1)

  # Calculate median aim and mean absolute change
  medians <- sapply(starts, function(i) {
   median(aim[i:(i + window - 1)])
  })

  changes <- sapply(starts, function(i) {
   mean(abs(diff(aim[i:(i + window - 1)])))
  })


# ----------------------------------------------------------
# Find stabilization
# ----------------------------------------------------------

qualifies <- medians >=
  strategy_threshold * final_strategy &
  changes < change_threshold &
  starts > onset_idx

if (any(qualifies)) {

  # First window satisfying both conditions
  stable_idx <- starts[which(qualifies)[1]]

} else {

  # Find window closest to satisfying both conditions
  eligible <- which(starts > onset_idx)

  if (length(eligible) == 0) next

  score <- pmax(
    0,
    strategy_threshold * final_strategy - medians
  ) + pmax(
    0,
    changes - change_threshold
  )

  best <- eligible[which.min(score[eligible])]

  stable_idx <- starts[best]
}


stable <- trials[stable_idx]

# Stabilization must occur after onset
if (stable <= onset) next

    # baseline aim
    
    baseline_aim <- d_all$aimdeviation_deg[
      d_all$cutrial_no >= 105 &
        d_all$cutrial_no <= 112
    ]
    
    baseline_aim <- baseline_aim[!is.na(baseline_aim)]
    
    if (length(baseline_aim) < 2) next
    
    baseline_mean <- mean(baseline_aim)
    
    
    # LEARNING WINDOW
    # onset -> stabilization
    
    learning_idx <- which(
      trials >= onset &
        trials <= stable
    )
    
    learning_aim <- aim[learning_idx]
    

    learning_aim <- learning_aim[!is.na(learning_aim)]
    
    if (length(learning_aim) < 2) next
    
    

    #normalize by baseline
    learning_centered <- learning_aim - baseline_mean
    
    
    # ==========================================================
    # FEATURES
    # ==========================================================
    
    # variability during learning
    learning_sd <- sd(learning_centered)
    
    
    # duration of learning period
    learning_length <- length(learning_aim)
    
    
    # trial-to-trial changes
    diffs <- diff(learning_aim)
    
    
    # average absolute trial-to-trial change
    learning_abs_diff <- mean(abs(diffs))
    
    
    
    # SIGN FLIPS
    # changes in direction of trial-to-trial aiming changes
    
    num_sign_flips <- sum(
      diff(sign(diffs)) != 0
    )
    
    num_sign_flips_prop <- 
      num_sign_flips / learning_length
    
    
    
    # jump features
    
    max_jump <- max(abs(diffs))
    mean_jump <- mean(abs(diffs))
    
    jump_ratio <- 
      max_jump / (mean_jump + 1e-6)
    
    largest_jump_frac <- 
      max_jump /
      (sum(abs(diffs)) + 1e-6)
    
    
    
    # smoothness
    
    smoothness <- 
      sum(abs(diffs)) /
      (
        max_jump * length(diffs) +
          1e-6
      )
    
    
    # linear trend
    
    t <- seq_along(learning_aim)
    
    lin_r2 <- summary(
      lm(learning_aim ~ t)
    )$r.squared
    
    
    
    # NA / Inf SAFETY
    learning_sd <- ifelse(
      is.finite(learning_sd),
      learning_sd,
      0
    )
    
    learning_abs_diff <- ifelse(
      is.finite(learning_abs_diff),
      learning_abs_diff,
      0
    )
    
    num_sign_flips_prop <- ifelse(
      is.finite(num_sign_flips_prop),
      num_sign_flips_prop,
      0
    )
    
    jump_ratio <- ifelse(
      is.finite(jump_ratio),
      jump_ratio,
      0
    )
    
    largest_jump_frac <- ifelse(
      is.finite(largest_jump_frac),
      largest_jump_frac,
      0
    )
    
    smoothness <- ifelse(
      is.finite(smoothness),
      smoothness,
      0
    )
    
    lin_r2 <- ifelse(
      is.finite(lin_r2),
      lin_r2,
      0
    )
    
    
    features <- rbind(
      features,
      data.frame(
        participant_id = id,
        onset_trial = onset,
        stable_trial = stable,
        learning_sd = learning_sd,
        learning_length = learning_length,
        learning_abs_diff = learning_abs_diff,
        num_sign_flips_prop = num_sign_flips_prop,
        jump_ratio = jump_ratio,
        largest_jump_frac = largest_jump_frac,
        lin_r2 = lin_r2,
        smoothness = smoothness,
        stringsAsFactors = FALSE
      )
    )
  }
  
  
  
  # STANDARDIZE FEATURES
  features_z <- features %>%
    mutate(
      across(
        all_of(numeric_cols),
        ~ as.numeric(scale(.))
      )
    ) %>%
    ungroup()
  
  
  return(
    list(
      features = features,
      features_z = features_z,
      input = features_z[, numeric_cols]
    )
  )
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



plotScree <- function () {
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
featuresFile <- getFeatures()$features_z
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
  # Load required packages inside the function
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  # Fetch data
  out <- kruskalFeatures()
  df_merged <- out$data
  
  # Define custom colors per rotation
  rot_cols <- c(
    "20" = "#4cc9f0",
    "30" = "#4895ef",
    "40" = "#4261ee",
    "50" = "#2835af",
    "60" = "#12086f"
  )
  
  # Order of all 8 features
  features <- c(
    "learning_sd", "learning_length", "learning_abs_diff", 
    "num_sign_flips_prop", "jump_ratio", "largest_jump_frac", 
    "lin_r2", "smoothness"
  )
  
  # 1. Reshape data from wide to long format for faceting
  df_long <- df_merged %>%
    pivot_longer(cols = all_of(features), names_to = "feature", values_to = "value") %>%
    mutate(feature = factor(feature, levels = features)) 
  
  # 2. Position asterisks LOWER (with fixed y limits, we can place them right at y = 4.5)
  anno_df <- data.frame(
    feature = factor(features, levels = features),
    label = case_when(
      features == "learning_sd"       ~ "***",  # p = 0.000101
      features == "learning_abs_diff" ~ "**",  # p = 0.000763
      TRUE                           ~ ""      
    ),
    x = 3,   # Centered over the "40" rotation
    y = 4.5  # Fixed height below the upper limit of 5
  ) %>%
    filter(label != "") 
  
  # 3. Clean display labels for your 8 features
  feature_labels <- c(
    "learning_sd"         = "Aiming Variability",
    "learning_length"     = "Strategy Development Duration",
    "learning_abs_diff"   = "Absolute Trial Differences",
    "num_sign_flips_prop" = "Directional Sign Flips",
    "jump_ratio"          = "Abruptness index",
    "largest_jump_frac"   = "Largest Step Index",
    "lin_r2"              = "Linearity of Adaptation",
    "smoothness"          = "Smoothness Index"
  )
  
  # 4. Generate the plot
  ggplot(df_long, aes(x = factor(rotation), y = value, fill = factor(rotation))) +
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
    
    # 4 columns and 2 rows. Removed scales="free_y" so all subplots match.
    facet_wrap(~ feature, ncol = 4, labeller = as_labeller(feature_labels)) +
    
    # Enforce uniform y-axis boundaries and tick marks (-3 to 5)
    coord_cartesian(ylim = c(-3, 5)) +
    scale_y_continuous(breaks = -3:5) +
    scale_fill_manual(values = rot_cols) +
    
    # Layer the significance asterisks uniformly
    geom_text(
      data = anno_df, 
      aes(x = x, y = y, label = label), 
      inherit.aes = FALSE, 
      size = 6, 
      fontface = "bold",
      vjust = 0.5
    ) +
    
    labs(
      x = "Rotation magnitude (°)",
      y = "Feature Value"
    ) +
    
    theme_classic(base_size = 12) +
    theme(
      legend.position = "none",
      axis.line = element_line(color = "black"),
      axis.title = element_text(size = 14),  
      axis.text = element_text(size = 11, color = "black"), 
      strip.text = element_text(size = 11, face = "bold"), 
      strip.background = element_blank()                  
    ) 
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
    "Strategy Development Duration",
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

