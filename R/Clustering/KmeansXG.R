#kmeans and xgboost classified learning phase
getFeaturesFromModel <- function() {
  model_df <- xgRun()
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  window_size = 4
  
  
  features <- data.frame(
    participant_id = character(),
    learning_sd = numeric(),
    learning_length = numeric(),
    learning_abs_diff = numeric(),
    num_negative_aims = numeric(),
    incrementality = numeric(),
    num_sign_flips = numeric(),
    jump_ratio = numeric(),
    largest_jump_frac = numeric(),
    lin_r2 = numeric(),
    early_jump_frac = numeric(),
    max_jump_norm = numeric(),
    smoothness = numeric(),
    step_index = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(model_df)) {
    
    id <- model_df$participant_id[i]
    onset  <- model_df$pred_start[i]
    stable <- model_df$pred_end[i]
    
    if (is.na(onset) | is.na(stable)) next
    
    d <- subset(strategy_data, participant_id == id & trial_type.x == "rotated")
    aim <- d$aimdeviation_deg
    trials <- d$trial_idx
    
    # learning-phase trial window
    learning_trials <- which(trials >= onset & trials <= stable)
    learning_aim <- aim[learning_trials]
    
    if (length(learning_aim) < 2) next  
    
    
    # features
    learning_sd       <- sd(learning_aim)
    learning_length   <- length(learning_trials)
    learning_abs_diff <- mean(abs(diff(learning_aim)))
    num_negative_aims <- mean(learning_aim < 0)
    diffs             <- diff(learning_aim)
    num_sign_flips    <- sum(diff(sign(diffs)) != 0)
    
    #step feature 
    max_jump <- max(abs(diffs))
    mean_jump <- mean(abs(diffs))
    jump_ratio <- max_jump / mean_jump
    max_jump_norm <- max(abs(diffs)) / (max(learning_aim) - min(learning_aim) + 1e-6)
    
    smoothness <- sum(abs(diffs)) / (max(abs(diffs)) * length(diffs) + 1e-6)
    
    abs_diffs <- abs(diff(learning_aim))
    largest_jump_frac <- max(abs_diffs) / (sum(abs_diffs) + 1e-6)
    step_index <- largest_jump_frac / (smoothness + 1e-6)
    
    
    t <- seq_along(learning_aim)
    lin_r2 <- summary(lm(learning_aim ~ t))$r.squared
    
    
    aim_trend_cor <- cor(seq_along(learning_aim), learning_aim)
    # Erraticness feature: SD of trial-to-trial changes
    diff_sd <- sd(diffs)
    
    
    learning_sd       <- ifelse(is.na(learning_sd), 0, learning_sd)
    learning_abs_diff <- ifelse(is.na(learning_abs_diff), 0, learning_abs_diff)
    num_negative_aims <- ifelse(is.na(num_negative_aims), 0, num_negative_aims)
    num_sign_flips    <- ifelse(is.na(num_sign_flips), 0, num_sign_flips)
    #diff_sd           <- ifelse(is.na(diff_sd), 0, diff_sd)
    cumulative_change <- abs(sum(diffs))
    
    incrementality <- cumulative_change / (max_jump + 1e-6)
    diffs <- diff(learning_aim)
    n <- length(diffs)
    early_n <- ceiling(0.3 * n)  # first 30% of trials
    early_jump_frac <- sum(abs(diffs[1:early_n])) / (sum(abs(diffs)) + 1e-6)
    
    
    
    features <- rbind(
      features,
      data.frame(
        participant_id = id,
        learning_sd = learning_sd,
        learning_length = learning_length,
        learning_abs_diff = learning_abs_diff,
        num_negative_aims = num_negative_aims,
        incrementality = incrementality,
        num_sign_flips = num_sign_flips,
        jump_ratio=jump_ratio,
        largest_jump_frac = largest_jump_frac,
        lin_r2 = lin_r2,
        early_jump_frac = early_jump_frac,
        max_jump_norm = max_jump_norm,
        smoothness = smoothness,
        step_index = step_index,
        stringsAsFactors = FALSE
      )
    )
  }
  
  numeric_cols <- c(
    "learning_sd",
    "learning_abs_diff",
    "learning_length",
    "num_negative_aims",
    "largest_jump_frac",
    "jump_ratio",
    "lin_r2",
    "smoothness"
  
    
  )
  
  
  list(
    features_df = features,
    kmeans_input = features[, numeric_cols]
  )
}


kPCA <- function () {
  extract <- getFeaturesFromModel() 
  
  model_df <- xgRun()
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  
  features_df <- extract$features_df        
  k_input     <- extract$kmeans_input     
  k_scaled <- scale(k_input)
  
  # -------------------------------
  # PCA 
  pca <- prcomp(k_scaled, center = TRUE, scale. = FALSE)
  pca$rotation <- pca$rotation*-1
  pca$x<- pca$x*-1
  
  pca_df <- as.data.frame(pca$x[,1:3])
  colnames(pca_df) <- c("PC1","PC2",'PC3')
  pca_df$participant_id <- features_df$participant_id
  
  # -------------------------------
  # 3. K-means clustering
  set.seed(123)
  km_res <- kmeans(pca_df[,c("PC1","PC2",'PC3')], centers = 3, nstart = 50)
  
  pca_df$cluster <- km_res$cluster
  pca_df$cluster_label <- as.factor(km_res$cluster)
  return(pca_df)
  
}

plotScree <- function () {
  extract <- getFeaturesFromModel() 
  features_df <- extract$features_df        
  k_input     <- extract$kmeans_input     
  k_scaled <- scale(k_input)
  
  pca <- prcomp(k_scaled, center = TRUE, scale. = FALSE)
  
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
       xlim = c(0, 8.2)
  )
  
  axis(1, at = 0:8.2)
  axis(2)
  
  abline(h = 0.8, col = "grey", lty = 2, lwd = 1)
  
  points(1:3, cum_var[1:3], col = "#f52f57", pch = 19)
  
  # var_each <- pca$sdev^2
  # screeplot(pca, type="lines")
  # points(var_each, col="red")
}

#which components? - plot pca
#pca$rotation
# Cluster separation along x-axis (PC1) mostly reflects stability vs erratic learning.
# Separation along y-axis (PC2) mostly reflects short vs long learning periods.


##NOTE: multiplied values by -1 (to be positive)
#       so higher the pca load, more variability, or longer the duration/more signflip

plotRaw <- function () {
  pca_df <- kPCA()
  model_df <- xgRun()
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  
  model_df_with_clusters <- model_df %>%
    left_join(pca_df[,c("participant_id", "cluster_label")], by = "participant_id")
  
  strategy_data_clustered <- strategy_data %>%
    inner_join(model_df_with_clusters %>% select(participant_id, cluster_label),
               by = "participant_id") %>%
    filter(!is.na(cluster_label))  
  
  
  ggplot(strategy_data_clustered, 
         aes(x = trial_idx, y = aimdeviation_deg, group = participant_id)) +
    
    geom_line(alpha = 0.5, color = "steelblue") +
    geom_hline(yintercept = 0, color = "black", size = 0.8) +
    facet_wrap(~ cluster_label, ncol = 1) +
    
    labs(
      x = "Trial",
      y = "Aim Deviation (deg)",
      title = "Aiming Trajectories by Cluster"
    ) +
    
    theme_minimal(base_size = 14) +
    ylim(-100, 100)
}

plotComponents <- function() {
  pca_df <- kPCA()
  
  pca_df <- pca_df %>%
    mutate(
      cluster_label = factor(
        cluster_label,
        levels = c(2, 3, 1),
        labels = c("Gradual", "Exploratory", "Stepwise")
      )
    )
  
  hulls <- pca_df %>%
    group_by(cluster_label) %>%
    slice(chull(PC1, PC2))
  
  ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster_label)) +
    geom_point(size = 3, alpha = 0.8) +
    geom_polygon(data = hulls, aes(fill = cluster_label), alpha = 0.15, color = NA) +
    labs(x = "(PC1): Learning Stability (Trajectory Smoothness)", y = "(PC2): Learning Variance", color = "Cluster", fill = "Cluster") +
    scale_fill_manual(values = c(
      "Exploratory" = "#c495c9",
      "Gradual"     = "#3dcad4",
      "Stepwise"    = "#d16483"
    )) +
    coord_equal() +
    scale_x_continuous(breaks = seq(-2, 10, by = 2)) +
    scale_y_continuous(breaks = seq(-2, 10, by = 2)) +
    
    scale_color_manual(values = c(
      "Exploratory" = "#c495c9",
      "Gradual"     = "#3dcad4",
      "Stepwise"    = "#d16483"
    )) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
       axis.line = element_line()
      # axis.text.x  = element_text(size = 24),
      # axis.text.y  = element_text(size = 24),
      # axis.title.x = element_text(size = 17),
      # axis.title.y = element_text(size = 17),
      # legend.title = element_text(size = 18),
      # legend.text  = element_text(size = 17) 
    )
}

plotTSNE <- function() {
  pca_df <- kPCA()
  pca_df <- pca_df %>%
    mutate(
      cluster_label = factor(
        cluster_label,
        levels = c(3, 2, 1),
        labels = c("Gradual", "Exploratory", "Stepwise")
      )
    )
  
  library(Rtsne)
  pca_features <- pca_df %>% select(PC1, PC2, PC3)
  
  # Run t-SNE
  set.seed(123) 
  tsne_out <- Rtsne(as.matrix(pca_features), dims = 2, perplexity = 30, verbose = TRUE)
  
  # Add t-SNE coordinates to your dataframe
  pca_df <- pca_df %>%
    mutate(
      TSNE1 = tsne_out$Y[,1],
      TSNE2 = tsne_out$Y[,2]
    )
  
  # Compute convex hulls for each cluster
  hulls <- pca_df %>%
    group_by(cluster_label) %>%
    slice(chull(TSNE1, TSNE2))
  
  # Plot t-SNE with clusters
  ggplot(pca_df, aes(x = TSNE1, y = TSNE2, color = cluster_label)) +
    geom_point(size = 3, alpha = 0.8) +
    geom_polygon(data = hulls, aes(fill = cluster_label), alpha = 0.15, color = NA) +
    labs(x = "", y = "", title  ="t-SNE", color = "Cluster", fill = "Cluster") +
    scale_fill_manual(values = c(
      "Exploratory" = "#c495c9",
      "Gradual"     = "#3dcad4",
      "Stepwise"    = "#d16483"
    )) +
    scale_color_manual(values = c(
      "Exploratory" = "#c495c9",
      "Gradual"     = "#3dcad4",
      "Stepwise"    = "#d16483"
    )) +
    theme_minimal(base_size = 16) +
    theme(legend.position = "right")  +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(),
      axis.text.x  = element_text(size = 24),
      axis.text.y  = element_text(size = 24),
      axis.title.x = element_text(size = 17),
      axis.title.y = element_text(size = 17),
      legend.title = element_text(size = 18),
      legend.text  = element_text(size = 17) 
    )
  
}

confMatrix <- function () {
  pca_df <- kPCA()
  model_df <- xgRun()
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  
  classification <- data.frame(
    participant_id = c("35f115", "dd50ef", "ee29d6", "1e2a6b", "20d744", 
                       "2c2f44", "70e8cb", "83456b", "8d426d", "9fb9fe", "ab3b79",
                       "ba8f14", "fd5cd5", "205194", "2525df", "2528e1", "31b753",
                       "3e3a73", "422c52", "4a6642", "58e451","59a9dd", "81d984","96634a",
                       "ba0a7c", "bccf6e","bde44b", "d1436b", "ffa337", "05484c", "0f6fbf",
                       "194dab", "360ea6","52ef4e", "54044d", "7eacfc", "901482", "94709f", 
                       "a18f63", "af8328", "b4d36d", "bb04a2", "c0144b", "d6141d", "e066de",
                       "d9ff04", "0b1dca", "0c7728", "13cb04", "13d986", "14893b", "1896cb",
                       "19e7ed", "33e532", "4093e8", "54c6f3", "622518",
                       "7cd1bd","7eec53","811eae","98e5cb","9db7b0","9eabc1","a23b35","a5310d","abf95a",
                       "bdb042","d0d19c","f275ca", "a02c67", "396716", "15f2a1", 
                       "19187c", "3091de", "654648",
                       
                       "fecce7", "feb484", "fd3e77", "ef7c34", "d0a134", 
                       "b7804c", "b5edb9", "b3fac9", "ac4304", "9d628e",
                       "9c8da7", "9b1b25","98abcb","977304","8bc9fd",
                       "85592d","822948","803175","7ddb15","73230b",
                       "71ea32","6442f3","6387a3","514daa","5129de",
                       "4ff32b","4cbb11","4b9fad","4b701d","30035a",
                       "2ff26d","27edd0","24c273","1cce80","1cc592","1c10b9",
                       
                       
                       
                       "9b5b71","7454c1","a16f97","a7178b","d53112",
                       "ad1dea", "afaaf4", "dfe4d5", "fa0f1a", "fe59c4",
                       "d1d7c3","d10bdf","03fd31","49e772","56968f", "bd8518", "139857", "657fba",
                       "a4cf19","2c82f8","2f40e0", "6359c5", "1d818f" ),
    
    
    
    
    label = c("step", "step", "step", "step", "step", 
              "step", "step", "erratic", "step", "step", "erratic", "step",
              "gradual", "step", "step", "step", "step", "erratic", "gradual", "gradual",
              "gradual", "erratic", "gradual", "gradual", "step", "step", "gradual","step", "step", 
              "erratic", "erratic","erratic",  "erratic", "erratic", "step", "step", "gradual", "step",
              "step","erratic", "step", "step", "step",  "step", "step",
              "step", "step", "erratic", "step", "step", "step", "erratic",
              "erratic", "gradual", "erratic", "step", "step",
              "step",  "step","gradual", "erratic", "step", "step", "gradual",
              "erratic", "step","erratic", "gradual", "erratic", "erratic", "erratic", "step",
              "erratic", "erratic", "erratic",
              
              "gradual", "step", "erratic", "step", "step",
              "step", "erratic", "step", "erratic", "step",
              "gradual","step","step","step","step",
              "gradual","erratic","erratic","step","erratic",
              "gradual","erratic","erratic","erratic","gradual",
              "step","step","step","step","step",
              "erratic","erratic","step","step","step", "step",
              
              
              "step", "erratic", "erratic", "gradual", "step", 
              "step", "gradual", "step", "step","erratic",
              "gradual","step","erratic","erratic","step", "gradual", "step", "erratic",
              "erratic","step","step", "step", "gradual")
  )
  
  classification$label[classification$participant_id == "13d986"] <- "gradual"
  
  
  
  #CONFUSION TABLE
  model_df_with_clusters <- model_df %>%
    left_join(pca_df[,c("participant_id", "cluster_label")], by = "participant_id")
  
  model_df_labeled <- model_df_with_clusters %>%
    inner_join(classification, by = "participant_id")
  strategy_data_labeled <- strategy_data %>%
    inner_join(
      model_df_labeled %>% select(participant_id, label, cluster_label),
      by = "participant_id"
    )
  
  cluster_map <- c("1", "2", "3")
  
  model_df_labeled$cluster_aligned <- factor(cluster_map[as.numeric(model_df_labeled$cluster_label)],
                                             levels = c("1", "2", "3"))
  
  table(model_df_labeled$label, model_df_labeled$cluster_aligned)
}




###bar plot
pcaBar <- function() {
  strategy_data_clustered <- plotRaw()$data
  
  
  proportions_df <- strategy_data_clustered %>% group_by(rotation, cluster_label) %>% 
    summarise(n_participants = n_distinct(participant_id), .groups = "drop") %>% 
    group_by(rotation) %>% mutate(prop = n_participants / sum(n_participants)) %>% 
    ungroup() 
  
  proportions_df$cluster_label <- factor(proportions_df$cluster_label, levels = sort(unique(proportions_df$cluster_label)))
  
  
  strategy_df <- getStrategies()
  
  total_per_rotation <- strategy_df %>%
    group_by(rotation) %>%
    summarise(total_n = n(), .groups = "drop")
  
  non_strategy <- strategy_df %>%
    filter(strategy == "No") %>%
    group_by(rotation) %>%
    summarise(n_non_strategy = n(), .groups = "drop") %>%
    left_join(total_per_rotation, by = "rotation") %>%
    mutate(
      cluster_label = "Non-strategy",
      prop = n_non_strategy / total_n
    )
  
  non_strategy <- non_strategy %>%
    rename(n_participants = n_non_strategy)
  
  proportions_df$cluster_label <- factor(proportions_df$cluster_label, 
                                         levels = c("1", "2", "3", "Non-strategy"))
  
  # Combine
  proportions_combined <- bind_rows(proportions_df, non_strategy)
  proportions_combined <- proportions_combined %>%
    filter(!is.na(cluster_label))
  proportions_combined$cluster_label <- factor(
    proportions_combined$cluster_label,
    levels = c("Non-strategy", "2", "3", "1"),
    labels = c("Non-Strategy", "Gradual","Exploratory","Stepwise")
  )
  
  ##find percentages
  rotation_totals <- proportions_combined %>%
    group_by(rotation) %>%
    summarise(total_n = sum(n_participants), .groups = "drop")
  
  proportions_plot <- proportions_combined %>%
    group_by(rotation) %>%
    mutate(
      total_n = sum(n_participants),
      percent = n_participants / total_n * 100
    ) %>%
    ungroup() %>%
    filter(cluster_label != "Non-Strategy")
  
  
  p <- ggplot(
    proportions_plot,
    aes(x = factor(rotation), y = percent, fill = cluster_label)
  ) +
    geom_bar(stat = "identity", color = "black", width = 0.7) +
    scale_fill_manual(
      values = c(
        "Gradual"    = "#3dcad4",
        "Exploratory" = "#c495c9",
        "Stepwise"     = "#d16483"
      ),
      labels = c("Gradual n = 16", "Exploratory n = 34", "Stepwise n = 64"),
      name = "Phenotype"
    ) +
    
    scale_y_continuous(limits = c(0, 100)
    ) +
    labs(
      x = "Rotation Group",
      y = "Percentage of Participants (%)",
      title = ""
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line()
      # axis.text.x  = element_text(size = 17),
      # axis.text.y  = element_text(size = 17),
      # axis.title.x = element_text(size = 17),
      # axis.title.y = element_text(size = 17),
      # legend.title = element_text(size = 17),
      # legend.text  = element_text(size = 16)
    ) 
  print(p)
  return(proportions_plot)
}





