##Unsupervised
library(dplyr)
library(ggplot2)
library(viridis)
library(knitr)
library(kableExtra)
library(tidyr)
library(gt)
library(cluster)

#Features
# (1) look at SD between the trial at which people start changing their aimdeviation_deg 
#    ( t-test comparing mean to 0) to the point at which they settle on a stable strategy (t-test showing difference from aim dev in last 16 trials)
# (2) find length of this "learning ohase" b/w two type points
# (3) Looking at variance and trial order of learning phase abs(mean()) diff())

getLearningPhase <- function() {
  
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  strategy_data <- subset(strategy_data, trial_type.x == "rotated" & trial_idx < 121)
  
  results <- data.frame(
    participant_id = character(),
    stable_strategy_trial = numeric(),
    stable_strategy_aimdev = numeric(),
    strategy_onset_trial = numeric(),
    strategy_onset_aimdev = numeric()
  )
  
  for (id in unique(strategy_data$participant_id)) {
    
    d <- subset(strategy_data, participant_id == id)
    aim <- d$aimdeviation_deg
    trials <- d$trial_idx
    N <- length(aim)
    stable_window_size <- 16  
    rolling_window <- 16
    
    #### STABLE STRATEGY: backward from final window ####
    stable_window <- tail(aim, stable_window_size)
    stable_mean <- mean(stable_window, na.rm=TRUE)
    
    # Forward rolling t-test
    stable_tp <- NA
    for (start in 1:(N - rolling_window + 1)) {
      window <- aim[start:(start + rolling_window - 1)]
      t_res <- t.test(window, mu = stable_mean)
      
      if (t_res$p.value > 0.05) {
        stable_tp <- trials[start + rolling_window - 1]  # last trial of window considered stable
        break  # exit loop once we find first stable window
      }
    }
 
    if (!is.na(stable_tp)) {
      stable_idx <- which(trials == stable_tp)
      window_end <- min(stable_idx + stable_window_size - 1, length(aim))
      stable_strategy_aimdev <- mean(aim[stable_idx:window_end])
    } else {
      stable_strategy_aimdev <- NA
    }
      
    
    # Interpretation:
    # stable_tp = first trial (backward) whose rolling window is NOT significantly different from the final stable mean
    # This marks the onset of “stable strategy”
    
    
    #### STRATEGY ONSET 
    onset_tp <- NA
    onset_window <- 8   
    min_aimdev <- 4.5    
    
    for (start in 1:(N - onset_window + 1)) {
      window <- aim[start:(start + onset_window - 1)]
      t_res <- t.test(window, mu = 3) #bc thats the strategy criteria we have in our ci
      if (t_res$p.value < 0.05 & abs(mean(window)) >= min_aimdev) {
        onset_tp <- trials[start]
        break
      }
    }

    results <- rbind(
      results,
      data.frame(
        participant_id = id,
        stable_strategy_trial = stable_tp,
        stable_strategy_aimdev = if (!is.na(stable_tp)) mean(aim[which(trials == stable_tp):min(which(trials == stable_tp) + stable_window_size - 1, length(aim))], na.rm = TRUE) else NA,
        strategy_onset_trial = onset_tp,
        strategy_onset_aimdev = if (!is.na(onset_tp)) mean(aim[which(trials == onset_tp):min(which(trials == onset_tp) + onset_window - 1, length(aim))], na.rm = TRUE) else NA
      )
    )
  }
  
  return(results)
}



# ---- Function to extract learning phase features for k-means ----
getFeatures <- function() {
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  strategy_data <- subset(strategy_data, trial_type.x == "rotated" & trial_idx < 121)
  results <- getLearningPhase()
  
  features <- data.frame(
    participant_id = character(),
    learning_sd = numeric(),
    learning_length = numeric(),
    learning_abs_diff = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(results)) {
    
    id <- results$participant_id[i]
    onset <- results$strategy_onset_trial[i]
    stable <- results$stable_strategy_trial[i]
    
    if (is.na(onset) | is.na(stable)) next
    
    # participant's aim data
    d <- subset(strategy_data, participant_id == id)
    aim <- d$aimdeviation_deg
    trials <- d$trial_idx
    
    #define learning phase
    learning_trials <- which(trials >= onset & trials <= stable)
    learning_aim <- aim[learning_trials]
    
    if(length(learning_trials) < 2) next  # need at least 2 trials to compute diff
    
    
    
    # Features
    learning_sd <- sd(learning_aim)
    learning_length <- length(learning_trials)
    learning_abs_diff <- mean(abs(diff(learning_aim)))
    ## maybe add sd difference from baseline
    
    
    
    features <- rbind(
      features,
      data.frame(
        participant_id = id,
        learning_sd = learning_sd,
        learning_length = learning_length,
        learning_abs_diff = learning_abs_diff,
        stringsAsFactors = FALSE
      )
    )
  }
  
  
  kmeans_features <- features[, c("learning_sd", "learning_length", "learning_abs_diff")]
  
  return(list(
    features_df = features,       
    kmeans_input = kmeans_features  #
  ))
}

kMeans <- function () {
  extract <- getFeatures()
  
  
  numeric_features <- extract$features_df[, c("learning_sd", "learning_length", "learning_abs_diff")]
  kmeans_input_scaled <- scale(extract$kmeans_input)
  
  pca <- prcomp(kmeans_input_scaled, center = TRUE, scale. = FALSE)
  explained_var <- pca$sdev^2 / sum(pca$sdev^2)
  
  # Take top 2 PCs
  trial_pca <- as.data.frame(pca$x[, 1:2])
  colnames(trial_pca) <- c("PC1", "PC2")
  trial_pca$participant_id <- extract$features_df$participant_id
  
  set.seed(123)
  km_res <- kmeans(trial_pca[, 1:2], centers = 3, nstart = 25) ####
  cluster_labels <- c("gradual", "erratic", "step")
  trial_pca$cluster <- km_res$cluster
  
  cluster_order <- aggregate(PC1 ~ cluster, data = trial_pca, FUN = mean)
  cluster_order <- cluster_order[order(cluster_order$PC1), "cluster"]
  # reorder cluster numbers so Cluster 1 has smallest PC1 mean
  trial_pca$cluster <- factor(trial_pca$cluster, labels = cluster_labels)
  
  features_with_clusters <- cbind(extract$features_df, cluster = trial_pca$cluster)
  
  print(features_with_clusters)
  
  clusplot(
    x = kmeans_input_scaled,  
    clus = as.numeric(trial_pca$cluster),      
    color = TRUE,             
    shade = TRUE,            
    labels = 2,               
    lines = 0                  
  )
  
  
  return(list(
    features_with_clusters = features_with_clusters,
    pca = pca,
    trial_pca = trial_pca,
    km_res = km_res
  ))
}





plotCluster <- function () {
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  strategy_data <- subset(strategy_data, trial_type.x == "rotated" & trial_idx < 121)
  strategy_data$participant_id <- as.character(strategy_data$participant_id)
 
  km_out <- kMeans()
  features_with_clusters <- km_out$features_with_clusters
  features_with_clusters$participant_id <- as.character(features_with_clusters$participant_id)
  
  strategy_data_clustered <- strategy_data %>%
    inner_join(features_with_clusters %>% select(participant_id, cluster), by = "participant_id")
  
  strategy_data_clustered$cluster <- as.factor(strategy_data_clustered$cluster)
  
  ggplot(strategy_data_clustered, aes(x = trial_idx, y = aimdeviation_deg, group = participant_id)) +
    geom_line(alpha = 0.5, color = "steelblue") +      # individual participant lines
    geom_hline(yintercept = 0, color = "black", size = 0.8) +
    facet_wrap(~cluster, ncol = 1) +                  # one panel per cluster
    labs(x = "Trial", y = "Aim Deviation (deg)", title = "Aiming Trajectories by Cluster") +
    theme_minimal() +
    ylim(-60, 60) 
  
}


#erratic if more than 3 sign flips

compare <- function () {
  res <- kMeans()  
  
  features_with_clusters <- res$features_with_clusters 
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
                     "bdb042","d0d19c","f275ca", "a02c67"),
  
  label = c("step", "step", "step", "step", "step", 
            "step", "step", "erratic", "step", "step", "erratic", "step",
            "gradual", "step", "step", "step", "step", "erratic", "gradual", "gradual",
            "gradual", "erratic", "gradual", "gradual", "step", "step", "gradual","step", "step", 
            "erratic", "erratic","erratic",  "erratic", "erratic", "step", "step", "gradual", "step",
            "step","erratic", "step", "step", "step",  "step", "step",
            "step", "step", "erratic", "step", "step", "step", "erratic",
            "erratic", "gradual", "erratic", "step", "step",
            "step",  "step","gradual", "erratic", "step", "step", "gradual",
            "erratic", "step","erratic", "gradual", "erratic", "erratic")
)


  comparison <- data.frame(
    participant_id = unlist(features_with_clusters$participant_id),
    cluster_label  = unlist(features_with_clusters$cluster)
  ) %>%
    inner_join(classification, by = "participant_id")
conf_mat <- table(comparison$cluster_label, comparison$label)
print(conf_mat)
chi_res <- chisq.test(conf_mat)
print(chi_res)
}





#look at step people, fit where the step occurs and answer if they are predicted by rotation
## when do erratic people start exploring how long to they explore for - what is their final strategy


##for plot keep the step plot and colour code by rotation size and see how ppl differ






# Table 1: cols rotation - step
# row 1: step ppl -- count step cluster
# row 2: step size per rotation - count getLearningPahse col strategy_onset_aimdev for step ppl
# row 3: step trial per rotation get colstrategy_onset_trial from get learning phase

# Table 2: exploration
# row 1: exploration ppl -count erratic cluster
# row 2: time spent exploring so length of learning phase - count strategy_onset_trial-stable_strategy_trial
# row 3: aim magnitude - strategy_onset_aimdev for erratic ppl



learning_phase_table <- function() {
  
  km_res_list <- kMeans()$features_with_clusters
  learning_phase <- getLearningPhase()
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  
  # Ensure matching IDs
  learning_phase$participant_id <- as.character(learning_phase$participant_id)
  km_res_list$participant_id <- as.character(km_res_list$participant_id)
  strategy_data$participant_id <- as.character(strategy_data$participant_id)
  
  # Add cluster label
  learning_phase <- learning_phase %>%
    left_join(km_res_list[, c("participant_id", "cluster")], by = "participant_id")
  
  # Add rotation column
  strategy_data <- strategy_data %>% rename(strategy_onset_trial = trial_idx)
  learning_phase <- learning_phase %>%
    left_join(strategy_data[, c("participant_id", "strategy_onset_trial", "rotation")],
              by = c("participant_id", "strategy_onset_trial"))
  
  total_participants <- n_distinct(km_res_list$participant_id)
  
  # ---------------------------------------------------
  # TABLE 1: STEP LEARNERS
  # ---------------------------------------------------
  step_table <- learning_phase %>%
    filter(cluster == "step") %>%
    group_by(rotation) %>%
    summarise(
      percent = (n() / 70) * 100,  
      mean_step_size = mean(strategy_onset_aimdev, na.rm = TRUE),
      mean_step_trial = mean(strategy_onset_trial, na.rm = TRUE),
      mean_aim_magnitude = mean(stable_strategy_aimdev, na.rm = TRUE)
    ) %>%
    pivot_longer(-rotation, names_to = "metric", values_to = "value") %>%
    pivot_wider(names_from = rotation, values_from = value) %>%
    mutate(metric = recode(metric,
                           "percent"            = "% Step-Learners",              
                           "mean_step_size"     = "Mean step size",
                           "mean_step_trial"    = "Mean step onset trial",
                           "mean_aim_magnitude" = "Mean stable strategy size"))  
  
  # gt table for step learners
  step_gt <- step_table %>%
    gt(rowname_col = "metric") %>%
    fmt_number(columns = everything(), decimals = 2) %>%
    tab_header(
      title = "Table 1. Step Learners among STABLE-strategy users by Rotation"
    )
  
  print(step_gt)
  
  # ---------------------------------------------------
  # TABLE 2: EXPLORATION (ERRATIC) LEARNERS
  # ---------------------------------------------------
  explore_table <- learning_phase %>%
    filter(cluster == "erratic",
           !is.na(strategy_onset_trial),
           !is.na(stable_strategy_trial)) %>%
    mutate(time_spent_exploring = stable_strategy_trial - strategy_onset_trial) %>%
    group_by(rotation) %>%
    summarise(
      percent_explore_people = (n_distinct(participant_id) / 70) * 100,   # <-- FIXED
      mean_time_exploring = mean(time_spent_exploring, na.rm = TRUE),
      mean_aim_magnitude = mean(stable_strategy_aimdev, na.rm = TRUE)
    ) %>%
    pivot_longer(-rotation, names_to = "metric", values_to = "value") %>%
    pivot_wider(names_from = rotation, values_from = value) %>%
    mutate(metric = recode(metric,
                           "percent_explore_people" = "% Exploration people",
                           "mean_time_exploring"    = "Mean time exploring",
                           "mean_aim_magnitude"     = "Mean stable strategy size"))
  
  # gt table for exploration learners
  explore_gt <- explore_table %>%
    gt(rowname_col = "metric") %>%
    fmt_number(columns = everything(), decimals = 2) %>%
    tab_header(
      title = "Table 2. Erratic Learners among STABLE-strategy users by Rotation"
    )
  
  print(explore_gt)
 
  
  # ---------------------------------------------------
  # TABLE 3: GRADUAL LEARNERS
  # ---------------------------------------------------
  gradual_table <- learning_phase %>%
    filter(cluster == "gradual", !is.na(stable_strategy_trial)) %>%
    group_by(rotation) %>%
    summarise(
      percent_gradual_people = (n_distinct(participant_id) / 70) * 100,   # <-- FIXED
      mean_asymptote_trial = mean(stable_strategy_trial, na.rm = TRUE),
      mean_asymptote_aimdev = mean(stable_strategy_aimdev[!is.na(stable_strategy_aimdev)], na.rm = TRUE)
    ) %>%
    pivot_longer(-rotation, names_to = "metric", values_to = "value") %>%
    pivot_wider(names_from = rotation, values_from = value) %>%
    mutate(metric = recode(metric,
                           "percent_gradual_people" = "% Gradual people",
                           "mean_asymptote_trial" = "Mean asymptote trial",
                           "mean_asymptote_aimdev" = "Mean asymptote aim deviation"))
  
  gradual_gt <- gradual_table %>%
    gt(rowname_col = "metric") %>%
    fmt_number(columns = everything(), decimals = 2) %>%
    tab_header(
      title = "Table 3. Gradual Learners among STABLE-strategy users by Rotation"
    )
  
  print(gradual_gt)
  
  return(list(
    step_table = step_table,
    explore_table = explore_table,
    gradual_table = gradual_table
  ))
}

plotStepOnsetDensity <- function() {

  km_res_list <- kMeans()$features_with_clusters
  learning_phase <- getLearningPhase()
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  
  learning_phase$participant_id <- as.character(learning_phase$participant_id)
  km_res_list$participant_id     <- as.character(km_res_list$participant_id)
  strategy_data$participant_id   <- as.character(strategy_data$participant_id)
  

  learning_phase <- learning_phase %>%
    left_join(km_res_list[, c("participant_id", "cluster")],
              by = "participant_id")
  

  strategy_data <- strategy_data %>%
    rename(strategy_onset_trial = trial_idx)
  
  learning_phase <- learning_phase %>%
    left_join(strategy_data[, c("participant_id", "strategy_onset_trial", "rotation")],
              by = c("participant_id", "strategy_onset_trial"))
  

  step_onsets <- learning_phase %>%
    filter(cluster == "step", !is.na(strategy_onset_trial)) %>%
    group_by(participant_id, rotation) %>%
    summarise(
      strategy_onset_trial = first(strategy_onset_trial),
      .groups = "drop"
    )
  
  if (nrow(step_onsets) == 0) stop("No step learners found.")

  dens_df <- step_onsets %>%
    group_by(rotation) %>%
    do({
      d <- density(.$strategy_onset_trial, bw = 4, n = 1001)
      tibble(
        X = d$x,               # Step onset → X axis
        Y = d$y / max(d$y),    # Normalized density → Y axis
        rotation = unique(.$rotation)
      )
    }) %>%
    ungroup() %>%
    mutate(rotation = as.factor(rotation))
  
  median_df <- step_onsets %>%
    group_by(rotation) %>%
    summarise(median_onset = median(strategy_onset_trial, na.rm = TRUE)) %>%
    mutate(rotation = as.factor(rotation))
  

  rotation_levels <- levels(dens_df$rotation)
  palette <- viridis::viridis(length(rotation_levels))
  
  ggplot(dens_df, aes(x = X, y = Y, group = rotation, fill = rotation)) +
    geom_polygon(alpha = 0.3, color = "black", linewidth = 0.2) +
    geom_vline(data = median_df, aes(xintercept = median_onset, color = rotation),
               linetype = "dashed", linewidth = 0.8, show.legend = FALSE) + 
    scale_fill_manual(values = palette) +
    scale_color_manual(values = palette) +
    labs(
      title = "Density of Step Onset Trials (Step Learners)",
      x = "Step Onset Trial",
      y = "Normalized Density",
      fill = "Rotation"
    ) +
    theme_classic(base_size = 14)
}





plotExplorationDensity <- function() {
  km_res_list <- kMeans()$features_with_clusters
  learning_phase <- getLearningPhase()
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  
  learning_phase$participant_id <- as.character(learning_phase$participant_id)
  km_res_list$participant_id     <- as.character(km_res_list$participant_id)
  strategy_data$participant_id   <- as.character(strategy_data$participant_id)
  
  # Add cluster labels
  learning_phase <- learning_phase %>%
    left_join(km_res_list[, c("participant_id", "cluster")],
              by = "participant_id")

  strategy_data <- strategy_data %>%
    rename(strategy_onset_trial = trial_idx)
  
  learning_phase <- learning_phase %>%
    left_join(strategy_data[, c("participant_id", "strategy_onset_trial", "rotation")],
              by = c("participant_id", "strategy_onset_trial"))
  

  #exclude rotations 30 and 40 - not enough data
  erratic_data <- learning_phase %>%
    filter(cluster == "erratic", 
           !is.na(strategy_onset_trial), 
           !is.na(stable_strategy_trial),
           !rotation %in% c(30, 40)) %>%
    mutate(time_spent_exploring = stable_strategy_trial - strategy_onset_trial) %>%
    group_by(participant_id, rotation) %>%
    summarise(
      stable_strategy_trial = first(stable_strategy_trial),
      stable_strategy_aimdev = first(stable_strategy_aimdev),
      strategy_onset_trial = first(strategy_onset_trial),
      strategy_onset_aimdev = first(strategy_onset_aimdev),
      time_spent_exploring = first(time_spent_exploring),
      .groups = "drop"
    )

  dens_df <- erratic_data %>%
    group_by(rotation) %>%
    do({
      d <- density(.$time_spent_exploring, bw = 4, n = 1001)
      tibble(
        X = d$x,               # time spent exploring → X axis
        Y = d$y / max(d$y)     # normalized density → Y axis
      )
    }) %>%
    ungroup() %>%
    mutate(rotation = as.factor(rep(unique(erratic_data$rotation), each = 1001)))
  
  # Compute median per rotation
  median_df <- erratic_data %>%
    group_by(rotation) %>%
    summarise(median_explore = median(time_spent_exploring, na.rm = TRUE)) %>%
    mutate(rotation = as.factor(rotation))
  

  rotation_levels <- levels(dens_df$rotation)
  palette <- viridis::viridis(length(rotation_levels))
  ggplot(dens_df, aes(x = X, y = Y, group = rotation, fill = rotation)) +
    geom_polygon(alpha = 0.3, color = "black", linewidth = 0.2) +
    geom_vline(data = median_df, aes(xintercept = median_explore, color = rotation),
               linetype = "dashed", linewidth = 0.8, show.legend = FALSE) +
    scale_fill_manual(values = palette) +
    scale_color_manual(values = palette) +
    labs(
      title = "Density of Exploration Time (Erratic Learners)",
      x = "Exploration during Trials",
      y = "Normalized Density",
      fill = "Rotation"
    ) +
    theme_classic(base_size = 14)
}


# -- plot all found strategies across rotation sizes (all clusters) -- #

plotAllStable <- function () {
  km_res_list <- kMeans()$features_with_clusters
  learning_phase <- getLearningPhase()
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  
  learning_phase$participant_id <- as.character(learning_phase$participant_id)
  km_res_list$participant_id     <- as.character(km_res_list$participant_id)
  strategy_data$participant_id   <- as.character(strategy_data$participant_id)
  
  learning_phase <- learning_phase %>%
    left_join(km_res_list[, c("participant_id", "cluster")],
              by = "participant_id")
  
  # Add rotation + strategy onset/stable trials
  strategy_data <- strategy_data %>%
    rename(strategy_onset_trial = trial_idx)
  
  learning_phase <- learning_phase %>%
    left_join(strategy_data[, c("participant_id", "strategy_onset_trial", "rotation")],
              by = c("participant_id", "strategy_onset_trial"))
  
  explore_raw <- learning_phase %>%
    filter(
      cluster %in% c("step", "erratic", "gradual"),  # all strategy learners
      !is.na(stable_strategy_trial)
    ) %>%
    group_by(participant_id, cluster, rotation) %>%
    summarise(
      stable_strategy_aimdev = first(stable_strategy_aimdev),
      .groups = "drop"
    )
  medians <- explore_raw %>%
    group_by(rotation) %>%
    summarise(
      median_aim = median(stable_strategy_aimdev, na.rm = TRUE),
      .groups = "drop"
    )
  rotation_levels <- levels(factor(explore_raw$rotation))
  palette <- viridis::viridis(length(rotation_levels))
  
  ggplot(explore_raw, aes(x = stable_strategy_aimdev, fill = factor(rotation))) +
    geom_density(alpha = 0.3) +
    scale_fill_manual(values = palette) +   # <-- assign your palette here
    labs(
      title = "Distribution of Stable Strategy Aim Deviation by Rotation",
      x = "Stable Strategy Aim Deviation",
      y = "Density",
      fill = "Rotation"
    ) +
    geom_vline(
      data = medians,
      aes(xintercept = median_aim, colour = factor(rotation)),
      linetype = "dashed",
      linewidth = 1,
      show.legend = FALSE
    ) +
    
    scale_fill_manual(values = palette) +
    scale_colour_manual(values = palette) +
    theme_classic(base_size = 14)
}

###stats 
lmStats <- function() {

  step_table <- learning_phase_table()

  km_res_list   <- kMeans()$features_with_clusters
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  learning_phase <- getLearningPhase()
  learning_phase$participant_id <- as.character(learning_phase$participant_id)
  km_res_list$participant_id    <- as.character(km_res_list$participant_id)
  strategy_data$participant_id  <- as.character(strategy_data$participant_id)
  

  learning_phase <- learning_phase %>%
    left_join(km_res_list[, c("participant_id", "cluster")],
              by = "participant_id")
  learning_phase <- learning_phase %>%
    left_join(strategy_data[, c("participant_id", "rotation")],
              by = "participant_id")
  
  
  #---------------------------------------------------
  # 1. Correlation between step size and aim magnitude when strategy gets stable
  #---------------------------------------------------

  ###BACK TO CORRELATION:
  # since final strategy depends on rotation size, this will also dominate the first relation: the correlation between step size and final strategy
  #SO NORMALIZE
  
  step_data <- learning_phase %>%
    filter(cluster == "step") %>%
    select(participant_id, rotation, strategy_onset_aimdev, stable_strategy_aimdev) %>%
    filter(!is.na(strategy_onset_aimdev) & !is.na(stable_strategy_aimdev)) %>%
    mutate(
      norm_onset = strategy_onset_aimdev / rotation,
      norm_stable = stable_strategy_aimdev / rotation
    )
  
  cor(step_data$norm_onset, step_data$norm_stable, use = "complete.obs")
  
  # multiple regression
  lm_final <- lm(stable_strategy_aimdev ~ strategy_onset_aimdev + rotation, data = step_data)
  summary(lm_final)
  
  
  #Both predictors are highly significant (p < 2e-16), meaning:
  
     #Step size still matters, even when controlling for rotation.
     #Rotation size also independently drives final strategy.
}



#---------------------------------------------------
# 2. Does aim magnitude depend on rotation size for step users?
#---------------------------------------------------
aimStep <- function () {
  
  
  step_table <- learning_phase_table()
  
  km_res_list   <- kMeans()$features_with_clusters
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  learning_phase <- getLearningPhase()
  learning_phase$participant_id <- as.character(learning_phase$participant_id)
  km_res_list$participant_id    <- as.character(km_res_list$participant_id)
  strategy_data$participant_id  <- as.character(strategy_data$participant_id)
  
  
  learning_phase <- learning_phase %>%
    left_join(km_res_list[, c("participant_id", "cluster")],
              by = "participant_id")
  learning_phase <- learning_phase %>%
    left_join(strategy_data[, c("participant_id", "rotation")],
              by = "participant_id")

  step_raw <- learning_phase %>%
    filter(
      cluster == "step",
      !is.na(strategy_onset_trial),
      !is.na(stable_strategy_trial)
    ) %>%
    group_by(participant_id, rotation) %>%
    summarise(
      onset_aimdev  = first(strategy_onset_aimdev),
      stable_aimdev = first(stable_strategy_aimdev),
      .groups = "drop"
    )
  
  aimMagStep <- aov(stable_aimdev ~ factor(rotation), data = step_raw)
  summary(aimMagStep)
  

}

#what about median?
medStep <- function () {
  step_table <- learning_phase_table()
  
  km_res_list   <- kMeans()$features_with_clusters
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  learning_phase <- getLearningPhase()
  learning_phase$participant_id <- as.character(learning_phase$participant_id)
  km_res_list$participant_id    <- as.character(km_res_list$participant_id)
  strategy_data$participant_id  <- as.character(strategy_data$participant_id)
  
  
  learning_phase <- learning_phase %>%
    left_join(km_res_list[, c("participant_id", "cluster")],
              by = "participant_id")
  learning_phase <- learning_phase %>%
    left_join(strategy_data[, c("participant_id", "rotation")],
              by = "participant_id")
  
  step_raw <- learning_phase %>%
    filter(
      cluster == "step",
      !is.na(strategy_onset_trial),
      !is.na(stable_strategy_trial)
    ) %>%
    group_by(participant_id, rotation) %>%
    summarise(
      onset_aimdev  = first(strategy_onset_aimdev),
      stable_aimdev = first(stable_strategy_aimdev),
      .groups = "drop"
    )
median_stable_by_rotation <- step_raw %>%
  group_by(rotation) %>%
  summarise(
    median_stable_aim = median(stable_aimdev, na.rm = TRUE),
    .groups = "drop"
  ) 
median_stable_by_rotation
}


  #---------------------------------------------------
  # 3. Mean time exploring vs rotation size for erratic users
  #---------------------------------------------------
 exploreDesc <- function () { 
   learning_phase <- getLearningPhase()
   
   km_res_list   <- kMeans()$features_with_clusters
   strategy_data <- read.csv("data/strategy_only_participants.csv")
   learning_phase <- getLearningPhase()
   learning_phase$participant_id <- as.character(learning_phase$participant_id)
   km_res_list$participant_id    <- as.character(km_res_list$participant_id)
   strategy_data$participant_id  <- as.character(strategy_data$participant_id)
   
   
   learning_phase <- learning_phase %>%
     left_join(km_res_list[, c("participant_id", "cluster")],
               by = "participant_id")
   learning_phase <- learning_phase %>%
     left_join(strategy_data[, c("participant_id", "rotation")],
               by = "participant_id")
   explore_raw <- learning_phase %>%
    filter(
      cluster == "erratic",
      !is.na(strategy_onset_trial),
      !is.na(stable_strategy_trial)
    ) %>%
    mutate(time_spent_exploring = stable_strategy_trial - strategy_onset_trial) %>%
    group_by(participant_id, rotation) %>%
    summarise(
      time_spent_exploring = first(time_spent_exploring),
      .groups = "drop"
    )
  
  explore_raw_filtered <- explore_raw %>%
    filter(rotation %in% c(50, 60)) %>%
    group_by(rotation) %>%
    summarise(
      N = n(),
      mean_time = mean(time_spent_exploring, na.rm = TRUE),
      median_time = median(time_spent_exploring, na.rm = TRUE),
      sd_time = sd(time_spent_exploring, na.rm = TRUE)
    )
 } 
  #---------------------------------------------------
  # 4. For erratic users, does strategy onset depend on rotation size?
  #---------------------------------------------------

  #more data needed
  
  #---------------------------------------------------
  # 5. All strategy users - time spent until stable strategy
  #---------------------------------------------------
 
  allStableTime <- function () {
    learning_phase <- getLearningPhase()
    
    km_res_list   <- kMeans()$features_with_clusters
    strategy_data <- read.csv("data/strategy_only_participants.csv")
    learning_phase <- getLearningPhase()
    learning_phase$participant_id <- as.character(learning_phase$participant_id)
    km_res_list$participant_id    <- as.character(km_res_list$participant_id)
    strategy_data$participant_id  <- as.character(strategy_data$participant_id)
    
    
    learning_phase <- learning_phase %>%
      left_join(km_res_list[, c("participant_id", "cluster")],
                by = "participant_id")
    learning_phase <- learning_phase %>%
      left_join(strategy_data[, c("participant_id", "rotation")],
                by = "participant_id")
    
   all_strategy_time <- learning_phase %>%
    filter(
      cluster %in% c("step", "erratic", "gradual"),
      !is.na(stable_strategy_trial)
    ) %>%
    group_by(participant_id, cluster, rotation) %>%
    summarise(
      stable_strategy_trial = first(stable_strategy_trial),
      .groups = "drop"
    )
  
    all_strategy_time_kruskal <- kruskal.test(stable_strategy_trial ~ factor(rotation), data = all_strategy_time)
    
    all_strategy_time_kruskal
  }
  
  
  #---------------------------------------------------
  # 6. All strategy users - aim deviation at stable strategy
  #---------------------------------------------------
 
  allStableAim <- function () {
    learning_phase <- getLearningPhase()
    
    km_res_list   <- kMeans()$features_with_clusters
    strategy_data <- read.csv("data/strategy_only_participants.csv")
    learning_phase <- getLearningPhase()
    learning_phase$participant_id <- as.character(learning_phase$participant_id)
    km_res_list$participant_id    <- as.character(km_res_list$participant_id)
    strategy_data$participant_id  <- as.character(strategy_data$participant_id)
    
    
    learning_phase <- learning_phase %>%
      left_join(km_res_list[, c("participant_id", "cluster")],
                by = "participant_id")
    learning_phase <- learning_phase %>%
      left_join(strategy_data[, c("participant_id", "rotation")],
                by = "participant_id")
    all_strategy_aim <- learning_phase %>%
    filter(
      cluster %in% c("step", "erratic", "gradual"),
      !is.na(stable_strategy_trial)
    ) %>%
    group_by(participant_id, cluster, rotation) %>%
    summarise(
      stable_strategy_aimdev = first(stable_strategy_aimdev),
      .groups = "drop"
    )
  
  all_strategy_aim_kruskal <- kruskal.test(stable_strategy_aimdev ~ factor(rotation), data = all_strategy_aim)
  all_strategy_aim_kruskal
  }
  






allFinalBar <- function () { 
  km_res_list <- kMeans()$features_with_clusters
  learning_phase <- getLearningPhase()
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  
  learning_phase$participant_id <- as.character(learning_phase$participant_id)
  km_res_list$participant_id     <- as.character(km_res_list$participant_id)
  strategy_data$participant_id   <- as.character(strategy_data$participant_id)
  
  learning_phase <- learning_phase %>%
    left_join(km_res_list[, c("participant_id", "cluster")],
              by = "participant_id")
  learning_phase <- learning_phase %>%
    left_join(strategy_data[, c("participant_id", "rotation")],
              by = "participant_id")
  
  
   all_strategy_aim <- learning_phase %>%
    filter(
      cluster %in% c("step", "erratic", "gradual"),
      !is.na(stable_strategy_trial)
    ) %>%
    group_by(participant_id, cluster, rotation) %>%
    summarise(
      stable_strategy_aimdev = first(stable_strategy_aimdev),
      .groups = "drop"
    )
  mean_per_rotation <- all_strategy_aim %>%
    group_by(rotation) %>%
    summarise(mean_stable = mean(stable_strategy_aimdev, na.rm = TRUE))
  
  ggplot(all_strategy_aim, aes(x = factor(rotation), y = stable_strategy_aimdev)) +
    geom_bar(data = mean_per_rotation,
             aes(y = mean_stable),
             stat = "identity",
             fill = "#A3C4DC",
             alpha = 0.8) +
    geom_jitter(width = 0.15, height = 0,
                aes(color = cluster),
                size = 2) +
    scale_color_manual(
      name = "Learning Style",
      values = c(
        "step"    = "#e7298a",
        "erratic" = "#7570b3",
        "gradual" = "#d95f02"
      )
    ) +
    labs(x = "Rotation (deg)",
         y = "Final Strategy (Stable Aim)",
         title = "Final Strategy by Rotation Size (All Learners)") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
}





###STACKED BAR PLOT!!
stackedPlot <- function () {
  km_res_list <- kMeans()$features_with_clusters
  learning_phase <- getLearningPhase()
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  
  learning_phase$participant_id <- as.character(learning_phase$participant_id)
  km_res_list$participant_id     <- as.character(km_res_list$participant_id)
  strategy_data$participant_id   <- as.character(strategy_data$participant_id)
  
  learning_phase <- learning_phase %>%
    left_join(km_res_list[, c("participant_id", "cluster")],
              by = "participant_id")
  learning_phase <- learning_phase %>%
    left_join(strategy_data[, c("participant_id", "rotation")],
              by = "participant_id")
  
   strategy_df <- getStrategies()
  all_participants <- strategy_df %>%
  select(participant_id, rotation, strategy)

all_participants <- all_participants %>%
  left_join(
    learning_phase %>% select(participant_id, cluster),
    by = "participant_id"
  )
all_participants <- all_participants %>%
  mutate(style = case_when(
    cluster == "step"           ~ "Stepwise",
    cluster == "gradual"        ~ "Gradual",
    cluster == "erratic"        ~ "Erratic",
    TRUE                        ~ "Non Strategy Users"  # fallback
  ))
style_summary <- all_participants %>%
  group_by(rotation, style) %>%
  summarise(count = n_distinct(participant_id), .groups = "drop") %>%
  group_by(rotation) %>%
  mutate(percent = count / sum(count) * 100)

levels = c("Stepwise", "Erratic","Gradual", "Non Strategy Users")
style_summary$style <- factor(style_summary$style, levels = c("Non Strategy Users",  "Gradual", "Erratic","Stepwise"))

ggplot(style_summary, aes(x = factor(rotation), y = percent, fill = style)) +
  geom_bar(stat = "identity", color = "black") +  # outline
  labs(
    x = "Rotation size (deg)",
    y = "Percentage of participants",
    fill = "Learning style"
  ) +
  scale_fill_manual(
    values = c(
      "Stepwise" = "#e7298a", 
      "Gradual" = "#d95f02", 
      "Erratic" = "#7570b3", 
      "Non Strategy Users" = "white"  # white on top
    ),
    breaks = c("Stepwise", "Gradual", "Erratic")  # removes white from legend
  ) +
  theme_minimal()
}


###STEP USERS FINAL STABLE STRATEGY DENSITY PLOT!!
stepStableDensity <- function () {
km_res_list <- kMeans()$features_with_clusters
learning_phase <- getLearningPhase()
strategy_data <- read.csv("data/strategy_only_participants.csv")

learning_phase$participant_id <- as.character(learning_phase$participant_id)
km_res_list$participant_id     <- as.character(km_res_list$participant_id)
strategy_data$participant_id   <- as.character(strategy_data$participant_id)


learning_phase <- learning_phase %>%
  left_join(km_res_list[, c("participant_id", "cluster")],
            by = "participant_id")

strategy_data <- strategy_data %>%
  rename(strategy_onset_trial = trial_idx)

learning_phase <- learning_phase %>%
  left_join(strategy_data[, c("participant_id", "strategy_onset_trial", "rotation")],
            by = c("participant_id", "strategy_onset_trial"))

explore_raw <- learning_phase %>%
  filter(
    cluster %in% c("step"),  # all strategy learners
    !is.na(stable_strategy_trial)
  ) %>%
  group_by(participant_id, cluster, rotation) %>%
  summarise(
    stable_strategy_aimdev = first(stable_strategy_aimdev),
    .groups = "drop"
  )


explore_raw$rotation <- factor(explore_raw$rotation)
medians <- explore_raw %>%
  group_by(rotation) %>%
  summarise(median_aim = median(stable_strategy_aimdev), .groups = "drop")


rotation_levels <- levels(explore_raw$rotation)
palette <- viridis(length(rotation_levels))
names(palette) <- rotation_levels  # important for mapping


ggplot(explore_raw, aes(x = stable_strategy_aimdev, fill = rotation)) +
  geom_density(alpha = 0.3) +
  geom_vline(data = medians, aes(xintercept = median_aim, color = rotation),
             linetype = "dashed", size = 1, show.legend = FALSE) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) +   # <-- same palette for median lines
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  labs(
    title = "Stable Strategy Aim Deviation (Absolute Degrees)",
    x = "Trial at Step Onset",
    y = "Density",
    fill = "Rotation"
  ) +
  theme_classic(base_size = 14)
}