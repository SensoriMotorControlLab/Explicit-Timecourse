#kmeans and xgboost classified learning phase
getFeaturesFromModel <- function() {
  model_df <- xgRun()
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  window_size = 3
  
  
  features <- data.frame(
    participant_id = character(),
    learning_sd = numeric(),
    learning_length = numeric(),
    learning_abs_diff = numeric(),
    num_negative_aims = numeric(),
    num_sign_flips = numeric(),
    diff_sd = numeric(), 
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(model_df)) {
    
    id <- model_df$participant_id[i]
    onset  <- model_df$trial.start[i]
    stable <- model_df$trial.end[i]
    
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
    num_negative_aims <- sum(learning_aim < 0)
    diffs             <- diff(learning_aim)
    num_sign_flips    <- sum(diff(sign(diffs)) != 0)
    

    # Smoothness feature via rolling SD
    if(length(learning_aim) >= window_size){
      rolling_sd <- rollapply(learning_aim, width = window_size, FUN = sd, fill = NA, align = "right")
      smoothness <- mean(rolling_sd, na.rm = TRUE)
    } else {
      smoothness <- 0  # assign 0 if too few trials
    }
    
    # Erraticness feature: SD of trial-to-trial changes
    diff_sd <- sd(diffs)
    
  
    learning_sd       <- ifelse(is.na(learning_sd), 0, learning_sd)
    learning_abs_diff <- ifelse(is.na(learning_abs_diff), 0, learning_abs_diff)
    num_negative_aims <- ifelse(is.na(num_negative_aims), 0, num_negative_aims)
    num_sign_flips    <- ifelse(is.na(num_sign_flips), 0, num_sign_flips)
    smoothness        <- ifelse(is.na(smoothness), 0, smoothness)
    diff_sd           <- ifelse(is.na(diff_sd), 0, diff_sd)
    
   
    features <- rbind(
      features,
      data.frame(
        participant_id = id,
        learning_sd = learning_sd,
        learning_length = learning_length,
        learning_abs_diff = learning_abs_diff,
        num_negative_aims = num_negative_aims,
        num_sign_flips = num_sign_flips,
        diff_sd = diff_sd,
        stringsAsFactors = FALSE
      )
    )
  }
  

  numeric_cols <- c("learning_sd","learning_length","learning_abs_diff",
                    "num_negative_aims","num_sign_flips","diff_sd")
  
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

pca_df <- as.data.frame(pca$x[,1:2])
colnames(pca_df) <- c("PC1","PC2")
pca_df$participant_id <- features_df$participant_id

# -------------------------------
# 3. K-means clustering
set.seed(123)
km_res <- kmeans(pca_df[,c("PC1","PC2")], centers = 3, nstart = 50)

pca_df$cluster <- km_res$cluster
pca_df$cluster_label <- as.factor(km_res$cluster)
return(pca_df)
}


#which components? - plot pca
              #pca$rotation
# Cluster separation along x-axis (PC1) mostly reflects stability vs erratic learning.
# Separation along y-axis (PC2) mostly reflects short vs long learning periods.

plotComponents <- function () {
  
  pca_df <- kPCA()
  
  ggplot(pca_df,
         aes(x = PC1, y = PC2, color = cluster_label, label = participant_id)) +
    geom_point(size = 3, alpha = 0.8) +
    geom_text(vjust = -1, size = 2.5, check_overlap = TRUE) +
    labs(
      x = "Principal Component 1",
      y = "Principal Component 2",
      color = "Cluster",
      title = "PCA of Learning Features with K-means Clusters"
    ) +
    theme_minimal(base_size = 14) +
    scale_color_brewer(palette = "Set1")
}


##plot 
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
                     "bdb042","d0d19c","f275ca", "a02c67", "396716", "15f2a1"),
  
  label = c("step", "step", "step", "step", "step", 
            "step", "step", "erratic", "step", "step", "erratic", "step",
            "gradual", "step", "step", "step", "step", "erratic", "gradual", "gradual",
            "gradual", "erratic", "gradual", "gradual", "step", "step", "gradual","step", "step", 
            "erratic", "erratic","erratic",  "erratic", "erratic", "step", "step", "gradual", "step",
            "step","erratic", "step", "step", "step",  "step", "step",
            "step", "step", "erratic", "step", "step", "step", "erratic",
            "erratic", "gradual", "erratic", "step", "step",
            "step",  "step","gradual", "erratic", "step", "step", "gradual",
            "erratic", "step","erratic", "gradual", "erratic", "erratic", "erratic", "step")
)


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

force_no_ids <- c("15f2a1", "19187c", "396716", "3091de", "654648")
strategy_df$strategy[strategy_df$participant_id %in% force_no_ids] <- "No"

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
  levels = c("Non-strategy", "1", "2", "3")
)

ggplot(proportions_combined, aes(x = factor(rotation), y = n_participants, fill = cluster_label)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_fill_manual(
    values = c("1" = "hotpink", "2" = "#d95f02", "3" = "#7570b3", "Non-strategy" = "white"),
    name = "Cluster / Non-strategy"
  ) +
  labs(
    x = "Rotation Size (degrees)",
    y = "Number of Participants",
    title = "Distribution of Learning Clusters Across Rotation Sizes"
  ) +
  theme_minimal(base_size = 14)
}



