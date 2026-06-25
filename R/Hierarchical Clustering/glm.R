setUpAIC <- function () {
  
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  
  # IMPORTANT: get cluster output properly
  participant_clusters <- runHClust() %>%
    mutate(cluster_label = case_when(
      cluster_label == 3 ~ "One-Step",
      cluster_label == 2 ~ "Multi-Step",
      cluster_label == 1 ~ "Exploratory"
    ))
  
  # join clusters to strategy data
  strategy_data_clustered <- strategy_data %>%
    inner_join(
      participant_clusters %>% dplyr::select(participant_id, cluster_label),
      by = "participant_id"
    )
  
  # strategy group counts (participant-level, not row-level)
  proportions_df <- strategy_data_clustered %>%
    distinct(participant_id, rotation, cluster_label) %>%
    group_by(rotation, cluster_label) %>%
    summarise(n_participants = n(), .groups = "drop")
  
  # non-strategy participants
  strategy_df <- getStrategies()
  
  total_per_rotation <- strategy_df %>%
    group_by(rotation) %>%
    summarise(total_n = n_distinct(participant_id), .groups = "drop")
  
  non_strategy <- strategy_df %>%
    filter(strategy == "No") %>%
    group_by(rotation) %>%
    summarise(n_participants = n_distinct(participant_id), .groups = "drop") %>%
    left_join(total_per_rotation, by = "rotation") %>%
    mutate(
      cluster_label = "Non-strategy",
      prop = n_participants / total_n
    )
  
  # strategy proportions
  proportions_df <- proportions_df %>%
    group_by(rotation) %>%
    mutate(prop = n_participants / sum(n_participants)) %>%
    ungroup()
  
  # combine
  proportions_combined <- bind_rows(proportions_df, non_strategy) %>%
    filter(!is.na(cluster_label))
  
  # FIXED factor ordering
  proportions_combined$cluster_label <- factor(
    proportions_combined$cluster_label,
    levels = c(
      "Non-strategy",
      "Exploratory",
      "One-Step",
      "Multi-Step"
    )
  )
  
  return(proportions_combined)
}

### tests

NonAIC <- function () {
  proportions_combined <- setUpAIC()
  strategy_df <- getStrategies()
  dat2 <- proportions_combined %>%
    group_by(rotation) %>%
    summarise(
      non_strategy = sum(n_participants[cluster_label == "Non-strategy"]),
      strategy = sum(n_participants[cluster_label != "Non-strategy"]),
      total = sum(n_participants)
    )
  
  m0 <- glm(cbind(strategy, non_strategy) ~ 1,
            family = binomial,
            data = dat2)
  
  m1 <- glm(cbind(strategy, non_strategy) ~ rotation,
            family = binomial,
            data = dat2)
  
  print(stats::AIC(m0, m1))
  
  deltaAIC <- stats::AIC(m0) - stats::AIC(m1)
  return(deltaAIC)
}

####
make_MUmodel <- function(){
  library(nnet)
  proportions_combined <- setUpAIC()
  clean_dat <- proportions_combined %>% 
    group_by(rotation, cluster_label) %>% 
    summarise(n_participants = sum(n_participants), .groups = "drop") %>%
    filter(cluster_label != "Non-strategy")
  
  
  
  m0 <- multinom(cluster_label ~ 1, weights = n_participants, data = clean_dat) 
  #m0 (null model): cluster distribution is the same across all rotation groups
  m1 <- multinom(cluster_label ~ rotation, weights = n_participants, data = clean_dat) 
  #  m1 (full model): cluster distribution depends on rotation
  stats::AIC(m0, m1) 
  
  deltaAIC <- stats::AIC(m0) - stats::AIC(m1) 
  deltaAIC
}

make_model <- function(cluster_name) {
  proportions_combined <- setUpAIC()
  dat <- proportions_combined %>%
    
    filter(cluster_label != "Non-strategy") %>% 
    group_by(rotation) %>%
    summarise(
      success = sum(n_participants[cluster_label == cluster_name]),
      failure = sum(n_participants[cluster_label != cluster_name]),
      .groups = "drop"
    )
  
  
  m0 <- glm(cbind(success, failure) ~ 1,
            family = binomial,
            data = dat)
  
  # 3. Run the Alternative Model (Rotation affects cluster probability)
  m1 <- glm(cbind(success, failure) ~ rotation,
            family = binomial,
            data = dat)
  
  
  return(c(
    cluster = cluster_name,
    deltaAIC = stats::AIC(m0) - stats::AIC(m1)
  ))
}

clusters_to_test <- setdiff(unique(proportions_combined$cluster_label), "Non-strategy")
results <- do.call(rbind, lapply(clusters_to_test, make_model))
