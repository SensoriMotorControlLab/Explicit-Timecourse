setUpAIC <- function () {
  
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  

  participant_clusters <- runHClust() %>%
    mutate(cluster_label = case_when(
      cluster_label == 2 ~ "Incremental",
      cluster_label == 3 ~ "Exploratory",
      cluster_label == 1 ~ "One-Step"
    ))
  

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
  

  proportions_df <- proportions_df %>%
    group_by(rotation) %>%
    mutate(prop = n_participants / sum(n_participants)) %>%
    ungroup()
  

  proportions_combined <- bind_rows(proportions_df, non_strategy) %>%
    filter(!is.na(cluster_label))
  

  proportions_combined$cluster_label <- factor(
    proportions_combined$cluster_label,
    levels = c(
      "Non-strategy",
      "Exploratory",
      "One-Step",
      "Incremental"
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


library(dplyr)
library(tidyr)
library(nnet)

# --------------------------------------------------
# SET UP DATA
# --------------------------------------------------

setUpAIC <- function() {
  
  strategy_data <- read.csv(
    "data/strategy_only_participants.csv"
  )
  
  # Run clustering once
  participant_clusters <- runHClust() %>%
    mutate(
      cluster_label = case_when(
        as.character(cluster_label) == "1" ~ "One-Step",
        as.character(cluster_label) == "2" ~ "Incremental",
        as.character(cluster_label) == "3" ~ "Exploratory",
        TRUE ~ NA_character_
      )
    ) %>%
    select(participant_id, cluster_label) %>%
    distinct(participant_id, .keep_all = TRUE)
  
  # Assign phenotypes to strategy users
  strategy_data_clustered <- strategy_data %>%
    inner_join(
      participant_clusters,
      by = "participant_id"
    )
  
  # Count strategy phenotypes
  proportions_df <- strategy_data_clustered %>%
    distinct(participant_id, rotation, cluster_label) %>%
    group_by(rotation, cluster_label) %>%
    summarise(
      n_participants = n(),
      .groups = "drop"
    )
  
  # All participants
  strategy_df <- getStrategies()
  
  total_per_rotation <- strategy_df %>%
    group_by(rotation) %>%
    summarise(
      total_n = n_distinct(participant_id),
      .groups = "drop"
    )
  
  # Non-strategy participants
  non_strategy <- strategy_df %>%
    filter(strategy == "No") %>%
    group_by(rotation) %>%
    summarise(
      n_participants = n_distinct(participant_id),
      .groups = "drop"
    ) %>%
    mutate(
      cluster_label = "Non-strategy"
    )
  
  # Combine all categories
  proportions_combined <- bind_rows(
    proportions_df,
    non_strategy
  ) %>%
    left_join(
      total_per_rotation,
      by = "rotation"
    ) %>%
    mutate(
      prop = n_participants / total_n,
      cluster_label = factor(
        cluster_label,
        levels = c(
          "Non-strategy",
          "Exploratory",
          "One-Step",
          "Incremental"
        )
      )
    ) %>%
    filter(!is.na(cluster_label))
  
  # Check that all participants are accounted for
  coverage <- proportions_combined %>%
    group_by(rotation) %>%
    summarise(
      counted = sum(n_participants),
      total = first(total_n),
      .groups = "drop"
    )
  
  print(coverage)
  
  if (any(coverage$counted != coverage$total)) {
    warning(
      "Some rotation groups have missing or duplicated classifications."
    )
  }
  
  return(proportions_combined)
}


# --------------------------------------------------
# CREATE DATA ONCE
# --------------------------------------------------

proportions_combined <- setUpAIC()


# --------------------------------------------------
# TEST 1: STRATEGY VS NON-STRATEGY
# --------------------------------------------------

NonAIC <- function(data = proportions_combined) {
  
  dat <- data %>%
    group_by(rotation) %>%
    summarise(
      non_strategy = sum(
        n_participants[
          cluster_label == "Non-strategy"
        ]
      ),
      strategy = sum(
        n_participants[
          cluster_label != "Non-strategy"
        ]
      ),
      .groups = "drop"
    )
  
  m0 <- glm(
    cbind(strategy, non_strategy) ~ 1,
    family = binomial,
    data = dat
  )
  
  m1 <- glm(
    cbind(strategy, non_strategy) ~ rotation,
    family = binomial,
    data = dat
  )
  
  print(stats::AIC(m0, m1))
  
  return(
    stats::AIC(m0) - stats::AIC(m1)
  )
}


# --------------------------------------------------
# TEST 2: MULTINOMIAL PHENOTYPE MODEL
# Strategy users only
# --------------------------------------------------

make_MUmodel <- function(data = proportions_combined) {
  
  clean_dat <- data %>%
    filter(cluster_label != "Non-strategy") %>%
    mutate(
      cluster_label = droplevels(cluster_label)
    ) %>%
    group_by(rotation, cluster_label) %>%
    summarise(
      n_participants = sum(n_participants),
      .groups = "drop"
    )
  
  m0 <- multinom(
    cluster_label ~ 1,
    weights = n_participants,
    data = clean_dat,
    trace = FALSE
  )
  
  m1 <- multinom(
    cluster_label ~ rotation,
    weights = n_participants,
    data = clean_dat,
    trace = FALSE
  )
  
  print(stats::AIC(m0, m1))
  
  return(
    stats::AIC(m0) - stats::AIC(m1)
  )
}


# --------------------------------------------------
# TEST 3: INDIVIDUAL PHENOTYPE MODELS
# Each phenotype vs everyone else
# --------------------------------------------------

make_model <- function(
    cluster_name,
    data = proportions_combined
) {
  
  dat <- data %>%
    group_by(rotation) %>%
    summarise(
      success = sum(
        n_participants[
          cluster_label == cluster_name
        ]
      ),
      total = first(total_n),
      .groups = "drop"
    ) %>%
    mutate(
      failure = total - success
    )
  
  m0 <- glm(
    cbind(success, failure) ~ 1,
    family = binomial,
    data = dat
  )
  
  m1 <- glm(
    cbind(success, failure) ~ rotation,
    family = binomial,
    data = dat
  )
  
  return(
    data.frame(
      cluster = cluster_name,
      deltaAIC = stats::AIC(m0) - stats::AIC(m1)
    )
  )
}


# --------------------------------------------------
# RUN ALL TESTS
# --------------------------------------------------

# Strategy adoption
NonAIC()

# Overall phenotype distribution among strategy users
make_MUmodel()

# Individual phenotypes vs all other participants
clusters_to_test <- c(
  "Exploratory",
  "Incremental",
  "One-Step"
)

results <- bind_rows(
  lapply(
    clusters_to_test,
    make_model,
    data = proportions_combined
  )
)

print(results)
