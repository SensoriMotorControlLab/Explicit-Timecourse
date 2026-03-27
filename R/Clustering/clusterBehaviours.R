## plot averages of each cluster

facetCluster <- function () {
  pca_df <- kPCA()
  model_df <- xgRun()
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  
  model_df_with_clusters <- model_df %>%
    left_join(pca_df[,c("participant_id", "cluster_label")], by = "participant_id")
  
  strategy_data_clustered <- strategy_data %>%
    inner_join(model_df_with_clusters %>% select(participant_id, cluster_label, trial_type.x),
               by = "participant_id")
  
  

  clusterMeans <- strategy_data_clustered %>%
    group_by(participant_id) %>%
    arrange(cutrial_no, .by_group = TRUE) %>%
    filter(
      (trial_type.x.x == "aligned" & row_number() %in% tail(which(trial_type.x.x == "aligned"), 8)) |
        trial_type.x.x == "rotated"
    ) %>%
    mutate(cutrial_no = row_number() - 9) %>%  
    ungroup() %>%
    group_by(cluster_label, cutrial_no) %>%
    summarise(
    mean_aim = mean(aimdeviation_deg, na.rm = TRUE),
  se = sd(aimdeviation_deg, na.rm = TRUE) / sqrt(n()),  # standard error
  ci_lower = mean_aim - 1.96 * se,                      # 95% CI lower
  ci_upper = mean_aim + 1.96 * se,                      # 95% CI upper
  .groups = "drop"
  )
  
  
  cluster_labels <- c("1" = "Gradual", "2" = "Exploratory", "3" = "Stepwise")
  clusterMeans$cluster_label <- recode(clusterMeans$cluster_label,
                                       "1" = "Gradual",
                                       "2" = "Exploratory",
                                       "3" = "Stepwise"
  )
   ggplot(clusterMeans,
         aes(x = cutrial_no, y = mean_aim, group = cluster_label)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = cluster_label), alpha = 0.2) +
    labs(x="Trial Number", y="Mean Aim Deviation")+
    coord_cartesian(ylim=c(-20,40)) +
    geom_line(aes(color = cluster_label), alpha = 0.8, size = 1) +
    scale_color_manual(
      values = c(
        "Gradual"    = "#3dcad4",
        "Exploratory" = "#c495c9",
        "Stepwise"     = "#d16483"
      )
    ) +
    scale_fill_manual(values = c(
      "Gradual" = "#3dcad4",
      "Exploratory" = "#c495c9",
      "Stepwise" = "#d16483"
    )) +
    geom_hline(yintercept = 0, color = "black", size = 0.2) +
    geom_vline(xintercept = 0, color = "black", size = 0.2) +
    facet_wrap(~ cluster_label, ncol = 1, labeller = labeller(cluster_label = cluster_labels)) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(),
      axis.text.x  = element_text(size = 17),
      axis.text.y  = element_text(size = 17),
      axis.title.x = element_text(size = 17),
      axis.title.y = element_text(size = 17),
      legend.title = element_text(size = 17),
      legend.text  = element_text(size = 16)
    ) 
  
}


normalizeClusters <- function () {
  pca_df <- kPCA()
  model_df <- xgRun()
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  
  model_df_with_clusters <- model_df %>%
    left_join(pca_df[,c("participant_id", "cluster_label")], by = "participant_id")
  
  strategy_data_clustered <- strategy_data %>%
    inner_join(model_df_with_clusters %>% select(participant_id, cluster_label, trial_type.x),
               by = "participant_id")
  
clusterMeans <- strategy_data_clustered %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  filter(
    (trial_type.x.x == "aligned" & row_number() %in% tail(which(trial_type.x.x == "aligned"), 8)) |
      trial_type.x.x == "rotated"
  ) %>%
  mutate(
    cutrial_no = row_number() - 9,
    norm_aim = aimdeviation_deg / rotation   
  ) %>%
  ungroup() %>%
  group_by(cluster_label, cutrial_no) %>%
  summarise(
    mean_aim = mean(norm_aim, na.rm = TRUE),
    se = sd(norm_aim, na.rm = TRUE) / sqrt(n()),
    ci_lower = mean_aim - 1.96 * se,
    ci_upper = mean_aim + 1.96 * se,
    .groups = "drop"
  )

cluster_labels <- c("1" = "Gradual", "2" = "Exploratory", "3" = "Stepwise")

p <- ggplot(clusterMeans,
            aes(x = cutrial_no, y = mean_aim,
                group = cluster_label,
                color = cluster_label,
                fill = cluster_label)) +
  
  geom_line(size = 1.2) +
  
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
              alpha = 0.2, colour = NA) +
  
  labs(x = "Trial Number", y = "Size-normalized Aim Deviation") +
  
  scale_color_manual(values = c(
    "1" = "#c495c9",
    "2" = "#3dcad4",
    "3" = "#d16483"
  )) +
  
  scale_fill_manual(values = c(
    "1" = "#c495c9",
    "2" = "#3dcad4",
    "3" = "#d16483"
  )) +
  
  coord_cartesian(ylim = c(0, 1)) +
  
  geom_hline(yintercept = 0, color = "black", size = 0.2) +
  geom_vline(xintercept = 0, color = "black", size = 0.2) +
  
  facet_wrap(~ cluster_label,
             ncol = 1,
             labeller = labeller(cluster_label = cluster_labels)) +
  
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    axis.text.x  = element_text(size = 17),
    axis.text.y  = element_text(size = 17),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17),
    legend.title = element_text(size = 17),
    legend.text  = element_text(size = 16)
  )
print(p)
}


clusterMetrics <- function() {
  
  pca_df   <- kPCA()
  model_df <- xgRun()
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  
  model_df_with_clusters <- model_df %>%
    left_join(pca_df[, c("participant_id", "cluster_label")], by = "participant_id")
  
  strategy_data_clustered <- strategy_data %>%
    inner_join(model_df_with_clusters %>% 
                 select(participant_id, cluster_label, trial_type.x),
               by = "participant_id")
  
  clusterMeans <- strategy_data_clustered %>%
    group_by(participant_id) %>%
    arrange(cutrial_no, .by_group = TRUE) %>%
    filter(
      (trial_type.x.x == "aligned" & row_number() %in% tail(which(trial_type.x.x == "aligned"), 8)) |
        trial_type.x.x == "rotated"
    ) %>%
    mutate(
      cutrial_no = row_number() - 9,
      norm_aim = aimdeviation_deg / abs(rotation)   
    ) %>%
    ungroup() %>%
    group_by(cluster_label, cutrial_no) %>%
    summarise(
      mean_aim = mean(norm_aim, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ── Fit exponential per cluster ──
  fit_one_cluster <- function(df) {
    
    signal <- df %>%
      filter(cutrial_no > 0) %>%
      arrange(cutrial_no) %>%
      pull(mean_aim)
    
    n <- length(signal)
    t_seq <- seq_len(n)
    
    fit <- tryCatch(
      Reach::exponentialFit(
        signal     = signal,
        timepoints = n,
        mode       = "learning"
      ),
      error = function(e) NULL
    )
    
    if (is.null(fit)) {
      return(tibble(
        lambda = NA, asymptote = NA, plateau_trial = NA,
        asymptote_sd = NA, r2 = NA
      ))
    }
    
    lambda <- fit["lambda"]
    N0     <- fit["N0"]
    
    fitted <- N0 * (1 - exp(-lambda * t_seq))
    
    # ── Asymptote (final value) ──
    asymptote <- N0
    
    # ── Plateau trial (95% of asymptote) ──
    plateau_trial <- -log(0.05) / lambda
    
    # ── Variability at asymptote (last 20% trials) ──
    tail_idx <- round(0.8 * n):n
    asymptote_sd <- sd(signal[tail_idx], na.rm = TRUE)
    
    # ── R² ──
    ss_res <- sum((signal - fitted)^2, na.rm = TRUE)
    ss_tot <- sum((signal - mean(signal, na.rm = TRUE))^2, na.rm = TRUE)
    r2 <- 1 - ss_res / ss_tot
    
    tibble(
      lambda = lambda,
      asymptote = asymptote,
      plateau_trial = plateau_trial,
      asymptote_sd = asymptote_sd,
      r2 = round(r2, 3)
    )
  }
  

  cluster_metrics <- clusterMeans %>%
    group_by(cluster_label) %>%
    group_modify(~ fit_one_cluster(.x)) %>%
    ungroup()
  

  cluster_labels <- c("1" = "Gradual", "2" = "Exploratory", "3" = "Stepwise")
  
  cluster_metrics <- cluster_metrics %>%
    mutate(cluster_name = cluster_labels[as.character(cluster_label)]) %>%
    select(cluster_name, everything())
  
  print(cluster_metrics)
  
  return(cluster_metrics)
}


