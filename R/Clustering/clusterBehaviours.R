## plot averages of each cluster


normalizeClusters <- function () {
  pca_df <- kPCA()
  model_df <- xgRun()
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  
  
  model_df_with_clusters <- model_df %>%
    left_join(
      pca_df %>% select(participant_id, cluster_label),
      by = "participant_id"
    )
  
  strategy_data_clustered <- strategy_data %>%
    inner_join(
      model_df_with_clusters %>% 
        select(participant_id, cluster_label),
      by = "participant_id"
    )
  
clusterMeans <- strategy_data_clustered %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  filter(
    (trial_type == "aligned" & row_number() %in% tail(which(trial_type == "aligned"), 8)) |
      trial_type == "rotated"
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

cluster_labels <- c("3" = "Gradual", "1" = "Exploratory", "2" = "Stepwise")

p <- ggplot(clusterMeans,
            aes(x = cutrial_no, y = mean_aim,
                group = cluster_label,
                color = cluster_label,
                fill = cluster_label)) +
  
  geom_line(size = 1.2) +
  
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
              alpha = 0.2, colour = NA) +
  
  labs(x = "Trial Number", y = "Rotation-normalized Aim Deviation") +
  
  scale_color_manual(values = c(
    "2" = "#a2bffe",
    "3" = "#e89c7b",
    "1" = "hotpink"
  )) +
  
  scale_fill_manual(values = c(
    "2" = "#a2bffe",
    "3" = "#e89c7b",
    "1" = "hotpink"
  )) +
  
  coord_cartesian(ylim = c(-0.2, 1), xlim=c(-2,24)) +
  
  geom_hline(yintercept = 0, color = "black", linetype="dashed", size = 0.2) +
  geom_vline(xintercept = 0, color = "black",linetype="dashed", size = 0.2) +
  
  facet_wrap(~ cluster_label,
             nrow= 1,
             labeller = labeller(cluster_label = cluster_labels)) +
  
  theme_classic() +
  theme(
  legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(size = 10, face = "bold"),
    # axis.text.x  = element_text(size = 17),
    # axis.text.y  = element_text(size = 17),
    # axis.title.x = element_text(size = 17),
    # axis.title.y = element_text(size = 17),
    # legend.title = element_text(size = 17),
    # legend.text  = element_text(size = 16)
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
    inner_join(
      model_df_with_clusters %>% 
        select(participant_id, cluster_label),
      by = "participant_id"
    )
  
  clusterMeans <- strategy_data_clustered %>%
    group_by(participant_id) %>%
    arrange(cutrial_no, .by_group = TRUE) %>%
    filter(
      (trial_type == "aligned" & row_number() %in% tail(which(trial_type == "aligned"), 8)) |
        trial_type == "rotated"
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
  

  cluster_labels <- c("1" = "Exploratory", "2" = "Stepwise", "3" = "Gradual")
  
  cluster_metrics <- cluster_metrics %>%
    mutate(cluster_name = cluster_labels[as.character(cluster_label)]) %>%
    select(cluster_name, everything())
  
  print(cluster_metrics)

  
  return(cluster_metrics)
}



#participant level ANOVAs

# clusterMetrics_individual <- function() {
#   
#   pca_df   <- kPCA()
#   model_df <- xgRun()
#   strategy_data <- read.csv("data/strategy_only_participants.csv")
#   
#   model_df_with_clusters <- model_df %>%
#     left_join(pca_df[, c("participant_id", "cluster_label")], by = "participant_id")
#   
#   strategy_data_clustered <- strategy_data %>%
#     inner_join(
#       model_df_with_clusters %>% 
#         select(participant_id, cluster_label),
#       by = "participant_id"
#     )
#   
#   trial_data <- strategy_data_clustered %>%
#     group_by(participant_id) %>%
#     arrange(cutrial_no, .by_group = TRUE) %>%
#     filter(
#       (trial_type == "aligned" & row_number() %in% tail(which(trial_type == "aligned"), 8)) |
#         trial_type == "rotated"
#     ) %>%
#     mutate(
#       cutrial_no = row_number() - 9,
#       norm_aim = aimdeviation_deg / abs(rotation)
#     ) %>%
#     filter(cutrial_no > 0) %>%
#     ungroup()
#   
#   # ── Fit per participant ──
#   fit_one <- function(df) {
#     
#     signal <- df$norm_aim
#     n <- length(signal)
#     t_seq <- seq_len(n)
#     
#     fit <- tryCatch(
#       Reach::exponentialFit(
#         signal = signal,
#         timepoints = n,
#         mode = "learning"
#       ),
#       error = function(e) NULL
#     )
#     
#     if (is.null(fit)) {
#       return(tibble(
#         plateau_trial = NA,
#         asymptote_sd = NA
#       ))
#     }
#     
#     lambda <- fit["lambda"]
#     N0     <- fit["N0"]
#     
#     # Plateau (95%)
#     plateau_trial <- -log(0.05) / lambda
#     
#     # Variability (last 20%)
#     tail_idx <- round(0.8 * n):n
#     asymptote_sd <- sd(signal[tail_idx], na.rm = TRUE)
#     
#     tibble(
#       plateau_trial = plateau_trial,
#       asymptote_sd = asymptote_sd,
#       lambda = lambda
#     )
#   }
#   
#   participant_metrics <- trial_data %>%
#     group_by(participant_id, cluster_label) %>%
#     group_modify(~ fit_one(.x)) %>%
#     ungroup()
#   
#   return(participant_metrics)
# }
# 
# 
# metrics <- clusterMetrics_individual()
# 
# # Plateau ANOVA
# anova_plateau <- aov(lambda ~ factor(cluster_label), data = metrics)
# summary(anova_plateau)
# 
# # Variance ANOVA
# anova_var <- aov(asymptote_sd ~ factor(cluster_label), data = metrics)
# summary(anova_var)



