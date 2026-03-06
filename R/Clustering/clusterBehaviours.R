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
  
  
  cluster_labels <- c("1" = "Stepwise", "2" = "Gradual", "3" = "Exploratory")
  
  ggplot(clusterMeans,
         aes(x = cutrial_no, y = mean_aim, group = cluster_label)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "steelblue", alpha = 0.2) +
    labs(x="Trial Number", y="Mean Aim Deviation")+
    coord_cartesian(ylim=c(-20,40)) +
    geom_line(alpha = 0.5, color = "steelblue", size=1) +
    geom_hline(yintercept = 0, color = "black", size = 0.2) +
    geom_vline(xintercept = 0, color = "black", size = 0.2) +
    facet_wrap(~ cluster_label, ncol = 1, labeller = labeller(cluster_label = cluster_labels)) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = 14, face = "bold"),
      axis.line = element_line(),
      axis.text.x  = element_text(size = 17),
      axis.text.y  = element_text(size = 17),
      axis.title.x = element_text(size = 17),
      axis.title.y = element_text(size = 17),
      legend.title = element_text(size = 17),
      legend.text  = element_text(size = 16)
    ) 
  
}