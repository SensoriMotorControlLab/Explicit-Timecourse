runHClust <- function () {
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  extract <- getFeatures() 
  features_z <- extract$features_z
  
  k_input <- extract$input
  
  pca <- prcomp(k_input, center = FALSE, scale. = FALSE)

  
  pca_df <- as.data.frame(pca$x[,1:3])
  colnames(pca_df) <- c("PC1","PC2","PC3")
  pca_df$participant_id <- features_z$participant_id
  
  rotation_df <- strategy_data %>%
    group_by(participant_id) %>%
    summarise(
      rotation = first(rotation),
      .groups = "drop"
    )
  
  pca_df <- pca_df %>%
    left_join(rotation_df, by = "participant_id")
  
  
  distance_matrix <- dist(pca_df[, 1:3], method = "euclidean")
  hc <- hclust(distance_matrix, method = "ward.D2")
  pca_df$cluster_label <- as.factor(cutree(hc, k = 3))
  
  return(pca_df)
}

silAnalys <- function () {
  pca_df <- runHClust()
  
  distance_matrix <- dist(pca_df[, 1:3], method = "euclidean")
  hc <- hclust(distance_matrix, method = "ward.D2")
  
  k_values <- 2:10
  
  sil_width <- sapply(k_values, function(k) {
    
    clusters_k <- cutree(hc, k = k)
    
    sil <- silhouette(
      clusters_k,
      distance_matrix
    )
    
    mean(sil[, "sil_width"])
  })
  
  sil_results <- data.frame(
    k = k_values,
    silhouette = sil_width
  )
  
  print(sil_results)
  
}
dendrogram_plot <- function() {
  # 1. Get data and build hierarchical tree
  pca_df <- runHClust()
  distance_matrix <- dist(pca_df[, 1:3], method = "euclidean")
  hc <- hclust(distance_matrix, method = "ward.D2")
  
  library(dendextend)
  dend <- as.dendrogram(hc)
  

  cluster_colors <- c(
    "1" = "orchid",
    "2" = "cyan",
    "3" = "#EAA178"
  )
  

  clusters_ordered <- cutree(hc, k = 3)[order.dendrogram(dend)]
  

  dend <- color_branches(
    dend, 
    clusters = clusters_ordered, 
    col = cluster_colors[as.character(unique(clusters_ordered))]
  )
  
  plot(
    dend,
    main = "",
    ylab = "Height",
    cex.lab = 1.3,
    cex.axis = 1.1,
    leaflab = "none"
  )
  
  abline(h = 10, col = "grey", lty = 2)
}

pcaPlot <- function () {
  pca_df <- runHClust() 
  
  pca_df <- pca_df %>%
    mutate(cluster_label = case_when(
      cluster_label == "2" ~ "Incremental",
      cluster_label == "3" ~ "Exploratory",
      cluster_label == "1" ~ "One-Step"
    ))
  
  
  
  ggplot(pca_df, aes(PC1, PC2, color = cluster_label, fill = cluster_label)) +
  

    
    geom_point(size = 2.0) + 
    theme_classic() + 
    
    scale_color_manual(values = c(
      "One-Step"  = "orchid",
      "Incremental"     = "cyan",
      "Exploratory"   = "#EAA178"
    )) +
    scale_fill_manual(values = c(
      "One-Step"  = "orchid",
      "Incremental"     = "cyan",
      "Exploratory"   = "#EAA178"
    )) +
    coord_cartesian(ylim=c(-5,5)) +
    guides(
      fill = "none", 
      color = guide_legend(override.aes = list(
        alpha = 1, 
        size = 4, 
        linetype = 0 
      ))
    ) +
    coord_fixed(ratio = 1, ylim = c(-5, 5)) +
    labs(color = "Cluster", title = "") +
    
    theme(
      legend.key = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey90"),
      axis.line = element_line(color = "black"),
      axis.title = element_text(size = 14),  
      axis.text = element_text(size = 14, color = "black"),
      legend.title = element_text(size = 14), 
      legend.text = element_text(size = 14)    
    )
  
}

#   
# p <- ggplot(pca_df, aes(
#   PC1, PC2,
#   color = cluster_label,
#   text = participant_id
# )) +
# 
#   geom_point(size = 2.0) +
#   theme_classic() +
# 
#   guides(
#     fill = "none",
#     color = guide_legend(override.aes = list(size = 4))
#   ) +
# 
#   labs(color = "Phenotype", title = "")
# 
# ggplotly(p, tooltip = "text")

stackedPlot <- function () {
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  pca_df <- runHClust() 
  pca_df <- pca_df %>%
    mutate(cluster_label = case_when(
      cluster_label == "2" ~ "Incremental",
      cluster_label == "3" ~ "Exploratory",
      cluster_label == "1" ~ "One-Step"
    ))
  
  unique_participants <- strategy_data %>%
  distinct(participant_id)
  cluster_lookup <- pca_df %>%
  dplyr::select(participant_id, cluster_label)


  strategy_data_clustered <- strategy_data %>%
  inner_join(cluster_lookup, by = "participant_id")


  strategy_counts <- strategy_data_clustered %>%
  group_by(rotation, cluster_label) %>%
  summarise(
    n_participants = n_distinct(participant_id),
    .groups = "drop"
  )

  #now add in non startegy ppl
  strategy_df <- getStrategies()

  non_strategy_counts <- strategy_df %>%
  filter(strategy == "No") %>%
  group_by(rotation) %>%
  summarise(
    n_participants = n_distinct(participant_id),
    .groups = "drop"
  ) %>%
  mutate(cluster_label = "Non-strategy")


  final_plot_df <- bind_rows(
  strategy_counts,
  non_strategy_counts
  ) %>%
  group_by(rotation) %>%
  mutate(
    percent = 100 * n_participants / sum(n_participants)
  ) %>%
  ungroup()


  final_plot_df$cluster_label <- factor(
  final_plot_df$cluster_label,
  levels = c(
    "Non-strategy",
    "One-Step",
    "Incremental",
    "Exploratory"
  
  )
  )

# Plot
ggplot(
  final_plot_df,
  aes(
    x = factor(rotation),
    y = percent,
    fill = cluster_label
  )
) +
  geom_bar(
    stat = "identity",
    color = "black",
    width = 0.7
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, "%")
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_fill_manual(
    values = c(
      "Non-strategy" = "white",
      "One-Step"  = "orchid",
      "Incremental"     = "cyan",
      "Exploratory"   = "#EAA178"
     
      
    )
  ) +
  theme_classic() +
  labs(
    x = "Rotation (Degrees)",
    y = "Percentage of Participants",
    fill = "Category"
  )
}




##

plotRot <- function () {
  pca_df <- runHClust() 
  rotation_summary <- pca_df %>%
  group_by(rotation) %>%
  summarise(
    mean_PC1 = mean(PC1, na.rm = TRUE),
    sd_PC1   = sd(PC1, na.rm = TRUE),
    mean_PC2 = mean(PC2, na.rm = TRUE),
    sd_PC2   = sd(PC2, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(pca_df, aes(x = PC1, y = PC2)) +
  
  geom_point(
    color = "grey70",
    alpha = 0.5,
    size = 2
  ) +
  

  geom_segment(
    data = rotation_summary,
    aes(
      x = mean_PC1 - sd_PC1,
      xend = mean_PC1 + sd_PC1,
      y = mean_PC2,
      yend = mean_PC2,
      color = factor(rotation)
    ),
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  
  coord_fixed(ratio = 1) +
  geom_segment(
    data = rotation_summary,
    aes(
      x = mean_PC1,
      xend = mean_PC1,
      y = mean_PC2 - sd_PC2,
      yend = mean_PC2 + sd_PC2,
      color = factor(rotation)
    ),
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  
  geom_point(
    data = rotation_summary,
    aes(x = mean_PC1, y = mean_PC2, color = factor(rotation)),
    shape = 3,
    size = 7,
    stroke = 2,
    inherit.aes = FALSE
  ) +
  coord_cartesian(
    xlim = c(-5, 5),
    ylim = c(-5, 5)
  ) +

  geom_text(
    data = rotation_summary,
    aes(x = mean_PC1, y = mean_PC2, label = rotation),
    nudge_y = 0.15,
    inherit.aes = FALSE
  ) +

  # 5 distinct colors for rotation
  scale_color_manual(
    values = c(
      "20" = "#4cc9f0",
      "30" = "#4895ef",
      "40" = "#4261ee",
      "50" = "#2835af",
      "60" = "#12086f"
    ),
    name = "Rotation"
  ) +
  theme_classic() +
  labs(
    x = "PC1",
    y = "PC2") +
  theme(axis.line = element_line(color = "black"),
axis.title = element_text(size = 14),  
axis.text = element_text(size = 14, color = "black"),
legend.title = element_text(size = 14), 
legend.text = element_text(size = 14))   
}

lmRot <- function() {
  pca_df <- runHClust()
  
  print("=== GROUP DESCRIPTIVE STATISTICS ===")
  rotation_summary <- pca_df %>%
    group_by(rotation) %>%
    summarise(
      mean_PC1 = mean(PC1, na.rm = TRUE),
      sd_PC1   = sd(PC1, na.rm = TRUE),
      mean_PC2 = mean(PC2, na.rm = TRUE),
      sd_PC2   = sd(PC2, na.rm = TRUE),
      mean_PC3 = mean(PC3, na.rm = TRUE),
      sd_PC3   = sd(PC3, na.rm = TRUE),
      .groups = "drop"
    )
  print(rotation_summary)
  cat("\n") 
  
  print("=== LINEAR MODEL: PC1 vs ROTATION ===")
  model1 <- lm(PC1 ~ rotation, data = pca_df)
  print(summary(model1))
  cat("\n--------------------------------------------------\n\n")
  
  print("=== LINEAR MODEL: PC2 vs ROTATION ===")
  model2 <- lm(PC2 ~ rotation, data = pca_df)
  print(summary(model2))
  cat("\n--------------------------------------------------\n\n")
  
  print("=== LINEAR MODEL: PC3 vs ROTATION ===")
  model3 <- lm(PC3 ~ rotation, data = pca_df)
  print(summary(model3))
  
  r1 <- cor(pca_df$rotation, pca_df$PC1, use = "complete.obs")
  r2 <- cor(pca_df$rotation, pca_df$PC2, use = "complete.obs")
  r3 <- cor(pca_df$rotation, pca_df$PC3, use = "complete.obs")
  
  cat("r (PC1 ~ rotation):", round(r1, 3), "\n")
  cat("r (PC2 ~ rotation):", round(r2, 3), "\n")
  cat("r (PC3 ~ rotation):", round(r3, 3), "\n\n")
}


lmPlot <- function () {
  pca_df <- runHClust()
  ggplot(pca_df, aes(x = rotation, y = PC3)) +
  
  geom_point(
    aes(color = factor(rotation)),
    size = 2,
    alpha = 0.8
  ) +
  
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#f94449",
    fill = "grey70",
    alpha = 0.2
  ) +
  
  scale_color_manual(
    values = c(
      "20" = "#4cc9f0",
      "30" = "#4895ef",
      "40" = "#4261ee",
      "50" = "#2835af",
      "60" = "#12086f"
    ),
    name = "Rotation"
  ) +
  
  theme_classic() +
  labs(x = "Rotation", y = "PC3") +
  theme(axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14),  
        axis.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 14))   
}




##determine k number

library(NbClust)

clusterNum <- function () {
  pca_df <- runHClust()
  nb_result <- NbClust(
  data = pca_df[, c("PC1", "PC2", "PC3")], 
  distance = "euclidean", 
  min.nc = 2, 
  max.nc = 10, 
  method = "ward.D2", 
  index = "all" # Evaluates ~30 indices (or specify a single one like "gap" or "silhouette")
  )


  print(nb_result$Best.nc)
  votes <- as.data.frame(
  table(nb_result$Best.nc["Number_clusters", ])
  )

  colnames(votes) <- c("k", "votes")

  votes$k <- as.numeric(as.character(votes$k))
  votes <- votes[votes$k > 0, ]
  votes <- votes[order(-votes$votes), ]

  print(votes)


  best_k <- votes$k[which.max(votes$votes)]

  cat("Most recommended number of clusters:", best_k, "\n")
  cat("Number of votes:", max(votes$votes), "\n")
}



plot_pca_triplot <- function() {
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  pca_df <- runHClust() 

  make_pca_plot <- function(x_var, y_var) {
    ggplot(pca_df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
      geom_point(size = 2.0, color = "#B8B0E8") + 
      theme_classic() + 
      coord_equal(xlim = c(-5, 5), ylim = c(-5, 5)) + 
      theme(
        legend.position = "None",
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14),  
        axis.text = element_text(size = 14, color = "black")   
      )
  }
  

  p1 <- make_pca_plot("PC1", "PC2")
  p2 <- make_pca_plot("PC1", "PC3")
  p3 <- make_pca_plot("PC2", "PC3")

  combined_plot <- p1 + p2 + p3 + plot_layout(ncol = 3)
  
  return(combined_plot)
}

