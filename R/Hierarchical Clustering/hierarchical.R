
runHClust <- function () {
  model_df <- read.csv("data/LearningClassifier.csv") #for pre-xG boost labels
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  extract <- getFeatures() 
  features_z <- extract$features_z

  k_input <- extract$kmeans_input
  
  pca <- prcomp(k_input, center = FALSE, scale. = FALSE)
  pca$x <- pca$x*-1
  pca$rotation <- pca$rotation*-1


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

dendogram <- function () {
 pca_df <- runHClust()
 
 distance_matrix <- dist(pca_df[, 1:3], method = "euclidean")
 hc <- hclust(distance_matrix, method = "ward.D2")
   library(dendextend)
  
  dend <- as.dendrogram(hc)
  
  dend <- color_branches(
    dend,
    h = 10,
    col = c("#EAA178", "cyan", "orchid")
  )
  
  # remove participant_id labels
  labels(dend) <- rep("", length(labels(dend)))
  
  plot(
    dend,
    main = "",
    ylab = "Height",
    leaflab = "none"
  )
  abline(h = 10, col = "grey", lty = 2)
  
}

pcaPlot <- function () {
  pca_df <- runHClust() 
  
  pca_df <- pca_df %>%
  mutate(cluster_label = case_when(
    cluster_label == "3" ~ "One-Step",
    cluster_label == "2" ~ "Multi-Step",
    cluster_label == "1" ~ "Exploratory"
    # cluster_label == "3" ~ "Delayed-Exploratory",
    # cluster_label == "1" ~ "Light-Exploratory",
    # cluster_label == "6" ~ "High-Exploratory"
  ))


# hulls <- pca_df %>%
#   group_by(cluster_label) %>%
#   slice(chull(PC1, PC2))

ggplot(pca_df, aes(PC1, PC2, color = cluster_label, fill = cluster_label)) +
  # 1. Hulls: Ensure alpha is low and show.legend is FALSE
  # geom_polygon(data = hulls,
  #              alpha = 0.15,      # Lower alpha helps if colors overlap
  #              linetype = "dashed",
  #              linewidth = 0.5,
  #              show.legend = FALSE) +
  

  geom_point(size = 2.0) + 
  theme_classic() + 
 
  scale_color_manual(values = c(
    "One-Step"  = "orchid",
    "Multi-Step"     = "cyan",
    "Exploratory"   = "#EAA178"
    # "Delayed-Exploratory" = "#fba0e3",
    # "Light-Exploratory" = "#ffcff1",
    # "High-Exploratory" = "#c54bbc"
  )) +
  scale_fill_manual(values = c(
    "One-Step"  = "orchid",
    "Multi-Step"     = "cyan",
    "Exploratory"   = "#EAA178"
    # "Delayed-Exploratory" = "#fba0e3",
    # "Light-Exploratory" = "#ffcff1",
    # "High-Exploratory" = "#c54bbc"
  )) +

  guides(
    fill = "none", 
    color = guide_legend(override.aes = list(
      alpha = 1, 
      size = 4, 
      linetype = 0 
    ))
  ) +
  
  labs(color = "Phenotype", title = "") +
  
  theme(
    legend.key = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
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
      cluster_label == "3" ~ "One-Step",
      cluster_label == "2" ~ "Multi-Step",
      cluster_label == "1" ~ "Exploratory"
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
    "Multi-Step",
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
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +
  scale_fill_manual(
    values = c(
      "Non-strategy" = "white",
      "One-Step"  = "orchid",
      "Multi-Step"     = "cyan",
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
  labs(x = "PC1", y = "PC2")
}


lmRot <- function () {
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
  
  model <- lm(mean_PC3 ~ rotation, data = rotation_summary)
  summary(lm(mean_PC3 ~ rotation, data = rotation_summary))

}

ggplot(pca_df, aes(x = rotation, y = PC1)) +
  
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
  labs(x = "Rotation", y = "PC1")



ggplot(pca_df, aes(x = rotation, y = -PC1, color = factor(rotation))) +
  
  geom_point(size = 2, alpha = 0.8) +
  
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
  labs(x = "Rotation", y = "PC1")



##gap statistic

library(cluster)

hc_fun <- function(x, k) {
  distance_matrix <- dist(x, method = "euclidean")
  hc <- hclust(distance_matrix, method = "ward.D2")
  
  list(cluster = cutree(hc, k))
}

set.seed(123)

gap_stat <- clusGap(
  pca_df[, c("PC1", "PC2", "PC3")],
  FUNcluster = hc_fun,
  K.max = 10,
  B = 500
)

plot(gap_stat)
