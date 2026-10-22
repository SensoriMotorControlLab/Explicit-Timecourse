
set.seed(123)

gamma_value <- 0.1

prepare_data <- function() {
  
  strategy_data <- read.csv("data/strategy_only_participants.csv")
  
  rotated_data <- strategy_data %>%
    filter(trial_type == "rotated") %>%
    arrange(participant_id, cutrial_no)
  
  ts_df <- rotated_data %>%
    group_by(participant_id) %>%
    filter(n() >= 120) %>%
    summarise(
      series = list(as.numeric(head(aimdeviation_deg, 120))),
      rotation = first(rotation),
      .groups = "drop"
    ) %>%
    filter(
      lengths(series) == 120,
      vapply(series, function(x) all(is.finite(x)), logical(1)),
      vapply(series, sd, numeric(1)) > 0
    )
  
  ids <- as.character(ts_df$participant_id)
  series_list <- ts_df$series
  names(series_list) <- ids
  
  series_list <- lapply(series_list, function(x) {
    as.numeric(scale(x))
  })
  
  list(
    ts_df = ts_df,
    ids = ids,
    series_list = series_list
  )
}


calculate_soft_dtw <- function() {
  
  soft_dtw_divergence <- function(x, y) {
    xy <- dtwclust::sdtw(x, y, gamma = gamma_value)
    xx <- dtwclust::sdtw(x, x, gamma = gamma_value)
    yy <- dtwclust::sdtw(y, y, gamma = gamma_value)
    
    xy - 0.5 * xx - 0.5 * yy
  }
  
  n <- length(series_list)
  
  soft_dtw_matrix <- matrix(
    0,
    nrow = n,
    ncol = n,
    dimnames = list(ids, ids)
  )
  
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      d <- soft_dtw_divergence(
        series_list[[i]],
        series_list[[j]]
      )
      
      soft_dtw_matrix[i, j] <- d
      soft_dtw_matrix[j, i] <- d
    }
  }
  
  diag(soft_dtw_matrix) <- 0
  
  soft_dtw_dist <- as.dist(soft_dtw_matrix)
  
  print(summary(as.vector(soft_dtw_matrix)))
  
  stopifnot(
    isTRUE(all.equal(soft_dtw_matrix, t(soft_dtw_matrix))),
    all(diag(soft_dtw_matrix) == 0)
  )
  
  list(
    soft_dtw_matrix = soft_dtw_matrix,
    soft_dtw_dist = soft_dtw_dist
  )
}


run_clustering <- function() {
  
  hc <- hclust(soft_dtw_dist, method = "ward.D2")
  
  k_initial <- 3
  clusters <- cutree(hc, k = k_initial)
  
  cluster_assignments <- data.frame(
    participant_id = ids,
    cluster = factor(clusters)
  )
  
  print(table(cluster_assignments$cluster))
  
  plot(
    hc,
    labels = FALSE,
    hang = -1,
    main = "Hierarchical Clustering: Soft-DTW",
    xlab = "Participants",
    ylab = "Height"
  )
  
  dend <- as.dendrogram(hc)
  
  dend <- dendextend::color_branches(
    dend,
    k = 3,
    col = c("orchid", "#36A9E1", "#F05A72")
  )
  
  plot(
    dend,
    main = "Soft-DTW Hierarchical Clustering (k = 3)",
    ylab = "Height",
    leaflab = "none"
  )
  
  list(
    hc = hc,
    clusters = clusters,
    cluster_assignments = cluster_assignments
  )
}


run_tsne <- function() {
  
  n <- length(ids)
  
  perplexities <- c(5, 10, 20, 30, 40)
  perplexities <- perplexities[
    3 * perplexities < n - 1
  ]
  
  tsne_fits <- list()
  
  for (perp in perplexities) {
    set.seed(123)
    
    tsne_fits[[as.character(perp)]] <- Rtsne(
      as.matrix(soft_dtw_dist),
      is_distance = TRUE,
      dims = 2,
      perplexity = perp,
      max_iter = 2000,
      check_duplicates = FALSE,
      pca = FALSE,
      verbose = FALSE
    )
  }
  
  selected_perplexity <- "20"
  
  if (!selected_perplexity %in% names(tsne_fits)) {
    stop("Selected perplexity is not available for this sample size.")
  }
  
  tsne <- tsne_fits[[selected_perplexity]]
  
  tsne_df <- data.frame(
    participant_id = ids,
    TSNE1 = tsne$Y[, 1],
    TSNE2 = tsne$Y[, 2],
    cluster = factor(clusters)
  )
  
  tsne_df$cluster <- factor(
    tsne_df$cluster,
    levels = c("1", "2", "3"),
    labels = c("Incremental", "Exploratory", "One-step")
  )
  
  p_tsne <- ggplot(
    tsne_df,
    aes(
      x = TSNE1,
      y = TSNE2,
      color = cluster,
      text = participant_id
    )
  ) +
    geom_point(size = 3, alpha = 0.85) +
    scale_color_manual(
      values = c(
        "Incremental" = "#a8bebf",
        "Exploratory" = "#ff6ab4",
        "One-step" = "#d8d681"
      )
    ) +
    theme_classic() +
    labs(
      title = " ",
      x = "t-SNE 1",
      y = "t-SNE 2",
      color = "Strategy"
    ) +
    theme(
      legend.position = "right"
    )
  
  
  list(
    tsne_fits = tsne_fits,
    tsne_df = tsne_df,
    p_tsne = p_tsne
  )
}


run_silhouette_analysis <- function() {
  
  k_results <- data.frame()
  
  for (k in 2:10) {
    group <- cutree(hc, k = k)
    sil <- cluster::silhouette(group, soft_dtw_dist)
    
    k_results <- rbind(
      k_results,
      data.frame(
        k = k,
        cluster_sizes = paste(
          as.integer(table(group)),
          collapse = ", "
        ),
        mean_silhouette = mean(sil[, "sil_width"]),
        negative_silhouette_pct =
          mean(sil[, "sil_width"] < 0) * 100
      )
    )
  }
  
  
  best_k <- k_results$k[
    which.max(k_results$mean_silhouette)
  ]
  
  cat("Candidate k with highest mean silhouette:", best_k, "\n")
  
  best_clusters <- cutree(hc, k = best_k)
  best_sil <- cluster::silhouette(best_clusters, soft_dtw_dist)
  
  list(
    k_results = k_results,
    best_k = best_k,
    best_clusters = best_clusters,
    best_sil = best_sil
  )
}


compare_rotation <- function() {
  
  plot_assignments <- data.frame(
    participant_id = ids,
    cluster = factor(clusters)
  )
  
  cluster_rotation <- ts_df %>%
    select(participant_id, rotation) %>%
    left_join(plot_assignments, by = "participant_id")
  
  total_learners_data <- read.csv("data/total_learners_data.csv")
  
  all_participants <- total_learners_data %>%
    distinct(participant_id, rotation) %>%
    left_join(
      cluster_rotation,
      by = c("participant_id", "rotation")
    ) %>%
    mutate(
      cluster = ifelse(
        is.na(cluster),
        "No Strategy",
        as.character(cluster)
      ),
      cluster = recode(
        cluster,
        "1" = "Incremental",
        "2" = "Exploratory",
        "3" = "One-step"
      ),
      cluster = factor(
        cluster,
        levels = c(
          "No Strategy",
          "Exploratory",
          "One-step",
          "Incremental"
        )
      )
    )
  
  print(table(all_participants$rotation, all_participants$cluster))
  
  p_rotation <- ggplot(
    all_participants,
    aes(x = factor(rotation), fill = cluster)
  ) +
    geom_bar(position = "fill") +
    scale_y_continuous(
      labels = scales::percent_format()
    ) +
    scale_fill_manual(
      values = c(
        "No Strategy" = "white",
        "Exploratory" = "#ff6ab4",
        "One-step" = "#d8d681",
        "Incremental" = "#a8bebf"
      )
    ) +
    theme_classic() +
    labs(
      title = "",
      x = "Rotation size",
      y = "Proportion of participants",
      fill = "Strategy"
    )
  
  
  list(
    all_participants = all_participants,
    p_rotation = p_rotation
  )
}


run_gradient_analysis <- function() {
  
  tsne_df$rotation <- ts_df$rotation
  
  rotation_summary <- tsne_df %>%
    group_by(rotation) %>%
    summarise(
      mean_x = mean(TSNE1, na.rm = TRUE),
      sd_x = sd(TSNE1, na.rm = TRUE),
      mean_y = mean(TSNE2, na.rm = TRUE),
      sd_y = sd(TSNE2, na.rm = TRUE),
      .groups = "drop"
    )
  
  model1 <- lm(TSNE1 ~ rotation, data = tsne_df)
  print(summary(model1))
  cat("\n--------------------------------------------------\n\n")
  
  model2 <- lm(TSNE2 ~ rotation, data = tsne_df)
  print(summary(model2))
  
  p_gradient <- ggplot(
    tsne_df,
    aes(x = TSNE1, y = TSNE2)
  ) +
    geom_point(
      colour = "grey70",
      size = 3,
      alpha = 0.8
    ) +
    geom_segment(
      data = rotation_summary,
      aes(
        x = mean_x - sd_x,
        xend = mean_x + sd_x,
        y = mean_y,
        yend = mean_y,
        colour = factor(rotation)
      ),
      linewidth = 0.8
    ) +
    geom_segment(
      data = rotation_summary,
      aes(
        x = mean_x,
        xend = mean_x,
        y = mean_y - sd_y,
        yend = mean_y + sd_y,
        colour = factor(rotation)
      ),
      linewidth = 0.8
    ) +
    geom_point(
      data = rotation_summary,
      aes(
        x = mean_x,
        y = mean_y,
        colour = factor(rotation)
      ),
      inherit.aes = FALSE,
      shape = 3,
      size = 6,
      stroke = 1.5
    ) +
    geom_text(
      data = rotation_summary,
      aes(
        x = mean_x,
        y = mean_y,
        label = paste0(rotation, "\u00B0"),
        colour = factor(rotation)
      ),
      inherit.aes = FALSE,
      nudge_y = -2.5,
      vjust = 1,
      show.legend = FALSE,
      size = 4
    ) +
    scale_colour_manual(
      values = c(
        "20" = "#6554C0",
        "30" = "#B8B0E8",
        "40" = "#D700CB",
        "50" = "#FA6A9B",
        "60" = "#FA002B"
      ),
      name = "Rotation (°)"
    ) +
    theme_classic() +
    labs(
      title = "t-SNE Embedding by Rotation Size",
      x = "t-SNE 1",
      y = "t-SNE 2"
    )
  
  print(p_gradient)
  
  list(
    rotation_summary = rotation_summary,
    model1 = model1,
    model2 = model2,
    p_gradient = p_gradient
  )
}


data_objects <- prepare_data()

ts_df <- data_objects$ts_df
ids <- data_objects$ids
series_list <- data_objects$series_list

dtw_objects <- calculate_soft_dtw()

soft_dtw_matrix <- dtw_objects$soft_dtw_matrix
soft_dtw_dist <- dtw_objects$soft_dtw_dist

clustering_objects <- run_clustering()

hc <- clustering_objects$hc
clusters <- clustering_objects$clusters
cluster_assignments <- clustering_objects$cluster_assignments

tsne_objects <- run_tsne()

tsne_fits <- tsne_objects$tsne_fits
tsne_df <- tsne_objects$tsne_df
p_tsne <- tsne_objects$p_tsne

silhouette_objects <- run_silhouette_analysis()

k_results <- silhouette_objects$k_results
best_k <- silhouette_objects$best_k
best_clusters <- silhouette_objects$best_clusters
best_sil <- silhouette_objects$best_sil

rotation_objects <- compare_rotation()

all_participants <- rotation_objects$all_participants
p_rotation <- rotation_objects$p_rotation

gradient_objects <- run_gradient_analysis()

rotation_summary <- gradient_objects$rotation_summary
model1 <- gradient_objects$model1
model2 <- gradient_objects$model2
p_gradient <- gradient_objects$p_gradient
