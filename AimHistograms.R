hist2d <- function(x, y, nbins = c(100, 100), edges = NULL) {
  
  df <- data.frame(x = x, y = y)
  
  if (!is.null(edges)) {
    x.edges <- edges[[1]]
    y.edges <- edges[[2]]
  } else {
    x.edges <- seq(floor(min(df$x)), ceiling(max(df$x)), length.out = nbins[1])
    y.edges <- seq(floor(min(df$y)), ceiling(max(df$y)), length.out = nbins[2])
  }
  
  xb <- findInterval(df$x, x.edges, rightmost.closed = TRUE)
  yb <- findInterval(df$y, y.edges, rightmost.closed = TRUE)
  
  freq2D <- as.matrix(table(
    factor(xb, levels = 1:(length(x.edges)-1)),
    factor(yb, levels = 1:(length(y.edges)-1))
  ))
  
  list(freq2D = freq2D, x.edges = x.edges, y.edges = y.edges)
}
#create data frame
learner_data <- read.csv("data/total_learners_data.csv")


learner_data <- learner_data %>%
  mutate(
    rotation = abs(rotation),
    time = cutrial_no - 113   
  )
strategy_data <- strategy_data %>%
  mutate(
    rotation = abs(rotation),
    time = cutrial_no - 113   
  )


all_hist_data <- strategy_data %>%
  filter(time >= -8 & time <= 100) %>%
  transmute(
    x = time,
    y = reachdeviation_deg,
    rotation = rotation
  )

plotAimHist <- function(rot_value, y_max) {

  df <- all_hist_data %>%
    filter(rotation == rot_value)
  
  plot(NA,
       main = paste0(rot_value, "° Rotation"),
       xlab = "Trial",
       ylab = "Reach Deviation (°)",
       xlim = c(-8, 60),
       ylim = c(-80, 80),
       ax = FALSE,
       bty = "n")
  
  img_info <- hist2d(
    x = df$x,
    y = df$y,
    edges = list(
      seq(-8, 60, 1),
      seq(-80, 80, 2.5)
    )
  )
  
  img <- log(img_info$freq2D + 1)
  
  image(
    x = img_info$x.edges,
    y = img_info$y.edges,
    z = img,
    col = colorRampPalette(c("white", "#4895ef", "#2835af", "#12086f"))(100),
    add = TRUE
  )
  
  axis(1, at = c(-8, 0, 8,16,24,32,40,48,56,60), cex.axis = 1)
  axis(2, at = seq(-80, 80, 20), cex.axis = 1)
  
  # reference box (rotation size)
  lines(
    x = c(-8, 0, 0, 60),
    y = c(-0.5, -0.5, y_max - 0.5, y_max - 0.5),
    col = "black",
    lty = 3,
    lwd = 2
  )
  
  # OPTIONAL: mean trajectory
   avg <- aggregate(y ~ x, data = df, FUN = mean)
   lines(avg$x, avg$y, col = "hotpink", lwd = 2)
}



par(mfrow = c(3,1))

plotAimHist(20, 20)
plotAimHist(30, 30)
plotAimHist(40, 40)



plotAimHist(50, 50)
plotAimHist(60, 60)


##



hist_data <- learner_data %>%
  mutate(time = as.numeric(time)) %>% # 
  filter(time >= -8 & time <= 100) %>%
  transmute(
    x = time,
    y = reachdeviation_deg,
    rotation = rotation
  )

plotReachHist <- function(rot_value, y_max) {
  
  df <- hist_data %>%
    filter(rotation == rot_value)
  
  plot(NA,
       main = paste0(rot_value, "° Rotation"),
       xlab = "Trial",
       ylab = "Reach Deviation (°)",
       xlim = c(-8, 60),
       ylim = c(-20, 60),
       ax = FALSE,
       bty = "n",
       cex.lab = 1.4,
       cex.axis = 1.5,
       cex.main = 1.5)
  
  img_info <- hist2d(
    x = df$x,
    y = df$y,
    edges = list(
      seq(-8, 60, 1),
      seq(-60, 60, 1)
    )
  )
  
  img <- log(img_info$freq2D + 1)
  
  image(
    x = img_info$x.edges,
    y = img_info$y.edges,
    z = img,
    col = colorRampPalette(c("white", "#ADD1F1", "#2073BC", "#12436D"))(100),
    add = TRUE
  )
  
  axis(1, at = c(-8, 0, 8,16,24,32,40,48,56), cex.axis = 1.5)
  axis(2, at = seq(-60, 60, 10), cex.axis = 1.5)
  
  # reference box (rotation size)
  lines(
    x = c(-8, 0, 0, 60),
    y = c(-0.5, -0.5, y_max - 0.5, y_max - 0.5),
    col = "navy",
    lty = 3,
    lwd = 2
  )
  avg <- aggregate(y ~ x, data = df, FUN = mean)
  lines(avg$x, avg$y, col = "hotpink", lwd = 2)
  
  # OPTIONAL: mean trajectory
  # avg <- aggregate(y ~ x, data = df, FUN = mean)
  # lines(avg$x, avg$y, col = "grey20", lwd = 2)
}



par(mfrow = c(5,1))

plotReachHist(20, 20)
plotReachHist(30, 30)
plotReachHist(40, 40)
plotReachHist(50, 50)
plotReachHist(60, 60)



##phenotype

# cluster lookup
cluster_lookup <- pca_df %>% 
  select(participant_id, cluster_label)
strat_data <- read.csv("data/strategy_only_participants.csv")
# join cluster into trial-level data FIRST
strat_data <- strat_data %>%
  mutate(time = cutrial_no - 113) %>%
  left_join(cluster_lookup, by = "participant_id")

# normalize AFTER join
strat_data <- strat_data %>%
  mutate(
    aim_norm = ifelse(
      is.finite(rotation) & rotation != 0,
      aimdeviation_deg / rotation,
      NA_real_
    )
  )

# build plotting dataset
hist_data <- strat_data %>%
  filter(time >= -8 & time <= 40) %>%
  mutate(
    # Collapse all 'Exploration' types into one
    cluster = case_when(
      grepl("Exploration", cluster_label) ~ "Exploration",
      cluster_label == "Gradual" ~ "Gradual",
      cluster_label == "Stepwise" ~ "Stepwise",
      TRUE ~ "Unclassified"
    )
  ) %>%
  transmute(x = time, y = aim_norm, cluster = cluster) %>%
  filter(is.finite(x), is.finite(y), cluster != "Unclassified")

plotClusterHist <- function(cluster_name) {
  
  df <- hist_data %>% filter(cluster == cluster_name)
  
  if (nrow(df) == 0) {
    warning(paste("No data for cluster", cluster_name))
    return(NULL)
  }
  
  cluster_colors <- c("Gradual" = "#E78AC3", "Stepwise" = "#FC8D62", "Exploration" = "#00BFC4")
  
  # 1. Background Plot
  plot(NA, main = cluster_name, xlab = "Trial Number", ylab = "Normalized Aim",
       xlim = c(-8, 40), ylim = c(-0.5, 1.5), axes = FALSE, bty = "n",
       cex.lab = 1.2, cex.main = 1.5)
  
  # 2. Density Heatmap (Using your custom hist2d - ensure 'show' is removed if needed)
  img_info <- hist2d(df$x, df$y, edges = list(seq(-8, 40, 1), seq(-0.5, 1.5, 0.05)))
  img <- (img_info$freq2D / max(img_info$freq2D, na.rm = TRUE))^0.5
  
  image(x = img_info$x.edges, y = img_info$y.edges, z = img,
        col = colorRampPalette(c("white", adjustcolor(cluster_colors[cluster_name], 0.2), cluster_colors[cluster_name]))(500),
        add = TRUE)
  

  # 4. Average Line (Keep it thick so it stands out)
  avg <- df %>% group_by(x) %>% summarise(y = mean(y, na.rm = TRUE), .groups = "drop")
  lines(avg$x, avg$y, col = "black", lwd = 1)
  
  # 5. Aesthetics
  axis(1, at = c(-8, 0, 40)); axis(2, at = seq(-0.5, 1.5, 0.5))
  abline(h = 1, col = "grey40", lty = 2, lwd = 1.5)
  abline(h = 0, col = "black", lwd = 1)
  abline(v = 0, col = "grey40", lty = 2, lwd = 1.5)
}

# 3. Call the plots by NAME now
par(mfrow = c(1,3))
plotClusterHist("Exploration")
plotClusterHist("Gradual")
plotClusterHist("Stepwise")