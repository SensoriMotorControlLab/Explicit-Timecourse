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


all_hist_data <- learner_data %>%
  filter(time >= -8 & time <= 100) %>%
  transmute(
    x = time,
    y = aimdeviation_deg,
    rotation = rotation
  )

plotAimHist <- function(rot_value, y_max) {
  
  df <- all_hist_data %>%
    filter(rotation == rot_value)
  
  plot(NA,
       main = paste0(rot_value, "° Rotation"),
       xlab = "Trial",
       ylab = "Aim Deviation (°)",
       xlim = c(-8, 40),
       ylim = c(-60, 60),
       ax = FALSE,
       bty = "n",
       cex.lab = 1.5,
       cex.axis = 1.5,
       cex.main = 1.5)
  
  img_info <- hist2d(
    x = df$x,
    y = df$y,
    edges = list(
      seq(-8, 40, 1),
      seq(-60, 60, 2.5)
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
  
  axis(1, at = c(-8, 0, 8,16,24,32,40), cex.axis = 1.5)
  axis(2, at = seq(-60, 60, 10), cex.axis = 1.5)
  
  # reference box (rotation size)
  lines(
    x = c(-8, 0, 0, 40),
    y = c(-0.5, -0.5, y_max - 0.5, y_max - 0.5),
    col = "black",
    lty = 3,
    lwd = 2
  )
  
  # OPTIONAL: mean trajectory
  # avg <- aggregate(y ~ x, data = df, FUN = mean)
  # lines(avg$x, avg$y, col = "navy", lwd = 2)
}



par(mfrow = c(3,2))

plotAimHist(20, 20)
plotAimHist(30, 30)
plotAimHist(40, 40)
plotAimHist(50, 50)
plotAimHist(60, 60)


##



  hist_data <- learner_data %>%
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
       xlim = c(-8, 40),
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
      seq(-8, 40, 1),
      seq(-60, 60, 3)
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
  
  axis(1, at = c(-8, 0, 8,16,24,32,40), cex.axis = 1.5)
  axis(2, at = seq(-60, 60, 10), cex.axis = 1.5)
  
  # reference box (rotation size)
  lines(
    x = c(-8, 0, 0, 40),
    y = c(-0.5, -0.5, y_max - 0.5, y_max - 0.5),
    col = "navy",
    lty = 3,
    lwd = 2
  )
  
  # OPTIONAL: mean trajectory
  # avg <- aggregate(y ~ x, data = df, FUN = mean)
  # lines(avg$x, avg$y, col = "grey20", lwd = 2)
}



par(mfrow = c(3,2))

plotReachHist(20, 20)
plotReachHist(30, 30)
plotReachHist(40, 40)
plotReachHist(50, 50)
plotReachHist(60, 60)



##phenotype
out <- confMatrix()

model_df_labeled <- out$data
conf_table <- out$table

# cluster lookup
cluster_lookup <- model_df_labeled %>%
  dplyr::select(participant_id, cluster_aligned)

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
  transmute(
    x = time,
    y = aim_norm,
    cluster = cluster_aligned
  ) %>%
  filter(is.finite(x), is.finite(y), !is.na(cluster))


plotClusterHist <- function(cluster_value) {
  
  df <- hist_data %>%
    filter(cluster == cluster_value)
  
  if (nrow(df) == 0) {
    warning(paste("No data for cluster", cluster_value))
    return(NULL)
  }
  
  cluster_names <- c(
    "1" = "Gradual",
    "2" = "Stepwise",
    "3" = "Exploratory"
  )
  
  plot(NA,
       main = cluster_names[as.character(cluster_value)],
       xlab = "Trial Number",
       ylab = "Normalized Aim Deviation",
       xlim = c(-8, 40),
       ylim = c(-0.5, 1.5),
       axes = FALSE,
       bty = "n",
       cex.lab = 1.5,
       cex.axis = 1.5,
       cex.main = 1.5)
  
  img_info <- hist2d(
    x = df$x,
    y = df$y,
    edges = list(
      seq(-8, 40, 1),
      seq(-0.5, 1.5, 0.05)
    )
  )
  img <- img_info$freq2D
  img <- img / max(img, na.rm = TRUE)
  img <- img^0.5
  cluster_colors <- c(
    "Gradual"     = "#e89c7b",
    "Stepwise"    = "#a2bffe",
    "Exploratory" = "hotpink"
  )
  
  cluster_names <- c(
    "1" = "Gradual",
    "2" = "Stepwise",
    "3" = "Exploratory"
  )
  
  main_color <- cluster_colors[cluster_names[as.character(cluster_value)]]
  
  image(
    x = img_info$x.edges,
    y = img_info$y.edges,
    z = img,
    col = colorRampPalette(c("white", adjustcolor(main_color, alpha.f = 1), main_color))(500),
    add = TRUE
  )
  
  axis(1, at = c(-8, 0, 40), cex.axis = 1.5)
  axis(2, at = seq(-0.5, 1.5, 0.5), cex.axis = 1.5)
  
  abline(h = 1, col = "grey", lty = 2, lwd = 2)  # full compensation line
  abline(v = 0, col = "grey", lty = 2, lwd = 2)
  
  avg <- df %>%
    group_by(x) %>%
    summarise(y = mean(y, na.rm = TRUE), .groups = "drop")
  line_colors <- c(
    "1" = "#dd571c",
    "2" = "#0077b6",
    "3" = "#b33b72"
  )
  lines(avg$x, avg$y,
        col = adjustcolor(line_colors[as.character(cluster_value)], 0.8),
        lwd = 2)
}

par(mfrow = c(1,3))

plotClusterHist("1")
plotClusterHist("2")
plotClusterHist("3")