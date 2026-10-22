library(dplyr)

# --------------------------------------------------
# 1. 2D histogram function
# --------------------------------------------------

hist2d <- function(x, y, nbins = c(100, 100), edges = NULL) {
  
  valid <- is.finite(x) & is.finite(y)
  x <- x[valid]
  y <- y[valid]
  
  if (length(x) == 0) {
    stop("No valid observations available for the histogram.")
  }
  
  if (!is.null(edges)) {
    x.edges <- edges[[1]]
    y.edges <- edges[[2]]
  } else {
    x.edges <- seq(
      floor(min(x)), ceiling(max(x)),
      length.out = nbins[1] + 1
    )
    y.edges <- seq(
      floor(min(y)), ceiling(max(y)),
      length.out = nbins[2] + 1
    )
  }
  
  xb <- findInterval(x, x.edges, rightmost.closed = TRUE)
  yb <- findInterval(y, y.edges, rightmost.closed = TRUE)
  
  freq2D <- as.matrix(table(
    factor(xb, levels = seq_len(length(x.edges) - 1)),
    factor(yb, levels = seq_len(length(y.edges) - 1))
  ))
  
  list(
    freq2D = freq2D,
    x.edges = x.edges,
    y.edges = y.edges
  )
}


# --------------------------------------------------
# 2. Load and prepare data
# --------------------------------------------------

learner_data <- read.csv("data/total_learners_data.csv")
strategy_data <- read.csv("data/strategy_only_participants.csv")

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


# Aiming data: strategy participants
all_hist_data <- strategy_data %>%
  filter(time >= -8, time <= 60) %>%
  transmute(
    x = time,
    y = aimdeviation_deg,
    rotation = rotation
  )


# Reach data: all learners
hist_data <- learner_data %>%
  filter(time >= -8, time <= 60) %>%
  transmute(
    x = time,
    y = reachdeviation_deg,
    rotation = rotation
  )


# --------------------------------------------------
# 3. Shared plotting function
# --------------------------------------------------

plotDeviationHist <- function(
    df, rot_value, y_max, y_label,
    show_y_label = FALSE) {
  
  df <- df %>%
    filter(
      rotation == rot_value,
      is.finite(x),
      is.finite(y)
    )
  
  if (nrow(df) == 0) {
    plot.new()
    title(main = paste0(rot_value, "\u00b0 Rotation"))
    text(0.5, 0.5, "No valid data")
    return(invisible(NULL))
  }
  
  # Histogram bin edges
  x_edges <- seq(-8, 60, by = 1)
  y_edges <- seq(-20, 80, by = 2.5)
  
  # Set up blank plot
  plot(
    NA,
    main = paste0(rot_value, "\u00b0 Rotation"),
    xlab = "Trial",
    ylab = if (show_y_label) y_label else "",
    xlim = c(-8, 60),
    ylim = c(-20, 80),
    xaxs = "i",
    yaxs = "i",
    xaxt = "n",
    yaxt = "n",
    bty = "n",
    cex.lab = 2,
    cex.axis = 2,
    cex.main = 2
  )
  
  # Calculate histogram frequencies
  img_info <- hist2d(
    x = df$x,
    y = df$y,
    edges = list(x_edges, y_edges)
  )
  
  # Bin centres
  x_centers <- head(x_edges, -1) + diff(x_edges) / 2
  y_centers <- head(y_edges, -1) + diff(y_edges) / 2
  
  # Log-transform frequencies
  img <- log1p(img_info$freq2D)
  
  # Histogram colour palette
  hist_cols <- colorRampPalette(c(
    "#FFFFFF",
    "#F5A6D7",
    "#D16BEA",
    "#8B5CF6",
    "#3457D5"
  ))(100)
  
  image(
    x = x_centers,
    y = y_centers,
    z = img,
    col = hist_cols,
    add = TRUE,
    useRaster = TRUE
  )
  
  # X-axis
  axis(
    1,
    at = c(-8, 0, 20, 40, 60),
    labels = c("-8", "0", "20", "40", "60"),
    cex.axis = 2,
    lwd = 0,
    lwd.ticks = 1
  )
  
  # Y-axis tick numbers on every panel
  axis(
    2,
    at = seq(-20, 80, by = 20),
    cex.axis = 2,
    lwd = 0,
    lwd.ticks = 1
  )
  
  # Reference lines: baseline and rotation magnitude
  lines(
    x = c(-8, 0, 0, 60),
    y = c(-0.5, -0.5, y_max - 0.5, y_max - 0.5),
    col = "black",
    lty = 3,
    lwd = 1.5
  )
  
  # Mean deviation at each trial
  avg <- df %>%
    group_by(x) %>%
    summarise(
      y_mean = mean(y, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(x)
  
  lines(
    x = avg$x,
    y = avg$y_mean,
    col = "#7A155D",
    lwd = 2.5
  )
}


# --------------------------------------------------
# 4. Reach and aiming plotting functions
# --------------------------------------------------

plotReachHist <- function(rot_value, y_max = rot_value,
                          show_y_label = FALSE) {
  plotDeviationHist(
    df = hist_data,
    rot_value = rot_value,
    y_max = y_max,
    y_label = "Reach Dev (\u00b0)",
    show_y_label = show_y_label
  )
}

plotAimHist <- function(rot_value, y_max = rot_value,
                        show_y_label = FALSE) {
  plotDeviationHist(
    df = all_hist_data,
    rot_value = rot_value,
    y_max = y_max,
    y_label = "Aim Dev (\u00b0)",
    show_y_label = show_y_label
  )
}


# --------------------------------------------------
# 5. Export: 5 columns, Reach above Aim
# --------------------------------------------------

png(
  "aim.png",
  width = 6000,
  height = 2500,
  res = 300
)

par(
  mfcol = c(2, 5),
  mar = c(4.5, 4.8, 3.5, 0.8),
  oma = c(1, 1, 0.5, 0.5)
)

rotations <- c(20, 30, 40, 50, 60)

for (i in seq_along(rotations)) {
  
  rot <- rotations[i]
  
  # Reach row: y-axis title only on far left
  plotReachHist(
    rot,
    rot,
    show_y_label = (i == 1)
  )
  
  # Aim row: y-axis title only on far left
  plotAimHist(
    rot,
    rot,
    show_y_label = (i == 1)
  )
}

dev.off()