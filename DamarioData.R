hist2d <- function(x, y=NA, nbins=c(25,25), edges=NA) {
  if (is.data.frame(x)) {
    df <- x
  } else if (is.matrix(x)) {
    df <- as.data.frame(x)
  } else {
    df <- data.frame('x'=x, 'y'=y)
  }
  
  # If edges are given, use them
  if (is.list(edges)) {
    x.edges <- edges[[1]]
    y.edges <- edges[[2]]
  } else if (is.numeric(nbins)) { 
    # Otherwise, fall back on nbins
    x.edges <- seq(floor(min(df[,1])), ceiling(max(df[,1])), length=nbins[1])
    y.edges <- seq(floor(min(df[,2])), ceiling(max(df[,2])), length=nbins[2])
  }
  
  xbincount <- findInterval(df[,1], x.edges, rightmost.closed = TRUE, left.open = FALSE, all.inside = FALSE)
  ybincount <- findInterval(df[,2], y.edges, rightmost.closed = TRUE, left.open = FALSE, all.inside = FALSE)
  xbincount <- factor(xbincount, levels=c(1:(length(x.edges)-1)))
  ybincount <- factor(ybincount, levels=c(1:(length(y.edges)-1)))
  
  freq2D <- as.matrix(table(xbincount,ybincount))
  dimnames(freq2D) <- NULL
  
  return(list('freq2D'=freq2D, 'x.edges'=x.edges, 'y.edges'=y.edges))
}

seb_data <- read.csv("/Users/elysa/Desktop//aiming_aiming.csv")

aligned_trials <- seb_data %>%
  filter(phase == "baseline") %>%
  group_by(participant) %>%
  arrange(trialno) %>%
  slice_tail(n = 8) %>%
  ungroup()
aligned_trials <- aligned_trials %>%
  group_by(participant) %>%
  arrange(trialno) %>%
  mutate(trialno_aligned = seq(-8, -1)) %>%
  ungroup()


rotated_trials <- seb_data %>%
  filter(phase == "rotation") %>%
  group_by(participant) %>%
  arrange(trialno) %>%
  slice_head(n = 32) %>%
  ungroup()
rotated_trials <- rotated_trials %>%
  group_by(participant) %>%
  arrange(trialno) %>%
  mutate(trialno_aligned = seq(0, 31)) %>%
  ungroup()


all_hist_data <- rbind(aligned_trials, rotated_trials) %>%
  select(trialno_aligned, aimingdeviation_deg) %>%
  rename(trialno = trialno_aligned)


plotSEBaim <- function(all_hist_data, df_steps) {
  
  # Set up empty plot
  plot(NA,
       main = "",
       xlab = "", ylab = "",
       xlim = c(-8, 32), ylim = c(15, -70),
       axes = FALSE, bty = "n",
       cex.lab = 3,     
       cex.axis = 3,
  )
  
  # 2D histogram
  img_info <- hist2d(
    x = all_hist_data$trialno,
    y = all_hist_data$aimingdeviation_deg,
    edges = list(seq(-8, 32, 1), seq(-70, 15, 2.5))  # flipped y
  )
  img <- log(img_info$freq2D + 1)
  
  # Image plot
  image(
    x = img_info$x.edges,
    y = img_info$y.edges,
    z = img,
    col = colorRampPalette(c("white", "gray90", "gray40", "gray28"))(100),
    add = TRUE
  )
  
  axis(side = 1, at = c(-8, 0, 8, 16, 24, 32), cex.axis = 2)  # x-axis numbers
  axis(side = 2, at = seq(0, -70, -10), cex.axis = 2)  
  
  for(part in unique(df_steps_line$participant)) {
    part_data <- df_steps_line %>% filter(participant == part)
    lines(part_data$x, part_data$y, col="brown3", lwd=1.5)
  }
  
  
}

# Call the function
plotSEBaim(all_hist_data, first_step_over_5)






first_step_over_5 <- rotated_trials %>%
  group_by(participant) %>%
  summarise(
    first_trial = {
      vals <- trialno_aligned[!is.na(aimingdeviation_deg) & aimingdeviation_deg < -5]
      if (length(vals) == 0) NA_real_ else min(vals)
    },
    .groups = "drop"
  ) %>%
  left_join(
    rotated_trials %>% select(participant, trialno_aligned, aimingdeviation_deg),
    by = c("participant" = "participant", "first_trial" = "trialno_aligned")
  ) %>%
  rename(first_aimdev = aimingdeviation_deg)


result_table <- dplyr::select(first_step_over_5, 
                              participant, first_trial, first_aimdev)

df_steps_line <- result_table %>%
  filter(!is.na(first_aimdev)) %>%
  rowwise() %>%
  mutate(
    x = list(c(-8:(first_trial-1), first_trial, first_trial:31)),      # -8 to just before first step, step trial, then rest
    y = list(c(rep(0, first_trial + 8), first_aimdev, rep(first_aimdev, 32 - first_trial)))  # 0, jump, then constant
  ) %>%
  unnest(c(x, y))