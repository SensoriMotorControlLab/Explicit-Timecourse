hist2d <- function(x, y=NA, nbins=c(25,25), edges=NA) {
  
  if (is.data.frame(x)) {
    # check shape of x?
    df <- x
  } else if (is.matrix(x)) {
    # check shape of x?
    df <- as.data.frame(x)
  } else {
    df <- data.frame('x'=x, 'y'=y)
  }
  
  # code below, somewhat based on:
  # http://stackoverflow.com/questions/18089752/r-generate-2d-histogram-from-raw-data
  
  if (is.numeric(nbins)) {
    x.edges <- seq(floor(min(df[,1])), ceiling(max(df[,1])), length=nbins[1])
    y.edges <- seq(floor(min(df[,2])), ceiling(max(df[,2])), length=nbins[2])
  }
  
  if (is.list(edges)) {
    x.edges <- edges[[1]]
    y.edges <- edges[[2]]
  }
  
  xbincount <- findInterval(df[,1], x.edges, rightmost.closed = T, left.open = F, all.inside = F)
  ybincount <- findInterval(df[,2], y.edges, rightmost.closed = T, left.open = F, all.inside = F)
  xbincount <- factor(xbincount, levels=c(1:(length(x.edges)-1)))
  ybincount <- factor(ybincount, levels=c(1:(length(y.edges)-1)))
  
  freq2D <- as.matrix(table(xbincount,ybincount))
  dimnames( freq2D ) <- c()
  rownames( freq2D ) <- c()
  colnames( freq2D ) <- c()
  
  return(list('freq2D'=freq2D, 'x.edges'=x.edges, 'y.edges'=y.edges))
  
}

#create data frame

last_8_aligned <- total_group_data[
  (total_group_data$cutrial_no %in% 81:88 & total_group_data$group == 'Group 1') |
    (total_group_data$cutrial_no %in% 105:112 & total_group_data$group == 'Group 2'),]

last_8_aligned <- last_8_aligned %>%
  group_by(group, participant_id) %>%  # adjust 'participant_id' to your actual column name
  mutate(time = -8:-1) %>%
  ungroup()

dfAligned <- data.frame(
  x = last_8_aligned$time,
  y = last_8_aligned$aimdeviation_deg,
  rotation_group = last_8_aligned$rotation
)


first_32_rotated <- total_group_data[
  (total_group_data$cutrial_no %in% 89:120 & total_group_data$group == 'Group 1') |
    (total_group_data$cutrial_no %in% 105:136 & total_group_data$group == 'Group 2'),]

first_32_rotated <- first_32_rotated %>%
  group_by(group, participant_id) %>%
  mutate(time = 0:31) %>%
  ungroup()

dfRotated <- data.frame(
  x = first_32_rotated$time,
  y= first_32_rotated$aimdeviation_deg,
  rotation_group = first_32_rotated$rotation
)

all_hist_data <- rbind(dfAligned, dfRotated)


#60 aiming histogram
plot60aim <- function () {
  aim_60_hist <- all_hist_data %>%
  filter(rotation_group == '60')

  plot(NA,
       main='Explicit Learning With a 60° Rotation',
       xlab='Trial', ylab='Aim Deviation (°)',
       xlim=c(-8,32), ylim=c(-15,60), 
       ax=F, bty='n')
  

  img_info <- hist2d(x=aim_60_hist, nbins=NA, edges=list(seq(-8,31.5,1), seq(-15,60,2.5)))
  img <- log(img_info$freq2D + 1)
  
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        col=colorRampPalette(c("white", "lavender", "slateblue", "navy"))(100),
        z=img,
        add=TRUE)
  
  axis(side=1, at=c(-8, 0,8, 16, 24, 32), labels=c(-8, 0,8, 16, 24, 32))
  axis(side=2, at=seq(-10,60,10))
  lines(x=c(-8, 0, 0, 32), 
        y=c(-0.5, -0.5, 59.5, 59.5), 
        col='navy', lty=3, lwd=2)
avg_aim60 <- aggregate(y ~ x, data=aim_60_hist, FUN=mean)
lines(avg_aim60$x, avg_aim60$y, col="hotpink", lwd=2)
text(x = -8, y = 63, labels = "n = 13", adj = c(0,2), col = "black", cex = 1)
}

#50 aiming histogram
plot50aim <- function () {
  aim_50_hist <- all_hist_data %>%
  filter(rotation_group == '50')

# Plotting
plot(NA,
     main='Explicit Learning With a 50° Rotation',
     xlab='Trial', ylab='Aim Deviation (°)',
     xlim=c(-8,32), ylim=c(-15,50), 
     ax=F, bty='n')

img_info <- hist2d(x=aim_50_hist, nbins=NA, edges=list(seq(-8,31.5,1), seq(-15,50,2.5)))
img <- log(img_info$freq2D + 1)


image(x=img_info$x.edges,
      y=img_info$y.edges,
      col=colorRampPalette(c("white", "lavender", "slateblue", "navy"))(100),
      z=img,
      add=TRUE)

axis(side=1, at=c(-8, 0,8, 16, 24, 32), labels=c(-8, 0,8, 16, 24, 32))
axis(side=2, at=seq(-10,50,10))
lines(x=c(-8, 0, 0, 32), 
      y=c(-0.5, -0.5, 49.5, 49.5), 
      col='navy', lty=3, lwd=2)

avg_aim50 <- aggregate(y ~ x, data=aim_50_hist, FUN=mean)
lines(avg_aim60$x, avg_aim50$y, col="hotpink", lwd=2)
text(x = -8, y = 53, labels = "n = 13", adj = c(0,2), col = "black", cex = 1)
}

#40 aiming histogram

plot40aim <- function () {
  aim_40_hist <- all_hist_data %>%
  filter(rotation_group == '40')

# Plotting
plot(NA,
     main='Explicit Learning With a 40° Rotation',
     xlab='Trial', ylab='Aim Deviation (°)',
     xlim=c(-8,32), ylim=c(-15,40), 
     ax=F, bty='n')

img_info <- hist2d(x=aim_40_hist, nbins=NA, edges=list(seq(-8,31.5,1), seq(-15,40,2.5)))
img <- log(img_info$freq2D + 1)


image(x=img_info$x.edges,
      y=img_info$y.edges,
      col=colorRampPalette(c("white", "lavender", "slateblue", "navy"))(100),
      z=img,
      add=TRUE)

axis(side=1, at=c(-8, 0,8, 16, 24, 32), labels=c(-8, 0,8, 16, 24, 32))
axis(side=2, at=seq(-10,60,10))
lines(x=c(-8, 0, 0, 32), 
      y=c(-0.5, -0.5, 39.5, 39.5), 
      col='navy', lty=3, lwd=2)

avg_aim40 <- aggregate(y ~ x, data=aim_40_hist, FUN=mean)
lines(avg_aim40$x, avg_aim40$y, col="hotpink", lwd=2)
text(x = -8, y = 43, labels = "n = 14", adj = c(0,2), col = "black", cex = 1)
}







######JUST STRATEGY USERS ######


last_8_aligned_strategy <- strategy_only_participants[
  (strategy_only_participants$cutrial_no %in% 81:88 & strategy_only_participants$group == 'Group 1') |
    (strategy_only_participants$cutrial_no %in% 105:112 & strategy_only_participants$group == 'Group 2'),]

last_8_aligned_strategy <- last_8_aligned_strategy %>%
  group_by(group, participant_id) %>%  # adjust 'participant_id' to your actual column name
  mutate(time = -8:-1) %>%
  ungroup()

dfAligned_strategy <- data.frame(
  x = last_8_aligned_strategy$time,
  y = last_8_aligned_strategy$aimdeviation_deg,
  rotation_group = last_8_aligned_strategy$rotation
)


first_32_rotated_strategy <- strategy_only_participants[
  (strategy_only_participants$cutrial_no %in% 89:120 & strategy_only_participants$group == 'Group 1') |
    (strategy_only_participants$cutrial_no %in% 105:136 & strategy_only_participants$group == 'Group 2'),]

first_32_rotated_strategy <- first_32_rotated_strategy %>%
  group_by(group, participant_id) %>%
  mutate(time = 0:31) %>%
  ungroup()

dfRotated_strategy <- data.frame(
  x = first_32_rotated_strategy$time,
  y= first_32_rotated_strategy$aimdeviation_deg,
  rotation_group = first_32_rotated_strategy$rotation
)

all_hist_strategy_data <- rbind(dfAligned_strategy, dfRotated_strategy)


#aiming 60 
aim_60_strategy_hist <- all_hist_strategy_data %>%
  filter(rotation_group == '60' & participant_id != '98e5cb')

# Plotting
plot(NA,
     main='Explicit Learning With a 60° Rotation',
     xlab='Trial', ylab='Aim Deviation (°)',
     xlim=c(-8,32), ylim=c(-15,60), 
     ax=F, bty='n')


img_info <- hist2d(x=aim_60_strategy_hist, nbins=NA, edges=list(seq(-8,31.5,1), seq(-15,60,2.5)))
img <- log(img_info$freq2D + 1)

image(x=img_info$x.edges,
      y=img_info$y.edges,
      col=colorRampPalette(c("white", "lavender", "slateblue", "navy"))(100),
      z=img,
      add=TRUE)

axis(side=1, at=c(-8, 0,8, 16, 24, 32), labels=c(-8, 0,8, 16, 24, 32))
axis(side=2, at=seq(-10,60,10))
lines(x=c(-8, 0, 0, 32), 
      y=c(-0.5, -0.5, 59.5, 59.5), 
      col='navy', lty=3, lwd=2)
avg_strategy60 <- aggregate(y ~ x, data=aim_60_strategy_hist, FUN=mean)
lines(avg_strategy60$x, avg_strategy60$y, col="hotpink", lwd=2)

#aiming 50 hist strategy users


aim_50_strategy_hist <- all_hist_strategy_data %>%
  filter(rotation_group == '50')

plot(NA,
     main='Explicit Learning With a 50° Rotation',
     xlab='Trial', ylab='Aim Deviation (°)',
     xlim=c(-8,32), ylim=c(-15,50), 
     ax=F, bty='n')


img_info <- hist2d(x=aim_50_strategy_hist, nbins=NA, edges=list(seq(-8,31.5,1), seq(-15,50,2.5)))
img <- log(img_info$freq2D + 1)

image(x=img_info$x.edges,
      y=img_info$y.edges,
      col=colorRampPalette(c("white", "lavender", "slateblue", "navy"))(100),
      z=img,
      add=TRUE)

axis(side=1, at=c(-8, 0,8, 16, 24, 32), labels=c(-8, 0,8, 16, 24, 32))
axis(side=2, at=seq(-10,50,10))
lines(x=c(-8, 0, 0, 32), 
      y=c(-0.5, -0.5, 49.5, 49.5), 
      col='navy', lty=3, lwd=2)

avg_strategy50 <- aggregate(y ~ x, data=aim_50_strategy_hist, FUN=mean)
lines(avg_strategy50$x, avg_strategy50$y, col="hotpink", lwd=2)

#40 strategy user histogram
aim_40_strategy_hist <- all_hist_strategy_data %>%
  filter(rotation_group == '40')

plot(NA,
     main='Explicit Learning With a 40° Rotation',
     xlab='Trial', ylab='Aim Deviation (°)',
     xlim=c(-8,32), ylim=c(-15,40), 
     ax=F, bty='n')


img_info <- hist2d(x=aim_40_strategy_hist, nbins=NA, edges=list(seq(-8,31.5,1), seq(-15,40,2.5)))
img <- log(img_info$freq2D + 1)

image(x=img_info$x.edges,
      y=img_info$y.edges,
      col=colorRampPalette(c("white", "lavender", "slateblue", "navy"))(100),
      z=img,
      add=TRUE)

axis(side=1, at=c(-8, 0,8, 16, 24, 32), labels=c(-8, 0,8, 16, 24, 32))
axis(side=2, at=seq(-10,40,10))
lines(x=c(-8, 0, 0, 32), 
      y=c(-0.5, -0.5, 39.5, 39.5), 
      col='navy', lty=3, lwd=2)

avg_strategy40 <- aggregate(y ~ x, data=aim_40_strategy_hist, FUN=mean)
lines(avg_strategy40$x, avg_strategy40$y, col="hotpink", lwd=2)



plot(NA, type='n',
     xlim = c(-8, 32),
     ylim = c(-10, 60),
     xlab = "Trial",
     ylab = "Aim Deviation (°)",
     main = "Explicit Learning Per Rotation")


lines(avg_strategy60$x, avg_strategy60$y, col = "orange", lwd = 2)
lines(avg_strategy50$x, avg_strategy50$y, col = "cyan", lwd = 2)
lines(avg_strategy40$x, avg_strategy40$y, col = "hotpink", lwd = 2)

abline(v = 0, col = 'navy', lty = 3, lwd = 2)


legend("topright", legend = c("60°", "50°", "40°"),
       col = c("orange", "cyan", "hotpink"), lwd = 2, bty = "n")

text(x = 0, y = -12, labels = "Rotation Starts", pos = (3), cex = 0.8)




#########REACH DEVIATION#######

dfAlignedReach <- data.frame(
  x = last_8_aligned$time,
  y = last_8_aligned$reachdeviation_deg,
  rotation_group = last_8_aligned$rotation
)

dfRotatedReach <- data.frame(
  x = first_32_rotated$time,
  y= first_32_rotated$reachdeviation_deg ,
  rotation_group = first_32_rotated$rotation
)

all_hist_data_reach <- rbind(dfAlignedReach, dfRotatedReach)


#60 reach histogram
plot60reach <- function () {
  aim_60_hist_reach <- all_hist_data_reach %>%
  filter(rotation_group == '60')

plot(NA,
     main='Reach Deviation With a 60° Rotation',
     xlab='Trial', ylab='Aim Deviation (°)',
     xlim=c(-8,32), ylim=c(-15,60), 
     ax=F, bty='n')


img_info <- hist2d(x=aim_60_hist_reach, nbins=NA, edges=list(seq(-8,31.5,1), seq(-15,60,2.5)))
img <- log(img_info$freq2D + 1)

image(x=img_info$x.edges,
      y=img_info$y.edges,
      col=colorRampPalette(c("white", "#d5d5d5", "#858f94", "#49525e"))(100),
      z=img,
      add=TRUE)

axis(side=1, at=c(-8, 0,8, 16, 24, 32), labels=c(-8, 0,8, 16, 24, 32))
axis(side=2, at=seq(-10,60,10))
lines(x=c(-8, 0, 0, 32), 
      y=c(-0.5, -0.5, 59.5, 59.5), 
      col='navy', lty=3, lwd=2)
avg_aim60r <- aggregate(y ~ x, data=aim_60_hist_reach, FUN=mean)
lines(avg_aim60r$x, avg_aim60r$y, col="#a93154", lwd=2)
text(x = -8, y = 63, labels = "n = 13", adj = c(0,2), col = "black", cex = 1)
}

#reach 50
plot50reach <- function () {
  aim_50_hist_reach <- all_hist_data_reach %>%
  filter(rotation_group == '50')

plot(NA,
     main='Reach Deviation With a 50° Rotation',
     xlab='Trial', ylab='Aim Deviation (°)',
     xlim=c(-8,32), ylim=c(-15,50), 
     ax=F, bty='n')


img_info <- hist2d(x=aim_50_hist_reach, nbins=NA, edges=list(seq(-8,31.5,1), seq(-15,50,2.5)))
img <- log(img_info$freq2D + 1)

image(x=img_info$x.edges,
      y=img_info$y.edges,
      col=colorRampPalette(c("white", "#d5d5d5", "#858f94", "#49525e"))(100),
      z=img,
      add=TRUE)

axis(side=1, at=c(-8, 0,8, 16, 24, 32), labels=c(-8, 0,8, 16, 24, 32))
axis(side=2, at=seq(-10,50,10))
lines(x=c(-8, 0, 0, 32), 
      y=c(-0.5, -0.5, 49.5, 49.5), 
      col='navy', lty=3, lwd=2)
avg_aim50r <- aggregate(y ~ x, data=aim_50_hist_reach, FUN=mean)
lines(avg_aim50r$x, avg_aim50r$y, col="#a93154", lwd=2)
text(x = -8, y = 63, labels = "n = 13", adj = c(0,2), col = "black", cex = 1)
}

#reach 40

plot40reach <- function() {
  aim_40_hist_reach <- all_hist_data_reach %>%
  filter(rotation_group == '40')

plot(NA,
     main='Reach Deviation With a 40° Rotation',
     xlab='Trial', ylab='Aim Deviation (°)',
     xlim=c(-8,32), ylim=c(-15,40), 
     ax=F, bty='n')


img_info <- hist2d(x=aim_50_hist_reach, nbins=NA, edges=list(seq(-8,31.5,1), seq(-15,40,2.5)))
img <- log(img_info$freq2D + 1)

image(x=img_info$x.edges,
      y=img_info$y.edges,
      col=colorRampPalette(c("#ffffff", "#d5d5d5", "#858f94", "#49525e"))(100),
      z=img,
      add=TRUE)

axis(side=1, at=c(-8, 0,8, 16, 24, 32), labels=c(-8, 0,8, 16, 24, 32))
axis(side=2, at=seq(-10,40,10))
lines(x=c(-8, 0, 0, 32), 
      y=c(-0.5, -0.5, 39.5, 39.5), 
      col='navy', lty=3, lwd=2)
avg_aim40r <- aggregate(y ~ x, data=aim_40_hist_reach, FUN=mean)
lines(avg_aim40r$x, avg_aim40r$y, col="#a93154", lwd=2)
text(x = -8, y = 63, labels = "n = 13", adj = c(0,2), col = "black", cex = 1)
}









