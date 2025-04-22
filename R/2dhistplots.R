get60_Data <- function() {
  aim60_path <- 'data/Instructed_summary/aiming60'
  all_60data <- list()
  aim60files <- list.files(aim60_path, pattern = "*.csv", full.names = TRUE)
  
  for (file_path in aim60files) {
    df <- read.csv(file_path)
    all_60data[[length(all_60data) + 1]] <- df
  }
  
  return(all_60data)
}

aim60_data <- get60_Data()
print(length(aim60_data))


#from reaches60 script
for (file in group1_files) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  df$cutrial_no <- as.integer(df$cutrial_no)
  aligned1 <- df[df$cutrial_no >= 1 & df$cutrial_no <= 88, c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
  group1_data[[length(group1_data) + 1]] <- aligned1
}
combined_g1_aligned60 <- do.call(rbind, group1_data)

# trials for Group 2 (Trial 113 to 232)
for (file in group2_files) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  aligned2 <- df[df$cutrial_no %in% c(1:24, 41:56, 65:72, 81:88, 97:112), c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
  group2_data[[length(group2_data) + 1]] <- aligned2
}

combined_g2_aligned60 <- do.call(rbind, group2_data)
######

getCombinedAlignedRotated60 <- function(all_data60) {
  data_path <- "data/Instructed_summary/aiming60/"
  
  aligned_g1 <-   combined_g1_aligned60[which(  combined_g1_aligned60$cutrial_no %in% 81:88),]
  rotated_g1 <- combined_g1_rotated60[which(combined_g1_rotated60$cutrial_no %in% 89:121),]
  
  aligned_g1_combined <- data.frame(
    x = aligned_g1$cutrial_no - 89,
    cutrial_no = aligned_g1$cutrial_no,  # Trial numbers
    aimdeviation_deg = aligned_g1$aimdeviation_deg  # Aim deviation values
  )
  

 
  rotated_g1_combined <- data.frame( x = rotated_g1$cutrial_no - 89,
                                     cutrial_no=rotated_g1$cutrial_no, aimdeviation_deg = 
                                  rotated_g1$aimdeviation_deg)
  
  ###
  aligned_g2 <- combined_g2_aligned60[combined_g2_aligned60$cutrial_no %in% 105:112,]
  rotated_g2 <-  combined_g2_rotated60[combined_g2_rotated60$cutrial_no %in% 113:144,]
  
  aligned_g2_combined <- data.frame(
    x = aligned_g2$cutrial_no - 113,
      cutrial_no = aligned_g2$cutrial_no,  
    aimdeviation_deg = aligned_g2$aimdeviation_deg)
  
  
  rotated_g2_combined <- data.frame(
    x = rotated_g2$cutrial_no - 113,cutrial_no=rotated_g2$cutrial_no, aimdeviation_deg = 
                                      rotated_g2$aimdeviation_deg)
  
  g1_60 <- data.frame(
    x = c(aligned_g1_combined$x, rotated_g1_combined$x),  # Combine x values from both phases
    aimdeviation_deg = c(aligned_g1_combined$aimdeviation_deg, rotated_g1_combined$aimdeviation_deg)  # Combine aim deviation values
  )
  g2_60 <- data.frame(
    x = c(aligned_g2_combined$x, rotated_g2_combined$x),  # Combine x values from both phases
    aimdeviation_deg = c(aligned_g2_combined$aimdeviation_deg, rotated_g2_combined$aimdeviation_deg)  # Combine aim deviation values
  )
  
  combined60 <- rbind(g1_60,g2_60)
  
  return(combined60)
}





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

plot60hist <- function() {
expl <- combined60

# Rename for plotting
names(expl) <- c('x', 'y')
# Plotting
plot(NA,
     main='Explicit Learning With a 60° Rotation',
     xlab='Trial', ylab='Aim Deviation (°)',
     xlim=c(-8,32), ylim=c(-15,60), 
     ax=F, bty='n')

# 2D histogram of the data (aimdeviation_deg vs cutrial_no)
img_info <- hist2d(x=expl, nbins=NA, edges=list(seq(-8,31.5,1), seq(-15,60,2.5)))
img <- log(img_info$freq2D + 1)

# Plot the image
image(x=img_info$x.edges,
      y=img_info$y.edges,
     # col=colorRampPalette(c("white", "plum2", "darkorchid4", "black"))(100),
      z=img,
      add=TRUE)


# Add legend for the rotation start points
#legend("bottomright", legend = c("Rotation Start for Old Paradigm", "Rotation Start for New Paradigm"),
      # col = c("black", "blue"), lty = 1, lwd = 2)

# Customize the axis
axis(side=1, at=c(-8, 0,8, 16, 24, 32), labels=c(-8, 0,8, 16, 24, 32))
axis(side=2, at=seq(-10,60,10))
lines(x=c(-8, 0, 0, 32), 
      y=c(-0.5, -0.5, 59.5, 59.5), 
      col='navy', lty=3, lwd=2)

avg_aim60 <- aggregate(aimdeviation_deg ~ x, data=combined60, FUN=mean)
lines(avg_aim60$x, avg_aim60$aimdeviation_deg, col="blue", lwd=2)
}



#50 DEGREE

get50_Data <- function() {
  aim50_path <- 'data/Instructed_summary/aiming50'
  all_50data <- list()
  aim50files <- list.files(aim50_path, pattern = "*.csv", full.names = TRUE)
  
  for (file_path in aim50files) {
    df <- read.csv(file_path)
    all_50data[[length(all_50data) + 1]] <- df
  }
  
  return(all_50data)
}

aim50_data <- get50_Data()

getCombinedAlignedRotated50 <- function(all_data50) {
  data_path <- "data/Instructed_summary/aiming50/"
  
  aligned_g150 <-   combined_g1_aligned50[which(combined_g1_aligned50$cutrial_no %in% 81:88),]
  rotated_g150 <- combined_g1_rotated50[which(combined_g1_rotated50$cutrial_no %in% 89:121),]
  
  aligned_g1_combined50 <- data.frame(
    x = aligned_g150$cutrial_no - 89,
    cutrial_no = aligned_g150$cutrial_no,  # Trial numbers
    aimdeviation_deg = aligned_g150$aimdeviation_deg  # Aim deviation values
  )
  
  
  
  rotated_g1_combined50 <- data.frame( x = rotated_g150$cutrial_no - 89,
                                     cutrial_no=rotated_g150$cutrial_no, aimdeviation_deg = 
                                       rotated_g150$aimdeviation_deg)
  
  ###
  aligned_g250 <- combined_g2_aligned50[combined_g2_aligned50$cutrial_no %in% 105:112,]
  rotated_g250 <-  combined_g2_rotated50[combined_g2_rotated50$cutrial_no %in% 113:144,]
  
  aligned_g2_combined50 <- data.frame(
    x = aligned_g250$cutrial_no - 113,
    cutrial_no = aligned_g250$cutrial_no,  
    aimdeviation_deg = aligned_g250$aimdeviation_deg)
  
  
  rotated_g2_combined50 <- data.frame(
    x = rotated_g250$cutrial_no - 113,cutrial_no=rotated_g250$cutrial_no, aimdeviation_deg = 
      rotated_g250$aimdeviation_deg)
  
  g1_50 <- data.frame(
    x = c(aligned_g1_combined50$x, rotated_g1_combined50$x),  # Combine x values from both phases
    aimdeviation_deg = c(aligned_g1_combined50$aimdeviation_deg, rotated_g1_combined50$aimdeviation_deg)  # Combine aim deviation values
  )
  g2_50 <- data.frame(
    x = c(aligned_g2_combined50$x, rotated_g2_combined50$x),  # Combine x values from both phases
    aimdeviation_deg = c(aligned_g2_combined50$aimdeviation_deg, rotated_g2_combined50$aimdeviation_deg)  # Combine aim deviation values
  )
  
  combined50 <- rbind(g1_50,g2_50)
  
  return(combined50)
}


plot50hist <- function() {
  exp2 <- combined50
  
  # Rename for plotting
  names(exp2) <- c('x', 'y')
  # Plotting
  plot(NA,
       main='Explicit Learning With a 50° Rotation',
       xlab='Trial', ylab='Aim Deviation (°)',
       xlim=c(-8,32), ylim=c(-15,50), 
       ax=F, bty='n')
  
  # 2D histogram of the data (aimdeviation_deg vs cutrial_no)
  img_info <- hist2d(x=exp2, nbins=NA, edges=list(seq(-8,31.5,1), seq(-15,50,2.5)))
  img <- log(img_info$freq2D + 1)
  
  # Plot the image
  image(x=img_info$x.edges,
        y=img_info$y.edges,
       # col=colorRampPalette(c("white", "pink", "deeppink", "maroon4"))(100),
        z=img,
        add=TRUE)
  
  axis(side=1, at=c(-8, 0,8, 16, 24, 32), labels=c(-8, 0,8, 16, 24, 32))
  axis(side=2, at=seq(-10,50,10))
  lines(x=c(-8, 0, 0, 32), 
        y=c(-0.5, -0.5, 49.5, 49.5), 
        col='navy', lty=3, lwd=2)
  
}
  

#40 DEGREE

get40_Data <- function() {
  aim40_path <- 'data/Instructed_summary/aiming40'
  all_40data <- list()
  aim40files <- list.files(aim40_path, pattern = "*.csv", full.names = TRUE)
  
  for (file_path in aim40files) {
    df <- read.csv(file_path)
    all_40data[[length(all_40data) + 1]] <- df
  }
  
  return(all_40data)
}

aim40_data <- get40_Data()
print(length(aim40_data))

getCombinedAlignedRotated40 <- function(all_data40) {
  data_path <- "data/Instructed_summary/aiming40/"
  
  aligned_g140 <-   combined_g1_aligned40[which(combined_g1_aligned40$cutrial_no %in% 81:88),]
  rotated_g140 <- combined_g1_rotated40[which(combined_g1_rotated40$cutrial_no %in% 89:121),]
  
  aligned_g1_combined40 <- data.frame(
    x = aligned_g140$cutrial_no - 89,
    cutrial_no = aligned_g140$cutrial_no,  # Trial numbers
    aimdeviation_deg = aligned_g140$aimdeviation_deg  # Aim deviation values
  )
  
  
  
  rotated_g1_combined40 <- data.frame( x = rotated_g140$cutrial_no - 89,
                                       cutrial_no=rotated_g140$cutrial_no, aimdeviation_deg = 
                                         rotated_g140$aimdeviation_deg)
  
  ###
  aligned_g240 <- combined_g2_aligned40[combined_g2_aligned40$cutrial_no %in% 105:112,]
  rotated_g240 <-  combined_g2_rotated40[combined_g2_rotated40$cutrial_no %in% 113:144,]
  
  aligned_g2_combined40 <- data.frame(
    x = aligned_g240$cutrial_no - 113,
    cutrial_no = aligned_g240$cutrial_no,  
    aimdeviation_deg = aligned_g240$aimdeviation_deg)
  
  
  rotated_g2_combined40 <- data.frame(
    x = rotated_g240$cutrial_no - 113,cutrial_no=rotated_g240$cutrial_no, aimdeviation_deg = 
      rotated_g240$aimdeviation_deg)
  
  g1_40 <- data.frame(
    x = c(aligned_g1_combined40$x, rotated_g1_combined40$x),  # Combine x values from both phases
    aimdeviation_deg = c(aligned_g1_combined40$aimdeviation_deg, rotated_g1_combined40$aimdeviation_deg)  # Combine aim deviation values
  )
  g2_40 <- data.frame(
    x = c(aligned_g2_combined40$x, rotated_g2_combined40$x),  # Combine x values from both phases
    aimdeviation_deg = c(aligned_g2_combined40$aimdeviation_deg, rotated_g2_combined40$aimdeviation_deg)  # Combine aim deviation values
  )
  
  combined40 <- rbind(g1_40,g2_40)
  
  return(combined50)
}


plot40hist <- function() {
  exp3 <- combined40
  
  # Rename for plotting
  names(exp3) <- c('x', 'y')
  # Plotting
  plot(NA,
       main='Explicit Learning With a 40° Rotation',
       xlab='Trial', ylab='Aim Deviation (°)',
       xlim=c(-8,32), ylim=c(-15,40), 
       ax=F, bty='n')
  
  # 2D histogram of the data (aimdeviation_deg vs cutrial_no)
  img_info <- hist2d(x=exp3, nbins=NA, edges=list(seq(-8,31.5,1), seq(-15,40,2.5)))
  img <- log(img_info$freq2D + 1)
  
  # Plot the image
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        z=img,
        add=TRUE)
  
  axis(side=1, at=c(-8, 0,8, 16, 24, 32), labels=c(-8, 0,8, 16, 24, 32))
  axis(side=2, at=seq(-10,40,10))
  lines(x=c(-8, 0, 0, 32), 
        y=c(-0.5, -0.5, 39.5, 39.5), 
        col='navy', lty=1, lwd=2)
  
}


###30 Degree

get30_Data <- function() {
  aim30_path <- 'data/Instructed_summary/aiming30'
  all_30data <- list()
  aim30files <- list.files(aim30_path, pattern = "*.csv", full.names = TRUE)
  
  for (file_path in aim30files) {
    df <- read.csv(file_path)
    all_30data[[length(all_30data) + 1]] <- df
  }
  
  return(all_30data)
}

aim30_data <- get30_Data()
print(length(aim30_data))

getCombinedAlignedRotated30 <- function(all_data30) {
  data_path <- "data/Instructed_summary/aiming30/"
  
  ###
  aligned_g230 <- combined_g2_aligned30[combined_g2_aligned30$cutrial_no %in% 105:112,]
  rotated_g230 <-  combined_g2_rotated30[combined_g2_rotated30$cutrial_no %in% 113:144,]
  
  aligned_g2_combined30 <- data.frame(
    x = aligned_g230$cutrial_no - 113,
    cutrial_no = aligned_g230$cutrial_no,  
    aimdeviation_deg = aligned_g230$aimdeviation_deg)
  
  
  rotated_g2_combined30 <- data.frame(
    x = rotated_g230$cutrial_no - 113,cutrial_no=rotated_g230$cutrial_no, aimdeviation_deg = 
      rotated_g230$aimdeviation_deg)
  

  g2_30 <- data.frame(
    x = c(aligned_g2_combined30$x, rotated_g2_combined30$x),  # Combine x values from both phases
    aimdeviation_deg = c(aligned_g2_combined30$aimdeviation_deg, rotated_g2_combined30$aimdeviation_deg)  # Combine aim deviation values
  )
  
  combined30 <- rbind(g2_30)
  
  return(combined30)
}


plot30hist <- function() {
  exp4 <- combined30
  
  # Rename for plotting
  names(exp4) <- c('x', 'y')
  # Plotting
  plot(NA,
       main='Explicit Learning With a 30° Rotation',
       xlab='Trial', ylab='Aim Deviation (°)',
       xlim=c(-8,32), ylim=c(-15,30), 
       ax=F, bty='n')
  
  # 2D histogram of the data (aimdeviation_deg vs cutrial_no)
  img_info <- hist2d(x=exp4, nbins=NA, edges=list(seq(-8,31.5,1.2), seq(-15,30,2.5)))
  img <- log(img_info$freq2D + 1)
  
  # Plot the image
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        #col=colorRampPalette(c("white", "lavender", "slateblue", "navy"))(100),
        z=img,
        add=TRUE)
  
  axis(side=1, at=c(-8, 0,8, 16, 24, 32), labels=c(-8, 0,8, 16, 24, 32))
  axis(side=2, at=seq(-10,30,10))
  lines(x=c(-8, 0, 0, 32), 
        y=c(-0.5, -0.5, 29.5, 29.5), 
        col='navy', lty=3, lwd=2)
  
}



#20 degree

get20_Data <- function() {
  aim20_path <- 'data/Instructed_summary/aiming20'
  all_20data <- list()
  aim20files <- list.files(aim20_path, pattern = "*.csv", full.names = TRUE)
  
  for (file_path in aim20files) {
    df <- read.csv(file_path)
    all_20data[[length(all_20data) + 1]] <- df
  }
  
  return(all_20data)
}

aim20_data <- get20_Data()
print(length(aim20_data))


getCombinedAlignedRotated20 <- function(all_data20) {
  data_path <- "data/Instructed_summary/aiming20/"
  
  ###
  aligned_g220 <- combined_g2_aligned20[combined_g2_aligned20$cutrial_no %in% 105:112,]
  rotated_g220 <-  combined_g2_rotated20[combined_g2_rotated20$cutrial_no %in% 113:144,]
  
  aligned_g2_combined20 <- data.frame(
    x = aligned_g220$cutrial_no - 113,
    cutrial_no = aligned_g220$cutrial_no,  
    aimdeviation_deg = aligned_g220$aimdeviation_deg)
  
  
  rotated_g2_combined20 <- data.frame(
    x = rotated_g220$cutrial_no - 113,cutrial_no=rotated_g220$cutrial_no, aimdeviation_deg = 
      rotated_g220$aimdeviation_deg)
  
  
  g2_20 <- data.frame(
    x = c(aligned_g2_combined20$x, rotated_g2_combined20$x),  # Combine x values from both phases
    aimdeviation_deg = c(aligned_g2_combined20$aimdeviation_deg, rotated_g2_combined20$aimdeviation_deg)  # Combine aim deviation values
  )
  
  combined20 <- rbind(g2_20)
  
  return(combined20)
}


plot20hist <- function() {
  exp5 <- combined20
  
  # Rename for plotting
  names(exp5) <- c('x', 'y')
  # Plotting
  plot(NA,
       main='Explicit Learning With a 20° Rotation',
       xlab='Trial', ylab='Aim Deviation (°)',
       xlim=c(-8,32), ylim=c(-15,20), 
       ax=F, bty='n')
  
  # 2D histogram of the data (aimdeviation_deg vs cutrial_no)
  img_info <- hist2d(x=exp5, nbins=NA, edges=list(seq(-8,31.5,1.2), seq(-15,20,2.5)))
  img <- log(img_info$freq2D + 1)
  
  # Plot the image
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        z=img,
        add=TRUE)
  
  axis(side=1, at=c(-8, 0,8, 16, 24, 32), labels=c(-8, 0,8, 16, 24, 32))
  axis(side=2, at=seq(-10,20,10))
  lines(x=c(-8, 0, 0, 32), 
        y=c(-0.5, -0.5, 19.5, 19.5), 
        col='navy', lty=1, lwd=2)
  
}