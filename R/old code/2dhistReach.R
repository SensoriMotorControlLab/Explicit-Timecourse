get60_Data <- function() {
  reach60_path <- 'data/Instructed_summary/reaching60'
  all_60data <- list()
  reach60files <- list.files(reach60_path, pattern = "*.csv", full.names = TRUE)
  
  for (file_path in reach60files) {
    df <- read.csv(file_path)
    all_60data[[length(all_60data) + 1]] <- df
  }
  
  return(all_60data)
}

reach60_data <- get60_Data()
print(length(reach60_data))


#from reaches60 script
for (file in group1_files) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  df$cutrial_no <- as.integer(df$cutrial_no)
  aligned1 <- df[df$cutrial_no >= 1 & df$cutrial_no <= 88, c("cutrial_no", "reachdeviation_deg", "reachdeviation_deg"), drop = FALSE]
  group1_data[[length(group1_data) + 1]] <- aligned1
}
combined_g1_aligned60 <- do.call(rbind, group1_data)

# trials for Group 2 (Trial 113 to 232)
for (file in group2_files) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  aligned2 <- df[df$cutrial_no %in% c(1:24, 41:56, 65:72, 81:88, 97:112), c("cutrial_no", "reachdeviation_deg", "reachdeviation_deg"), drop = FALSE]
  group2_data[[length(group2_data) + 1]] <- aligned2
}

combined_g2_aligned60 <- do.call(rbind, group2_data)
######

getCombinedAlignedRotated60 <- function(all_data60) {
  data_path <- "data/Instructed_summary/reaching60/"
  
  aligned_g1 <-   combined_g1_aligned60[which(  combined_g1_aligned60$cutrial_no %in% 81:88),]
  rotated_g1 <- combined_g1_rotated60[which(combined_g1_rotated60$cutrial_no %in% 89:121),]
  
  aligned_g1_combined <- data.frame(
    x = aligned_g1$cutrial_no - 89,
    cutrial_no = aligned_g1$cutrial_no,  # Trial numbers
    reachdeviation_deg = aligned_g1$reachdeviation_deg  # reach deviation values
  )
  
  
  
  rotated_g1_combined <- data.frame( x = rotated_g1$cutrial_no - 89,
                                     cutrial_no=rotated_g1$cutrial_no, reachdeviation_deg = 
                                       rotated_g1$reachdeviation_deg)
  
  ###
  aligned_g2 <- combined_g2_aligned60[combined_g2_aligned60$cutrial_no %in% 105:112,]
  rotated_g2 <-  combined_g2_rotated60[combined_g2_rotated60$cutrial_no %in% 113:144,]
  
  aligned_g2_combined <- data.frame(
    x = aligned_g2$cutrial_no - 113,
    cutrial_no = aligned_g2$cutrial_no,  
    reachdeviation_deg = aligned_g2$reachdeviation_deg)
  
  
  rotated_g2_combined <- data.frame(
    x = rotated_g2$cutrial_no - 113,cutrial_no=rotated_g2$cutrial_no, reachdeviation_deg = 
      rotated_g2$reachdeviation_deg)
  
  g1_60REACH <- data.frame(
    x = c(aligned_g1_combined$x, rotated_g1_combined$x),  # Combine x values from both phases
    reachdeviation_deg = c(aligned_g1_combined$reachdeviation_deg, rotated_g1_combined$reachdeviation_deg)  # Combine reach deviation values
  )
  g2_60REACH <- data.frame(
    x = c(aligned_g2_combined$x, rotated_g2_combined$x),  # Combine x values from both phases
    reachdeviation_deg = c(aligned_g2_combined$reachdeviation_deg, rotated_g2_combined$reachdeviation_deg)  # Combine reach deviation values
  )
  
  combined60REACH <- rbind(g1_60REACH,g2_60REACH)
  
  return(combined60REACH)
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

plot60histREACH <- function() {
  explREACH <- combined60REACH
  
  # Rename for plotting
  names(explREACH) <- c('x', 'y')
  # Plotting
  plot(NA,
       main='Reach Deviation With a 60° Rotation',
       xlab='Trial', ylab='Reach Deviation (°)',
       xlim=c(-8,32), ylim=c(-15,60), 
       ax=F, bty='n')
  
  # 2D histogram of the data (reachdeviation_deg vs cutrial_no)
  img_info <- hist2d(x=explREACH, nbins=NA, edges=list(seq(-8,31.5,1), seq(-15,60,2.5)))
  img <- log(img_info$freq2D + 1)
  
  # Plot the image
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        col=colorRampPalette(c("white", "pink1", "violetred3", "violetred4"))(100),
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
  
  avg_reach60 <- aggregate(reachdeviation_deg ~ x, data=combined60, FUN=mean)
  lines(avg_reach60$x, avg_reach60$reachdeviation_deg, col="blue", lwd=2)
}



#50 DEGREE

get50_Data <- function() {
  reach50_path <- 'data/Instructed_summary/reaching50'
  all_50data <- list()
  reach50files <- list.files(reach50_path, pattern = "*.csv", full.names = TRUE)
  
  for (file_path in reach50files) {
    df <- read.csv(file_path)
    all_50data[[length(all_50data) + 1]] <- df
  }
  
  return(all_50data)
}

reach50_data <- get50_Data()

getCombinedAlignedRotated50 <- function(all_data50) {
  data_path <- "data/Instructed_summary/reaching50/"
  
  aligned_g150 <-   combined_g1_aligned50[which(combined_g1_aligned50$cutrial_no %in% 81:88),]
  rotated_g150 <- combined_g1_rotated50[which(combined_g1_rotated50$cutrial_no %in% 89:121),]
  
  aligned_g1_combined50 <- data.frame(
    x = aligned_g150$cutrial_no - 89,
    cutrial_no = aligned_g150$cutrial_no,  # Trial numbers
    reachdeviation_deg = aligned_g150$reachdeviation_deg  # reach deviation values
  )
  
  
  
  rotated_g1_combined50 <- data.frame( x = rotated_g150$cutrial_no - 89,
                                       cutrial_no=rotated_g150$cutrial_no, reachdeviation_deg = 
                                         rotated_g150$reachdeviation_deg)
  
  ###
  aligned_g250 <- combined_g2_aligned50[combined_g2_aligned50$cutrial_no %in% 105:112,]
  rotated_g250 <-  combined_g2_rotated50[combined_g2_rotated50$cutrial_no %in% 113:144,]
  
  aligned_g2_combined50 <- data.frame(
    x = aligned_g250$cutrial_no - 113,
    cutrial_no = aligned_g250$cutrial_no,  
    reachdeviation_deg = aligned_g250$reachdeviation_deg)
  
  
  rotated_g2_combined50 <- data.frame(
    x = rotated_g250$cutrial_no - 113,cutrial_no=rotated_g250$cutrial_no, reachdeviation_deg = 
      rotated_g250$reachdeviation_deg)
  
  g1_50REACH <- data.frame(
    x = c(aligned_g1_combined50$x, rotated_g1_combined50$x),  
    reachdeviation_deg = c(aligned_g1_combined50$reachdeviation_deg, rotated_g1_combined50$reachdeviation_deg)  # Combine reach deviation values
  )
  g2_50REACH <- data.frame(
    x = c(aligned_g2_combined50$x, rotated_g2_combined50$x),  
    reachdeviation_deg = c(aligned_g2_combined50$reachdeviation_deg, rotated_g2_combined50$reachdeviation_deg)  # Combine reach deviation values
  )
  
  combined50REACH <- rbind(g1_50REACH,g2_50REACH)
  
  return(combined50REACH)
}


plot50histREACH <- function() {
  exp2REACH <- combined50REACH
  
  # Rename for plotting
  names(exp2REACH) <- c('x', 'y')
  # Plotting
  plot(NA,
       main='Reach Deviation With a 50° Rotation',
       xlab='Trial', ylab='Reach Deviation (°)',
       xlim=c(-8,32), ylim=c(-15,50), 
       ax=F, bty='n')
  
  # 2D histogram of the data (reachdeviation_deg vs cutrial_no)
  img_info <- hist2d(x=exp2REACH, nbins=NA, edges=list(seq(-8,31.5,1), seq(-15,50,2.5)))
  img <- log(img_info$freq2D + 1)
  
  # Plot the image
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        col=colorRampPalette(c("white", "pink1", "violetred3", "violetred4"))(100),
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
  reach40_path <- 'data/Instructed_summary/reaching40'
  all_40data <- list()
  reach40files <- list.files(reach40_path, pattern = "*.csv", full.names = TRUE)
  
  for (file_path in reach40files) {
    df <- read.csv(file_path)
    all_40data[[length(all_40data) + 1]] <- df
  }
  
  return(all_40data)
}

reach40_data <- get40_Data()
print(length(reach40_data))

getCombinedAlignedRotated40 <- function(all_data40) {
  data_path <- "data/Instructed_summary/reaching40/"
  
  aligned_g140 <-   combined_g1_aligned40[which(combined_g1_aligned40$cutrial_no %in% 81:88),]
  rotated_g140 <- combined_g1_rotated40[which(combined_g1_rotated40$cutrial_no %in% 89:121),]
  
  aligned_g1_combined40 <- data.frame(
    x = aligned_g140$cutrial_no - 89,
    cutrial_no = aligned_g140$cutrial_no,  # Trial numbers
    reachdeviation_deg = aligned_g140$reachdeviation_deg  # reach deviation values
  )
  
  
  
  rotated_g1_combined40 <- data.frame( x = rotated_g140$cutrial_no - 89,
                                       cutrial_no=rotated_g140$cutrial_no, reachdeviation_deg = 
                                         rotated_g140$reachdeviation_deg)
  
  ###
  aligned_g240 <- combined_g2_aligned40[combined_g2_aligned40$cutrial_no %in% 105:112,]
  rotated_g240 <-  combined_g2_rotated40[combined_g2_rotated40$cutrial_no %in% 113:144,]
  
  aligned_g2_combined40 <- data.frame(
    x = aligned_g240$cutrial_no - 113,
    cutrial_no = aligned_g240$cutrial_no,  
    reachdeviation_deg = aligned_g240$reachdeviation_deg)
  
  
  rotated_g2_combined40 <- data.frame(
    x = rotated_g240$cutrial_no - 113,cutrial_no=rotated_g240$cutrial_no, reachdeviation_deg = 
      rotated_g240$reachdeviation_deg)
  
  g1_40REACH <- data.frame(
    x = c(aligned_g1_combined40$x, rotated_g1_combined40$x),  # Combine x values from both phases
    reachdeviation_deg = c(aligned_g1_combined40$reachdeviation_deg, rotated_g1_combined40$reachdeviation_deg)  # Combine reach deviation values
  )
  g2_40REACH <- data.frame(
    x = c(aligned_g2_combined40$x, rotated_g2_combined40$x),  # Combine x values from both phases
    reachdeviation_deg = c(aligned_g2_combined40$reachdeviation_deg, rotated_g2_combined40$reachdeviation_deg)  # Combine reach deviation values
  )
  
  combined40REACH <- rbind(g1_40REACH,g2_40REACH)
  
  return(combined40REACH)
}


plot40histREACH <- function() {
  exp3REACH <- combined40REACH
  
  # Rename for plotting
  names(exp3REACH) <- c('x', 'y')
  # Plotting
  plot(NA,
       main='Reach Deviation With a 40° Rotation',
       xlab='Trial', ylab='Reach Deviation (°)',
       xlim=c(-8,32), ylim=c(-15,40), 
       ax=F, bty='n')
  
  # 2D histogram of the data (reachdeviation_deg vs cutrial_no)
  img_info <- hist2d(x=exp3REACH, nbins=NA, edges=list(seq(-8,31.5,1), seq(-15,40,2.5)))
  img <- log(img_info$freq2D + 1)
  
  # Plot the image
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        col=colorRampPalette(c("white", "pink1", "violetred3", "violetred4"))(100),
        z=img,
        add=TRUE)
  
  axis(side=1, at=c(-8, 0,8, 16, 24, 32), labels=c(-8, 0,8, 16, 24, 32))
  axis(side=2, at=seq(-10,40,10))
  lines(x=c(-8, 0, 0, 32), 
        y=c(-0.5, -0.5, 39.5, 39.5), 
        col='navy', lty=3, lwd=2)
  
}


###30 Degree

get30_Data <- function() {
  reach30_path <- 'data/Instructed_summary/reaching30'
  all_30data <- list()
  reach30files <- list.files(reach30_path, pattern = "*.csv", full.names = TRUE)
  
  for (file_path in reach30files) {
    df <- read.csv(file_path)
    all_30data[[length(all_30data) + 1]] <- df
  }
  
  return(all_30data)
}

reach30_data <- get30_Data()
print(length(reach30_data))

getCombinedAlignedRotated30 <- function(all_data30) {
  data_path <- "data/Instructed_summary/reaching30/"
  
  ###
  aligned_g230 <- combined_g2_aligned30[combined_g2_aligned30$cutrial_no %in% 105:112,]
  rotated_g230 <-  combined_g2_rotated30[combined_g2_rotated30$cutrial_no %in% 113:144,]
  
  aligned_g2_combined30 <- data.frame(
    x = aligned_g230$cutrial_no - 113,
    cutrial_no = aligned_g230$cutrial_no,  
    reachdeviation_deg = aligned_g230$reachdeviation_deg)
  
  
  rotated_g2_combined30 <- data.frame(
    x = rotated_g230$cutrial_no - 113,cutrial_no=rotated_g230$cutrial_no, reachdeviation_deg = 
      rotated_g230$reachdeviation_deg)
  
  
  g2_30REACH <- data.frame(
    x = c(aligned_g2_combined30$x, rotated_g2_combined30$x),  # Combine x values from both phases
    reachdeviation_deg = c(aligned_g2_combined30$reachdeviation_deg, rotated_g2_combined30$reachdeviation_deg)  # Combine reach deviation values
  )
  
  combined30REACH <- rbind(g2_30REACH)
  
  return(combined30REACH)
}


plot30histREACH <- function() {
  exp4REACH <- combined30REACH
  
  # Rename for plotting
  names(exp4REACH) <- c('x', 'y')
  # Plotting
  plot(NA,
       main='Reach Deviation With a 30° Rotation',
       xlab='Trial', ylab='Reach Deviation (°)',
       xlim=c(-8,32), ylim=c(-15,30), 
       ax=F, bty='n')
  
  # 2D histogram of the data (reachdeviation_deg vs cutrial_no)
  img_info <- hist2d(x=exp4REACH, nbins=NA, edges=list(seq(-8,31.5,1.2), seq(-15,30,2.5)))
  img <- log(img_info$freq2D + 1)
  
  # Plot the image
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        col=colorRampPalette(c("white", "pink1", "violetred3", "violetred4"))(100),
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
  reach20_path <- 'data/Instructed_summary/reaching20'
  all_20data <- list()
  reach20files <- list.files(reach20_path, pattern = "*.csv", full.names = TRUE)
  
  for (file_path in reach20files) {
    df <- read.csv(file_path)
    all_20data[[length(all_20data) + 1]] <- df
  }
  
  return(all_20data)
}

reach20_data <- get20_Data()
print(length(reach20_data))


getCombinedAlignedRotated20 <- function(all_data20) {
  data_path <- "data/Instructed_summary/reaching20/"
  
  ###
  aligned_g220 <- combined_g2_aligned20[combined_g2_aligned20$cutrial_no %in% 105:112,]
  rotated_g220 <-  combined_g2_rotated20[combined_g2_rotated20$cutrial_no %in% 113:144,]
  
  aligned_g2_combined20 <- data.frame(
    x = aligned_g220$cutrial_no - 113,
    cutrial_no = aligned_g220$cutrial_no,  
    reachdeviation_deg = aligned_g220$reachdeviation_deg)
  
  
  rotated_g2_combined20 <- data.frame(
    x = rotated_g220$cutrial_no - 113,cutrial_no=rotated_g220$cutrial_no, reachdeviation_deg = 
      rotated_g220$reachdeviation_deg)
  
  
  g2_20REACH <- data.frame(
    x = c(aligned_g2_combined20$x, rotated_g2_combined20$x),  # Combine x values from both phases
    reachdeviation_deg = c(aligned_g2_combined20$reachdeviation_deg, rotated_g2_combined20$reachdeviation_deg)  # Combine reach deviation values
  )
  
  combined20REACH <- rbind(g2_20REACH)
  
  return(combined20REACH)
}


plot20histREACH <- function() {
  exp5REACH <- combined20REACH
  
  # Rename for plotting
  names( exp5REACH) <- c('x', 'y')
  # Plotting
  plot(NA,
       main='Reach Deviation With a 20° Rotation',
       xlab='Trial', ylab='Reach Deviation (°)',
       xlim=c(-8,32), ylim=c(-15,20), 
       ax=F, bty='n')
  
  # 2D histogram of the data (reachdeviation_deg vs cutrial_no)
  img_info <- hist2d(x= exp5REACH, nbins=NA, edges=list(seq(-8,31.5,1.2), seq(-15,20,2.5)))
  img <- log(img_info$freq2D + 1)
  
  # Plot the image
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        col=colorRampPalette(c("white", "pink1", "violetred3", "violetred4"))(100),
        z=img,
        add=TRUE)
  
  axis(side=1, at=c(-8, 0,8, 16, 24, 32), labels=c(-8, 0,8, 16, 24, 32))
  axis(side=2, at=seq(-10,20,10))
  lines(x=c(-8, 0, 0, 32), 
        y=c(-0.5, -0.5, 19.5, 19.5), 
        col='navy', lty=3, lwd=2)
  
}