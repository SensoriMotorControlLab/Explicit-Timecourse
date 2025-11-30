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

df <- function () {
  total_learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors =FALSE)
  learner_id <- suppressMessages(getLearners())
  
  
  
   last_8_aligned <- total_learners_data[
  (total_learners_data$cutrial_no %in% 85:88 & total_learners_data$group == 'Group 1') |
    (total_learners_data$cutrial_no %in% 109:112 & total_learners_data$group == 'Group 2'),]

  last_8_aligned <- last_8_aligned %>%
  group_by(group, participant_id) %>%
  arrange(cutrial_no) %>%
  mutate(time = seq(-n(), -1)) %>%
  ungroup()


  last_8_aligned_learners <- last_8_aligned %>%
  semi_join(learner_id , by = c("rotation", "participant_id"))

  dfAligned <- data.frame(
  x = last_8_aligned_learners$time,
  y = last_8_aligned_learners$aimdeviation_deg,
  rotation_group = last_8_aligned_learners$rotation
  )


  first_32_rotated <- total_learners_data[
  (total_learners_data$cutrial_no %in% 89:120 & total_learners_data$group == 'Group 1') |
    (total_learners_data$cutrial_no %in% 105:136 & total_learners_data$group == 'Group 2'),]


  first_32_rotated <- first_32_rotated %>%
  group_by(group, participant_id) %>%
  arrange(cutrial_no) %>%
  mutate(time = seq(0, n()-1)) %>%
  ungroup()


  first_32_rotated_learners <- first_32_rotated %>%
  semi_join(  learner_id , by = c("rotation", "participant_id"))

  dfRotated <- data.frame(
  x = first_32_rotated_learners$time,
  y= first_32_rotated_learners$aimdeviation_deg,
  rotation_group = first_32_rotated_learners$rotation
  )

  all_hist_data <- rbind(dfAligned, dfRotated)
  return(all_hist_data)
}

#60 aiming histogram
plot60aim <- function () {
  all_hist_data <- df ()
  aim_60_hist <- all_hist_data %>%
    filter(rotation_group == '60')
  
  plot(NA,
       main='60° Rotation',
       xlab='Trial', ylab='Aim Deviation (°)',
       xlim=c(-4,32), ylim=c(-15,65),  
       ax=F, bty='n',
       cex.lab = 1,
       cex.axis = 1.5,  
       cex.main = 1.5)
  
  img_info <- hist2d(
    x = aim_60_hist,
    nbins = NA,
    edges = list(seq(-4,31.5,1), seq(-15,50,2.5))
  )
  img <- log(img_info$freq2D + 1)
  
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        col=colorRampPalette(c("white", "#B9D3EE", "#638ac1", "#271716"))(100),
        z=img,
        add=TRUE)
  
  # X-axis numbers same as 50
  axis(side=1, at=c(-4, 0, 8, 16, 24, 32), labels=c(-4, 0, 8, 16, 24, 32), cex.axis = 1.5)
  
  # Y-axis numbers same as 50
  axis(side=2, at=seq(-10, 60, 10), cex.axis = 1.5)
  
  # Reference box line same style, adjusted to 60° height
  lines(x=c(-4, 0, 0, 32), 
        y=c(-0.5, -0.5, 59.5, 59.5), 
        col='navy', lty=3, lwd=2)
}

#avg_aim60 <- aggregate(y ~ x, data=aim_60_hist, FUN=mean)
#lines(avg_aim60$x, avg_aim60$y, col="grey30", lwd=2)
#text(x = -8, y = 63, adj = c(0,2), col = "black", cex = 1)


#50 aiming histogram
plot50aim <- function () {
  all_hist_data <- df ()
  aim_50_hist <- all_hist_data %>%
    filter(rotation_group == '50')
  
  # Plotting
  plot(NA,
       main='50° Rotation',
       xlab='Trial', ylab='Aim Deviation (°)',
       xlim=c(-4,32), ylim=c(-15,65), 
       ax=F, bty='n',
       cex.lab = 1,
       cex.axis = 1.5,  
       cex.main = 1.5)
  
  img_info <- hist2d(x=aim_50_hist, nbins=NA, edges=list(seq(-4,31.5,1), seq(-15,65,2.5)))
  img <- log(img_info$freq2D + 1)
  
  
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        col = colorRampPalette(c("white", "#B9D3EE", "#638ac1", "#271716"))(100),
        z=img,
        add=TRUE)
  
  axis(side=1, at=c(-4, 0,8, 16, 24, 32), labels=c(-4, 0,8, 16, 24, 32), cex.axis = 1.5)
  axis(side = 2, at = seq(-10, 65, 10), cex.axis = 1.5)
  lines(x=c(-4, 0, 0, 32), 
        y=c(-0.5, -0.5, 49.5, 49.5), 
        col='navy', lty=3, lwd=2)
  
  avg_aim50 <- aggregate(y ~ x, data=aim_50_hist, FUN=mean)
  #lines(avg_aim60$x, avg_aim50$y, col="hotpink", lwd=2)
  #text(x = -8, y = 53, adj = c(0,2), col = "black", cex = 1)
}

#40 aiming histogram

plot40aim <- function () {
  all_hist_data <- df ()
  aim_40_hist <- all_hist_data %>%
    filter(rotation_group == '40')
  
  # Plotting
  plot(NA,
       main=' 40° Rotation ',
       xlab='Trial', ylab='Aim Deviation (°)',
       xlim=c(-4,32), ylim=c(-15,65), 
       ax=F, bty='n',
       cex.lab = 1,
       cex.axis = 1.5,  
       cex.main = 1.5)
  
  img_info <- hist2d(x=aim_40_hist, nbins=NA, edges=list(seq(-4,31.5,1), seq(-15,40,2.5)))
  img <- log(img_info$freq2D + 1)
  
  
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        col = colorRampPalette(c("white", "#B9D3EE", "#638ac1", "#271716"))(100),
        z=img,
        add=TRUE)
  
  axis(side=1, at=c(-4, 0,8, 16, 24, 32), labels=c(-4, 0,8, 16, 24, 32),cex.axis = 1.5)
  axis(side=2, at=seq(-10,65,10),cex.axis = 1.5)
  lines(x=c(-4, 0, 0, 32), 
        y=c(-0.5, -0.5, 39.5, 39.5), 
        col='navy', lty=3, lwd=2)
  
 # avg_aim40 <- aggregate(y ~ x, data=aim_40_hist, FUN=mean)
  #lines(avg_aim40$x, avg_aim40$y, col="hotpink", lwd=2)
  #text(x = -8, y = 43, adj = c(0,2), col = "black", cex = 1)
}


plot30aim <- function () {
  all_hist_data <- df ()
  aim_30_hist <- all_hist_data %>%
    filter(rotation_group == '30')
  
  # Plotting
  plot(NA,
       main='30° Rotation',
       xlab='Trial', ylab='Aim Deviation (°)',
       xlim=c(-8,32), ylim=c(-15,65), 
       ax=F, bty='n',
       cex.lab = 1,
       cex.axis = 1.5,  
       cex.main = 1.5)
  
  img_info <- hist2d(x=aim_30_hist, nbins=NA, edges=list(seq(-8,31.5,1), seq(-15,30,2.5)))
  img <- log(img_info$freq2D + 1)
  
  
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        col = colorRampPalette(c("white", "#B9D3EE", "#638ac1", "#271716"))(100),
        z=img,
        add=TRUE)
  
  axis(side=1, at=c(-8, 0,8, 16, 24, 32), labels=c(-8, 0,8, 16, 24, 32))
  axis(side=2, at=seq(-10,60,10))
  lines(x=c(-8, 0, 0, 32), 
        y=c(-0.5, -0.5, 29.5, 29.5), 
        col='navy', lty=3, lwd=2)
  
 # avg_aim40 <- aggregate(y ~ x, data=aim_40_hist, FUN=mean)
  #lines(avg_aim40$x, avg_aim40$y, col="hotpink", lwd=2)
  #text(x = -8, y = 43, adj = c(0,2), col = "black", cex = 1)
}


plot20aim <- function () {
  all_hist_data <- df ()
  aim_20_hist <- all_hist_data %>%
    filter(rotation_group == '20')
  
  # Plotting
  plot(NA,
       main=' 20° Rotation',
       xlab='Trial', ylab='Aim Deviation (°)',
       xlim=c(-8,32), ylim=c(-15,65), 
       ax=F, bty='n',
       cex.lab = 1,
       cex.axis = 1.5,  
       cex.main = 1.5)
  
  img_info <- hist2d(x=aim_20_hist, nbins=NA, edges=list(seq(-8,31.5,1), seq(-15,20,2.5)))
  img <- log(img_info$freq2D + 1)
  
  

  
        image(x=img_info$x.edges,
              y=img_info$y.edges,
              col = colorRampPalette(c("white", "#B9D3EE", "#638ac1", "#271716"))(100),
              z=img,
              add=TRUE)
        
        axis(side=1, at=c(-8, 0,8, 16, 24, 32), labels=c(-8, 0,8, 16, 24, 32))
        axis(side=2, at=seq(-10,60,10))
        lines(x=c(-8, 0, 0, 32), 
              y=c(-0.5, -0.5, 19.5, 19.5), 
              col='navy', lty=3, lwd=2)
  
#  avg_aim40 <- aggregate(y ~ x, data=aim_40_hist, FUN=mean)
  #lines(avg_aim40$x, avg_aim40$y, col="hotpink", lwd=2)
  #text(x = -8, y = 43, adj = c(0,2), col = "black", cex = 1)
}





####Simulations

simulate_step_function_variable_jump <- function(n_participants = 50,
                                                 jump_range = c(5, 20),
                                                 pre_mean = 0, post_mean = 60,
                                                 pre_sd = 5, post_sd = 9) {
  df_list <- list()
  
  for (i in 1:n_participants) {
    jump_trial <- sample(jump_range[1]:jump_range[2], 1)
    pid <- paste0("sim_", i)
    
    aligned <- data.frame(participant_id = pid,
                          time = -8:-1,
                          aimdeviation_deg = rnorm(8, pre_mean, pre_sd))
    
    rotated <- data.frame(participant_id = pid,
                          time = 0:31)
    
    rotated$aimdeviation_deg <- ifelse(rotated$time < jump_trial,
                                       rnorm(32, pre_mean, pre_sd),
                                       rnorm(32, post_mean, post_sd))
    
    df_list[[i]] <- rbind(aligned, rotated)
  }
  
  df <- do.call(rbind, df_list)
  df$rotation_group <- "60"
  return(df)
}



simulate_exponential_learning <- function(n_participants = 50,
                                          start_mean = 0,
                                          end_mean = 60,
                                          sd = 10,
                                          n_pre = 8,
                                          n_post = 32,
                                          rate = 0.15) {
  df_list <- list()
  
  for (i in 1:n_participants) {
    pid <- paste0("sim_", i)
    
    aligned <- data.frame(
      participant_id = pid,
      time = -n_pre:-1,
      aimdeviation_deg = rnorm(n_pre, start_mean, sd)
    )
    
    time_post <- 0:(n_post - 1)
    exp_vals <- end_mean * (1 - exp(-rate * time_post))
    exp_means <- start_mean + exp_vals
    
    rotated <- data.frame(
      participant_id = pid,
      time = time_post,
      aimdeviation_deg = rnorm(n_post, exp_means, sd)
    )
    
    df_list[[i]] <- rbind(aligned, rotated)
  }
  
  df <- do.call(rbind, df_list)
  df$rotation_group <- "60"
  return(df)
}




plot_step_histogram <- function(sim_data) {
  plot(NA,
       xlab = 'Trial', ylab = 'Aim Deviation (°)',
       xlim = c(-8, 32), ylim = c(-15, 65),
       ax = FALSE, bty = 'n',
       cex.lab = 1.5,
       cex.axis = 1.2)
  
  img_info <- hist2d(x = sim_data[, c("time", "aimdeviation_deg")],
                     edges = list(seq(-8, 31.5, 1), seq(-15, 65, 2.5)))
  
  img <- log(img_info$freq2D + 1)
  
  image(x = img_info$x.edges,
        y = img_info$y.edges,
        col = colorRampPalette(c("white", "#FFB281", "#F5546E", "#7D1D67"))(100),
        z = img,
        add = TRUE)
  
  axis(side = 1, at = c(-8, 0, 8, 16, 24, 32))
  axis(side = 2, at = seq(-10, 60, 10))
  lines(x = c(-8, 0, 0, 32), y = c(-0.5, -0.5, 64.5, 64.5),
        col = 'navy', lty = 3, lwd = 2)
}


# Simulate step learners
step_data_var_jump <- simulate_step_function_variable_jump(n_participants = 80)

# Simulate exponential learners
exp_data <- simulate_exponential_learning(n_participants = 80)




#take each participants mean aim dev for the last 16 trials, 
#find the mean of all that, and the sd and plot those group-level parameters to the histogram to see


