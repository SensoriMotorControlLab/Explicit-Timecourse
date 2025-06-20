plotAll20 <- function() {
  
  aligned20_data <- getAligned20()
  rotated20_data <- getRotated20()
  after20_data <- getAfter20()
  
  
  group2_c20 <- do.call(rbind, aligned20_data$group2)
  group2_combined20 <- do.call(rbind, rotated20_data$group2)
  g2_combined20 <- do.call(rbind, after20_data$group2)
  
  group2avg20 <- aggregate(group2_c20$reachdeviation_deg, by = list(group2_c20$cutrial_no), FUN = mean)
  group2_avg20 <- aggregate(group2_combined20$reachdeviation_deg, by = list(group2_combined20$cutrial_no), FUN = mean)
  g2_avg20 <- aggregate(g2_combined20$reachdeviation_deg, by = list(g2_combined20$cutrial_no), FUN = mean)
  
  
  all_data20 <- rbind(
    group2avg20, 
    group2_avg20, 
    g2_avg20)
  
  all_data20 <- all_data20[order(all_data20$Group.1), ] 
  
  #Get CI
  CI <- function(df) {
    aggregate(reachdeviation_deg ~ cutrial_no, 
              data = df, 
              FUN = function(x) Reach::getConfidenceInterval(x))
  }
  
  CI(group1_c)
  CI(group2_c)
  CI(g1_combined)
  CI(g2_combined)
  CI(group1_combined)
  CI(group2_combined)
  
  ci_list20 <- list(
    CI(group2_c20),
    CI(g2_combined20),
    CI(group2_combined20)
  )
  
  ci_combined20 <- do.call(rbind, ci_list20)
  ci_combined20 <- ci_combined20[order(ci_combined20$cutrial_no), ]
  
  lo20 <- ci_combined20$reachdeviation_deg[, 1]
  hi20 <- ci_combined20$reachdeviation_deg[ ,2]
  cutrial_no20 <- ci_combined20$cutrial_no
  
  
  y_lower_limit <- -20
  y_upper_limit <- 20
  
  
  valid_indices20 <- lo20 >= y_lower_limit & lo20 <= y_upper_limit & hi20 >= y_lower_limit & hi20 <= y_upper_limit
  lo_filtered20 <- lo20[valid_indices20]
  hi_filtered20 <- hi20[valid_indices20]
  cutrial_no_filtered20 <- cutrial_no20[valid_indices20]
  
  
  if (length(lo_filtered20) == length(hi_filtered20) && length(hi_filtered20) == length(cutrial_no_filtered20)) {
    plot(-1000, 1000, type = "n",
         main = "Reach Deviation Across All Trials", 
         xlab = "Trial", ylab = "Reach Deviation (degrees)",
         xlim = c(0, 256), ylim = c(-20, 20))
    
    
    #lines(all_data$Group.1, all_data$x, col = "hotpink", lwd = 2, pch = 16)  
    smoothreach20 <- smooth.spline(all_data20$Group.1, all_data20$x, spar = 0.02)
    lines(smoothreach20, col='tomato', lwd=2)
    abline(h = 0, col = "black", lty = 3)
    
    lo_smooth_filtered20 <- smooth.spline(cutrial_no_filtered20, lo_filtered20, spar = 0.008)
    hi_smooth_filtered20 <- smooth.spline(cutrial_no_filtered20, hi_filtered20, spar = 0.008)
    
    # Draw the polygon for the filtered CIs
    polygon(x = c(lo_smooth_filtered20$x, rev(hi_smooth_filtered20$x)),
            y = c(lo_smooth_filtered20$y, rev(hi_smooth_filtered20$y)),
            col = rgb(1, 0.388, 0.278, alpha = 0.2),
            border = NA)
  } else {
    stop("Lengths of lo_filtered, hi_filtered, and cutrial_no_filtered do not match!")
  }
  
  
  
  
  
  
  plotAllAim20 <- function () {
    
    aligned20_data <- getAligned20()
    rotated20_data <- getRotated20()
    after20_data <- getAfter20()
    
    
    group2_c20 <- do.call(rbind, aligned20_data$group2)
    group2_combined20 <- do.call(rbind, rotated20_data$group2)
    g2_combined20 <- do.call(rbind, after20_data$group2)
    
    group2avg20 <- aggregate(group2_c20$aimdeviation_deg, by = list(group2_c20$cutrial_no), FUN = mean)
    group2_avg20 <- aggregate(group2_combined20$aimdeviation_deg, by = list(group2_combined20$cutrial_no), FUN = mean)
    g2_avg20 <- aggregate(g2_combined20$aimdeviation_deg, by = list(g2_combined20$cutrial_no), FUN = mean)
    
    
    all_data20 <- rbind(
      group2avg20, 
      group2_avg20, 
      g2_avg20)
    
    all_data20 <- all_data20[order(all_data20$Group.1), ] 
    
    #Get CI
    CI <- function(df) {
      aggregate(aimdeviation_deg ~ cutrial_no, 
                data = df, 
                FUN = function(x) Reach::getConfidenceInterval(x))
    }
    
    CI(group1_c)
    CI(group2_c)
    CI(g1_combined)
    # CI(g2_combined)
    CI(group1_combined)
    CI(group2_combined)
    
    ci_list20 <- list(
      CI(group2_c20),
      CI(group2_combined20))
    
    
    ci_combined20 <- do.call(rbind, ci_list20)
    ci_combined20 <- ci_combined20[order(ci_combined20$cutrial_no), ]
    
    
    lo <- ci_combined20$aimdeviation_deg [, 1]
    hi <- ci_combined20$aimdeviation_deg[ ,2]
    cutrial_no <- ci_combined20$cutrial_no
    
    
    y_lower_limit <- -20
    y_upper_limit <- 20
    
    
    valid_indices20 <- lo >= y_lower_limit & lo <= y_upper_limit & hi >= y_lower_limit & hi <= y_upper_limit
    lo_filtered20 <- lo[valid_indices20]
    hi_filtered20 <- hi[valid_indices20]
    cutrial_no_filtered20 <- cutrial_no[valid_indices20]
    
    
    if (length(lo_filtered20) == length(hi_filtered20) && length(hi_filtered20) == length(cutrial_no_filtered20)) {
      plot(-1000, 1000, type = "n",
           main = "Aiming Strategies with a 20° Rotation", 
           xlab = "Trial", ylab = "Mean Aim Deviation (°)",
           xlim = c(0, 256), ylim = c(-20, 20))
      
      #lines(all_data$Group.1, all_data$x, col = "hotpink", lwd = 2, pch = 16)  
      all_data_clean20 <- all_data20[!is.na(all_data20$Group.1) & !is.na(all_data20$x) & 
                                       !is.infinite(all_data20$Group.1) & !is.infinite(all_data20$x), ]
      
      
      smoothall20 <- smooth.spline(all_data_clean20$Group.1, all_data_clean20$x, spar = 0.0001)
      lines(smoothall20, col='magenta4', lwd=2)
      abline(h = 0, col = "black", lty = 3)
      
      lo_smooth_filtered20 <- smooth.spline(cutrial_no_filtered20, lo_filtered20, spar = 0.008)
      hi_smooth_filtered20 <- smooth.spline(cutrial_no_filtered20, hi_filtered20, spar = 0.008)
      
      # Draw the polygon for the filtered CIs
      polygon(x = c(lo_smooth_filtered20$x, rev(hi_smooth_filtered20$x)),
              y = c(lo_smooth_filtered20$y, rev(hi_smooth_filtered20$y)),
              col =rgb(0.6, 0, 0.6, alpha = 0.2) ,
              border = NA)
    } else {
      stop("Lengths of lo_filtered, hi_filtered, and cutrial_no_filtered do not match!")