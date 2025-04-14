plotAll30 <- function() {
  
  aligned30_data <- getAligned30()
  rotated30_data <- getRotated30()
  after30_data <- getAfter30()
  
  
  group2_c30 <- do.call(rbind, aligned30_data$group2)
  group2_combined30 <- do.call(rbind, rotated30_data$group2)
  g2_combined30 <- do.call(rbind, after30_data$group2)
  
  group2avg30 <- aggregate(group2_c30$reachdeviation_deg, by = list(group2_c30$cutrial_no), FUN = mean)
  group2_avg30 <- aggregate(group2_combined30$reachdeviation_deg, by = list(group2_combined30$cutrial_no), FUN = mean)
  g2_avg30 <- aggregate(g2_combined30$reachdeviation_deg, by = list(g2_combined30$cutrial_no), FUN = mean)
  
  
  all_data30 <- rbind(
   group2avg30, 
    group2_avg30, 
    g2_avg30)
  
  all_data30 <- all_data30[order(all_data30$Group.1), ] 
  
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
  
  ci_list30 <- list(
    CI(group2_c30),
    CI(g2_combined30),
    CI(group2_combined30)
  )
  
  ci_combined30 <- do.call(rbind, ci_list30)
  ci_combined30 <- ci_combined30[order(ci_combined30$cutrial_no), ]
  
  lo30 <- ci_combined30$reachdeviation_deg[, 1]
  hi30 <- ci_combined30$reachdeviation_deg[ ,2]
  cutrial_no30 <- ci_combined30$cutrial_no
  
  
  y_lower_limit <- -20
  y_upper_limit <- 30
  
  
  valid_indices30 <- lo30 >= y_lower_limit & lo30 <= y_upper_limit & hi30 >= y_lower_limit & hi30 <= y_upper_limit
  lo_filtered30 <- lo30[valid_indices30]
  hi_filtered30 <- hi30[valid_indices30]
  cutrial_no_filtered30 <- cutrial_no30[valid_indices30]
  
  
  if (length(lo_filtered30) == length(hi_filtered30) && length(hi_filtered30) == length(cutrial_no_filtered30)) {
    plot(-1000, 1000, type = "n",
         main = "Reach Deviation Across All Trials", 
         xlab = "Trial", ylab = "Reach Deviation (degrees)",
         xlim = c(0, 256), ylim = c(-20, 30))
    
    
    #lines(all_data$Group.1, all_data$x, col = "hotpink", lwd = 2, pch = 16)  
    smoothreach30 <- smooth.spline(all_data30$Group.1, all_data30$x, spar = 0.02)
    lines(smoothreach30, col='tomato', lwd=2)
    abline(h = 0, col = "black", lty = 3)
    
    lo_smooth_filtered30 <- smooth.spline(cutrial_no_filtered30, lo_filtered30, spar = 0.008)
    hi_smooth_filtered30 <- smooth.spline(cutrial_no_filtered30, hi_filtered30, spar = 0.008)
    
    # Draw the polygon for the filtered CIs
    polygon(x = c(lo_smooth_filtered30$x, rev(hi_smooth_filtered30$x)),
            y = c(lo_smooth_filtered30$y, rev(hi_smooth_filtered30$y)),
            col = rgb(1, 0.388, 0.278, alpha = 0.2),
            border = NA)
  } else {
    stop("Lengths of lo_filtered, hi_filtered, and cutrial_no_filtered do not match!")
  }
  





plotAllAim30 <- function () {
  
  aligned30_data <- getAligned30()
  rotated30_data <- getRotated30()
  after30_data <- getAfter30()
  
  
  group2_c30 <- do.call(rbind, aligned30_data$group2)
  group2_combined30 <- do.call(rbind, rotated30_data$group2)
  g2_combined30 <- do.call(rbind, after30_data$group2)
  
  group2avg30 <- aggregate(group2_c30$aimdeviation_deg, by = list(group2_c30$cutrial_no), FUN = mean)
  group2_avg30 <- aggregate(group2_combined30$aimdeviation_deg, by = list(group2_combined30$cutrial_no), FUN = mean)
  g2_avg30 <- aggregate(g2_combined30$aimdeviation_deg, by = list(g2_combined30$cutrial_no), FUN = mean)
  
  
  all_data30 <- rbind(
    group2avg30, 
    group2_avg30, 
   g2_avg30)
  
  all_data30 <- all_data30[order(all_data30$Group.1), ] 
  
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
  
  ci_list30 <- list(
    CI(group2_c30),
    CI(group2_combined30))
  
  
  ci_combined30 <- do.call(rbind, ci_list30)
  ci_combined30 <- ci_combined30[order(ci_combined30$cutrial_no), ]
  
  
  lo <- ci_combined30$aimdeviation_deg [, 1]
  hi <- ci_combined30$aimdeviation_deg[ ,2]
  cutrial_no <- ci_combined30$cutrial_no
  
  
  y_lower_limit <- -20
  y_upper_limit <- 30
  
  
  valid_indices30 <- lo >= y_lower_limit & lo <= y_upper_limit & hi >= y_lower_limit & hi <= y_upper_limit
  lo_filtered30 <- lo[valid_indices30]
  hi_filtered30 <- hi[valid_indices30]
  cutrial_no_filtered30 <- cutrial_no[valid_indices30]
  
  
  if (length(lo_filtered30) == length(hi_filtered30) && length(hi_filtered30) == length(cutrial_no_filtered30)) {
    plot(-1000, 1000, type = "n",
         main = "Aiming Strategies with a 30° Rotation", 
         xlab = "Trial", ylab = "Mean Aim Deviation (°)",
         xlim = c(0, 256), ylim = c(-20, 30))
    
    #lines(all_data$Group.1, all_data$x, col = "hotpink", lwd = 2, pch = 16)  
    all_data_clean30 <- all_data30[!is.na(all_data30$Group.1) & !is.na(all_data30$x) & 
                                     !is.infinite(all_data30$Group.1) & !is.infinite(all_data30$x), ]
    
    
    smoothall30 <- smooth.spline(all_data_clean30$Group.1, all_data_clean30$x, spar = 0.0001)
    lines(smoothall30, col='magenta4', lwd=2)
    abline(h = 0, col = "black", lty = 3)
    
    lo_smooth_filtered30 <- smooth.spline(cutrial_no_filtered30, lo_filtered30, spar = 0.008)
    hi_smooth_filtered30 <- smooth.spline(cutrial_no_filtered30, hi_filtered30, spar = 0.008)
    
    # Draw the polygon for the filtered CIs
    polygon(x = c(lo_smooth_filtered30$x, rev(hi_smooth_filtered30$x)),
            y = c(lo_smooth_filtered30$y, rev(hi_smooth_filtered30$y)),
            col =rgb(0.6, 0, 0.6, alpha = 0.2) ,
            border = NA)
  } else {
    stop("Lengths of lo_filtered, hi_filtered, and cutrial_no_filtered do not match!")
  }