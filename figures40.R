plotAligned40 <- function() {
  aligned40_data <- getAligned40()
  
  group1_c <- do.call(rbind, aligned_data$group1)
  group2_c <- do.call(rbind, aligned_data$group2)
  
  group1avg <- aggregate(group1_c$reachdeviation_deg, by = list(group1_c$cutrial_no), FUN = mean)
  group2avg <- aggregate(group2_c$reachdeviation_deg, by = list(group2_c$cutrial_no), FUN = mean)
  c_data <- rbind(group1avg, group2avg)
  
  plot(-1000,1000,
       type = "n", col = "black",
       main = "Average Reach Deviation during Aligned Trials", xlab = "Trial Number", ylab = "Reach Deviation",
       xlim=c(0,104),  ylim = c(-20,80))
  lines(c_data$x, col="cadetblue")
  abline(h=0, col="black", lty=3)
  
}


plotRotated40 <- function () {
  
  rotated40_data <- getRotated40()
  
  group1_combined <- do.call(rbind, group1_rotated)
  group2_combined <- do.call(rbind, group2_rotated)
  
  group1_avg <- aggregate(group1_combined$reachdeviation_deg, by = list(group1_combined$cutrial_no), FUN = mean)
  group2_avg <- aggregate(group2_combined$reachdeviation_deg, by = list(group2_combined$cutrial_no), FUN = mean)
  combined_data <- merge(group1_avg, group2_avg, by = "Group.1", all = TRUE)
  
  # Calculate the average of the deviations, ignoring NAs
  combined_data$avg_deviation <- rowMeans(combined_data[, c("x.x", "x.y")], na.rm = TRUE)
  
  # Plot the combined data
  plot(combined_data$Group.1, combined_data$avg_deviation, type="l", col="purple", 
       xlim=c(89, 232), ylim=c(-20, 80), 
       xlab="Trial Number", ylab="Reach Deviation (degrees)", 
       main="Reach Deviation During Rotation Trials")
  
  abline(h=0,col="black",lty=3)
  
}


plotAfter40 <- function () {
  
  after40_data <- getAfter40()
  
  g1_combined <- do.call(rbind, group1_after)
  g2_combined <- do.call(rbind, group2_after)
  
  g1_avg <- aggregate(g1_combined$reachdeviation_deg, by = list(g1_combined$cutrial_no), FUN = mean)
  g2_avg <- aggregate(g2_combined$reachdeviation_deg, by = list(g2_combined$cutrial_no), FUN = mean)
  
  combined_d <- rbind(g1_avg, g2_avg)
  combined_d <- combined_d[order(combined_d$Group.1), ] 
  
  
  plot(-1000, 1000, type = "n",
       main = "Reach Deviation After Rotation",xlab = "Trial Number", ylab = "Reach Deviation", 
       xlim = c(209, 256), ylim = c(-5, 80))
  
  lines(combined_d$Group.1, combined_d$x, col = "deeppink", lwd = 2, pch = 16)  
  
  abline(h=0, col="black", lty=3)
}


plotAll40 <- function() {
  
  aligned40_data <- getAligned40()
  rotated40_data <- getRotated40()
  after40_data <- getAfter40()
  
  
  group1_c40 <- do.call(rbind, aligned40_data$group1)
  group2_c40 <- do.call(rbind, aligned40_data$group2)
  group1_combined40 <- do.call(rbind, rotated40_data$group1)
  group2_combined40 <- do.call(rbind, rotated40_data$group2)
  g1_combined40 <- do.call(rbind, after40_data$group1)
  g2_combined40 <- do.call(rbind, after40_data$group2)
  
  group1avg40 <- aggregate(group1_c40$reachdeviation_deg, by = list(group1_c40$cutrial_no), FUN = mean)
  group2avg40 <- aggregate(group2_c40$reachdeviation_deg, by = list(group2_c40$cutrial_no), FUN = mean)
  group1_avg40 <- aggregate(group1_combined40$reachdeviation_deg, by = list(group1_combined40$cutrial_no), FUN = mean)
  group2_avg40 <- aggregate(group2_combined40$reachdeviation_deg, by = list(group2_combined40$cutrial_no), FUN = mean)
  g1_avg40 <- aggregate(g1_combined40$reachdeviation_deg, by = list(g1_combined40$cutrial_no), FUN = mean)
  g2_avg40 <- aggregate(g2_combined40$reachdeviation_deg, by = list(g2_combined40$cutrial_no), FUN = mean)
  
  
  all_data40 <- rbind(
    group1avg40, group2avg40, 
    group1_avg40, group2_avg40, 
    g1_avg40 ,  g2_avg40)
  
  all_data40 <- all_data40[order(all_data40$Group.1), ] 
  
  #Get CI

  
  CI(group1_c)
  CI(group2_c)
  CI(g1_combined)
  CI(g2_combined)
  CI(group1_combined)
  CI(group2_combined)
  
  
  CI <- function(df, n_iter = 1000) {
    # Check if the data frame has only one row (one participant)
    if (nrow(df) == 1) {
      # For a group with only one participant, use bootstrapping to estimate a CI
      boot_samples <- replicate(n_iter, sample(df$reachdeviation_deg, replace = TRUE))
      boot_means <- apply(boot_samples, 2, mean)
      ci_lower <- quantile(boot_means, 0.025)
      ci_upper <- quantile(boot_means, 0.975)
      
      # Return the CI for this single participant
      return(data.frame(cutrial_no = df$cutrial_no, 
                        lower = ci_lower, 
                        upper = ci_upper, 
                        mean = mean(df$reachdeviation_deg)))
    } else {
      # Standard CI calculation for groups with more than one participant
      return(aggregate(reachdeviation_deg ~ cutrial_no, 
                       data = df, 
                       FUN = function(x) Reach::getConfidenceInterval(x)))
    }
  }
  
  # Call the CI function for each group and store the results
  ci_list40 <- list(
    CI(group1_c40),
    CI(g1_combined40),
    CI(group1_combined40)
  )
  
  ci_combined40 <- do.call(rbind, ci_list40)
  ci_combined40 <- ci_combined40[order(ci_combined40$cutrial_no), ]
  
  lo40 <- ci_combined40$reachdeviation_deg[, 1]
  hi40 <- ci_combined40$reachdeviation_deg[ ,2]
  cutrial_no <- ci_combined40$cutrial_no
  
  
  y_lower_limit <- -20
  y_upper_limit <- 80
  
  
  valid_indices40 <- lo40 >= y_lower_limit & lo40 <= y_upper_limit & hi40 >= y_lower_limit & hi40 <= y_upper_limit
  lo_filtered40 <- lo40[valid_indices40]
  hi_filtered40 <- hi40[valid_indices40]
  cutrial_no_filtered40 <- cutrial_no[valid_indices40]
  
  
  if (length(lo_filtered40) == length(hi_filtered40) && length(hi_filtered40) == length(cutrial_no_filtered40)) {
    plot(-1000, 1000, type = "n",
         main = "Reach Deviation Across All Trials", 
         xlab = "Trial", ylab = "Reach Deviation (degrees)",
         xlim = c(0, 256), ylim = c(-20, 80))
    
    
    #lines(all_data$Group.1, all_data$x, col = "hotpink", lwd = 2, pch = 16)  
    smoothreach40 <- smooth.spline(all_data40$Group.1, all_data40$x, spar = 0.02)
    lines(smoothreach40, col='tomato', lwd=2)
    abline(h = 0, col = "black", lty = 3)
    
    lo_smooth_filtered40 <- smooth.spline(cutrial_no_filtered40, lo_filtered40, spar = 0.008)
    hi_smooth_filtered40 <- smooth.spline(cutrial_no_filtered40, hi_filtered40, spar = 0.008)
    
    # Draw the polygon for the filtered CIs
    polygon(x = c(lo_smooth_filtered40$x, rev(hi_smooth_filtered40$x)),
            y = c(lo_smooth_filtered40$y, rev(hi_smooth_filtered40$y)),
            col = rgb(1, 0.388, 0.278, alpha = 0.2),
            border = NA)
  } else {
    stop("Lengths of lo_filtered, hi_filtered, and cutrial_no_filtered do not match!")
  }
  
  
  
  #####AIMING
  
  plotAlignedAim40 <- function() {
    aligned40_data <- getAligned40()
    
    group1_c <- do.call(rbind, aligned_data$group1)
    group2_c <- do.call(rbind, aligned_data$group2)
    
    
    group1avg <- aggregate(group1_c$aimdeviation_deg, by = list(group1_c$cutrial_no), FUN = mean)
    group2avg <- aggregate(group2_c$aimdeviation_deg, by = list(group2_c$cutrial_no), FUN = mean)
    c_data <- rbind(group1avg, group2avg)
    
    plot(-1000,1000,
         type = "n", col = "black",
         main = "Average Aim Deviation during Aligned Trials", xlab = "Trial Number", ylab = "Aiming Strategy",
         xlim=c(0,104),  ylim = c(-20,80))
    lines(c_data$x, col="cadetblue")
    abline(h=0, col="black", lty=3)
    
  }
  
  
  plotStrategy40 <- function () {
    
    rotated40_data <- getRotated40()
    
    group1_combined <- do.call(rbind, group1_rotated)
    group2_combined <- do.call(rbind, group2_rotated)
    
    group1_avg <- aggregate(group1_combined$aimdeviation_deg, by = list(group1_combined$cutrial_no), FUN = mean)
    group2_avg <- aggregate(group2_combined$aimdeviation_deg, by = list(group2_combined$cutrial_no), FUN = mean)
    combined_data <- merge(group1_avg, group2_avg, by = "Group.1", all = TRUE)
    
    # Calculate the average of the deviations, ignoring NAs
    combined_data$avg_deviation <- rowMeans(combined_data[, c("x.x", "x.y")], na.rm = TRUE)
    
    # Plot the combined data
    plot(combined_data$Group.1, combined_data$avg_deviation, type="l", col="purple", 
         xlim=c(89, 232), ylim=c(-20, 80), 
         xlab="Trial Number", ylab="Aim Deviation (degrees)", 
         main="Aiming Strategy During Rotation Trials")
    
    abline(h=0,col="black",lty=3)
    
  }
  
  
  plotAfterAim40 <- function () {
    
    after40_data <- getAfter40()
    
    g1_combined <- do.call(rbind, group1_after)
    g2_combined <- do.call(rbind, group2_after)
    
    g1_avg <- aggregate(g1_combined$aimdeviation_deg, by = list(g1_combined$cutrial_no), FUN = mean)
    g2_avg <- aggregate(g2_combined$aimdeviation_deg, by = list(g2_combined$cutrial_no), FUN = mean)
    
    combined_d <- rbind(g1_avg, g2_avg)
    
    
    plot(-1000, 1000, type = "n",
         main = "Aim Deviation After Rotation",xlab = "Trial Number", ylab = "Strategy Change", 
         xlim = c(209, 256), ylim = c(-5, 80))
    
    lines(combined_d$Group.1, combined_d$x, col = "deeppink", lwd = 2, pch = 16)  
    
    abline(h=0, col="black", lty=3)
    abline(v=233, col="turquoise3", lty=3)
    text(x = 233, y = 0, labels = "Left hand Trials for Group 1", col = "black", pos = 4, cex = 1)
  }
  
  
  plotAllAim40 <- function () {
    
    aligned40_data <- getAligned40()
    rotated40_data <- getRotated40()
    after40_data <- getAfter40()
    
    
    group1_c40 <- do.call(rbind, aligned40_data$group1)
    group2_c40 <- do.call(rbind, aligned40_data$group2)
    group1_combined40 <- do.call(rbind, rotated40_data$group1)
    group2_combined40 <- do.call(rbind, rotated40_data$group2)
    g1_combined40 <- do.call(rbind, after40_data$group1)
    g2_combined40 <- do.call(rbind, after40_data$group2)
    
    group1avg40 <- aggregate(group1_c40$aimdeviation_deg, by = list(group1_c40$cutrial_no), FUN = mean)
    group2avg40 <- aggregate(group2_c40$aimdeviation_deg, by = list(group2_c40$cutrial_no), FUN = mean)
    group1_avg40 <- aggregate(group1_combined40$aimdeviation_deg, by = list(group1_combined40$cutrial_no), FUN = mean)
    group2_avg40 <- aggregate(group2_combined40$aimdeviation_deg, by = list(group2_combined40$cutrial_no), FUN = mean)
    g1_avg40 <- aggregate(g1_combined40$aimdeviation_deg, by = list(g1_combined40$cutrial_no), FUN = mean)
    g2_avg40 <- aggregate(g2_combined40$aimdeviation_deg, by = list(g2_combined40$cutrial_no), FUN = mean)
    
    
    all_data40 <- rbind(
      group1avg40, group2avg40, 
      group1_avg40, group2_avg40, 
      g1_avg40 ,  g2_avg40)
    
    all_data40 <- all_data40[order(all_data40$Group.1), ] 
    
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
    
    ci_list40 <- list(
      CI(group1_c40),
      CI(g1_combined40),
      CI(group1_combined40))
   
    ci_combined40 <- do.call(rbind, ci_list40)
    ci_combined40 <- ci_combined40[order(ci_combined40$cutrial_no), ]
    
    
    lo <- ci_combined40$aimdeviation_deg [, 1]
    hi <- ci_combined40$aimdeviation_deg[ ,2]
    cutrial_no <- ci_combined40$cutrial_no
    
    
    y_lower_limit <- -20
    y_upper_limit <- 80
    
    
    valid_indices40 <- lo >= y_lower_limit & lo <= y_upper_limit & hi >= y_lower_limit & hi <= y_upper_limit
    lo_filtered40 <- lo[valid_indices40]
    hi_filtered40 <- hi[valid_indices40]
    cutrial_no_filtered40 <- cutrial_no[valid_indices40]
    
    
    if (length(lo_filtered40) == length(hi_filtered40) && length(hi_filtered40) == length(cutrial_no_filtered40)) {
      plot(-1000, 1000, type = "n",
           main = "Aiming Strategies with a 40° Rotation", 
           xlab = "Trial", ylab = "Mean Aim Deviation (°)",
           xlim = c(0, 256), ylim = c(-20, 70))
      
      #lines(all_data$Group.1, all_data$x, col = "hotpink", lwd = 2, pch = 16)  
      all_data_clean40 <- all_data40[!is.na(all_data40$Group.1) & !is.na(all_data40$x) & 
                                   !is.infinite(all_data40$Group.1) & !is.infinite(all_data40$x), ]
      
      
      smoothall40 <- smooth.spline(all_data_clean40$Group.1, all_data_clean40$x, spar = 0.0001)
      lines(smoothall40, col='magenta4', lwd=2)
      abline(h = 0, col = "black", lty = 3)
      
      lo_smooth_filtered40 <- smooth.spline(cutrial_no_filtered40, lo_filtered40, spar = 0.008)
      hi_smooth_filtered40 <- smooth.spline(cutrial_no_filtered40, hi_filtered40, spar = 0.008)
      
      # Draw the polygon for the filtered CIs
      polygon(x = c(lo_smooth_filtered40$x, rev(hi_smooth_filtered40$x)),
              y = c(lo_smooth_filtered40$y, rev(hi_smooth_filtered40$y)),
              col =rgb(0.6, 0, 0.6, alpha = 0.2) ,
              border = NA)
    } else {
      stop("Lengths of lo_filtered, hi_filtered, and cutrial_no_filtered do not match!")
    }
}
