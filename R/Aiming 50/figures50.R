plotAligned50 <- function() {
  aligned50_data <- getAligned50()
  
  group1_c <- do.call(rbind, aligned50_data$group1)
  group2_c <- do.call(rbind, aligned50_data$group2)
  
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


plotRotated50 <- function () {
  
  rotated50_data <- getRotated50()
  
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


plotAfter50 <- function () {
  
  after50_data <- getAfter50()
  
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


plotAll50 <- function() {
  
  aligned50_data <- getAligned50()
  rotated50_data <- getRotated50()
  after50_data <- getAfter50()
  
  
  group1_c50 <- do.call(rbind, aligned50_data$group1)
  group2_c50 <- do.call(rbind, aligned50_data$group2)
  group1_combined50 <- do.call(rbind, rotated50_data$group1)
  group2_combined50 <- do.call(rbind, rotated50_data$group2)
  g1_combined50 <- do.call(rbind, after50_data$group1)
  g2_combined50 <- do.call(rbind, after50_data$group2)
  
  group1avg50 <- aggregate(group1_c50$reachdeviation_deg, by = list(group1_c50$cutrial_no), FUN = mean)
  group2avg50 <- aggregate(group2_c50$reachdeviation_deg, by = list(group2_c50$cutrial_no), FUN = mean)
  group1_avg50 <- aggregate(group1_combined50$reachdeviation_deg, by = list(group1_combined50$cutrial_no), FUN = mean)
  group2_avg50 <- aggregate(group2_combined50$reachdeviation_deg, by = list(group2_combined50$cutrial_no), FUN = mean)
  g1_avg50 <- aggregate(g1_combined50$reachdeviation_deg, by = list(g1_combined50$cutrial_no), FUN = mean)
  g2_avg50 <- aggregate(g2_combined50$reachdeviation_deg, by = list(g2_combined50$cutrial_no), FUN = mean)
  
  
  all_data50 <- rbind(
    group1avg50, group2avg50, 
    group1_avg50, group2_avg50, 
    g1_avg50 ,  g2_avg50)
  
  all_data50 <- all_data50[order(all_data50$Group.1), ] 
  
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
  
  ci_list50 <- list(
    CI(group1_c50),
    CI(group2_c50),
    CI(g1_combined50),
    CI(g2_combined50),
    CI(group1_combined50),
    CI(group2_combined50)
  )
  
  ci_combined50 <- do.call(rbind, ci_list50)
  ci_combined50 <- ci_combined50[order(ci_combined50$cutrial_no), ]
  
  lo50 <- ci_combined50$reachdeviation_deg[, 1]
  hi50 <- ci_combined50$reachdeviation_deg[ ,2]
  cutrial_no50 <- ci_combined50$cutrial_no
  
  
  y_lower_limit <- -20
  y_upper_limit <- 80
  
  
  valid_indices50 <- lo50 >= y_lower_limit & lo50 <= y_upper_limit & hi50 >= y_lower_limit & hi50 <= y_upper_limit
  lo_filtered50 <- lo50[valid_indices50]
  hi_filtered50 <- hi50[valid_indices50]
  cutrial_no_filtered50 <- cutrial_no[valid_indices50]
  
  
  if (length(lo_filtered50) == length(hi_filtered50) && length(hi_filtered50) == length(cutrial_no_filtered50)) {
    plot(-1000, 1000, type = "n",
         main = "Reach Deviation Across All Trials", 
         xlab = "Trial", ylab = "Reach Deviation (degrees)",
         xlim = c(0, 256), ylim = c(-20, 80))
    
    
    #lines(all_data$Group.1, all_data$x, col = "hotpink", lwd = 2, pch = 16)  
    smoothreach50 <- smooth.spline(all_data50$Group.1, all_data50$x, spar = 0.02)
    lines(smoothreach50, col='tomato', lwd=2)
    abline(h = 0, col = "black", lty = 3)
    
    lo_smooth_filtered50 <- smooth.spline(cutrial_no_filtered50, lo_filtered50, spar = 0.008)
    hi_smooth_filtered50 <- smooth.spline(cutrial_no_filtered50, hi_filtered50, spar = 0.008)
    
    # Draw the polygon for the filtered CIs
    polygon(x = c(lo_smooth_filtered50$x, rev(hi_smooth_filtered50$x)),
            y = c(lo_smooth_filtered50$y, rev(hi_smooth_filtered50$y)),
            col = rgb(1, 0.388, 0.278, alpha = 0.2),
            border = NA)
  } else {
    stop("Lengths of lo_filtered, hi_filtered, and cutrial_no_filtered do not match!")
  }
}
  
 
  
  #####AIMING
  
  plotAlignedAim50 <- function() {
    aligned50_data <- getAligned50()
    
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
  
  
  plotStrategy50 <- function () {
    
    rotated50_data <- getRotated50()
    
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
  
  plotAfter50 <- function () { 
 
   after50_data <- getAfter50() 
  
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

  
  plotAllAim50 <- function () {
    
    aligned50_data <- getAligned50()
    rotated50_data <- getRotated50()
    after50_data <- getAfter50()
    
    
    group1_c50 <- do.call(rbind, aligned50_data$group1)
    group2_c50 <- do.call(rbind, aligned50_data$group2)
    group1_combined50 <- do.call(rbind, rotated50_data$group1)
    group2_combined50 <- do.call(rbind, rotated50_data$group2)
    g1_combined50 <- do.call(rbind, after50_data$group1)
    g2_combined50 <- do.call(rbind, after50_data$group2)
    
    group1avg50 <- aggregate(group1_c50$aimdeviation_deg, by = list(group1_c50$cutrial_no), FUN = mean)
    group2avg50 <- aggregate(group2_c50$aimdeviation_deg, by = list(group2_c50$cutrial_no), FUN = mean)
    group1_avg50 <- aggregate(group1_combined50$aimdeviation_deg, by = list(group1_combined50$cutrial_no), FUN = mean)
    group2_avg50 <- aggregate(group2_combined50$aimdeviation_deg, by = list(group2_combined50$cutrial_no), FUN = mean)
    g1_avg50 <- aggregate(g1_combined50$aimdeviation_deg, by = list(g1_combined50$cutrial_no), FUN = mean)
    g2_avg50 <- aggregate(g2_combined50$aimdeviation_deg, by = list(g2_combined50$cutrial_no), FUN = mean)
    
    
    all_data50 <- rbind(
      group1avg50, group2avg50, 
      group1_avg50, group2_avg50, 
      g1_avg50 ,  g2_avg50)
    
    all_data50 <- all_data50[order(all_data50$Group.1), ] 
    
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
    
    ci_list50 <- list(
      CI(group1_c50),
      CI(group2_c50),
      CI(g1_combined50),
      CI(group1_combined50),
      CI(group2_combined50))
    
    
    ci_combined50 <- do.call(rbind, ci_list50)
    ci_combined50 <- ci_combined50[order(ci_combined50$cutrial_no), ]
    
    
    lo <- ci_combined50$aimdeviation_deg [, 1]
    hi <- ci_combined50$aimdeviation_deg[ ,2]
    cutrial_no <- ci_combined50$cutrial_no
    
    
    y_lower_limit <- -20
    y_upper_limit <- 80
    
    
    valid_indices50 <- lo >= y_lower_limit & lo <= y_upper_limit & hi >= y_lower_limit & hi <= y_upper_limit
    lo_filtered50 <- lo[valid_indices50]
    hi_filtered50 <- hi[valid_indices50]
    cutrial_no_filtered50 <- cutrial_no[valid_indices50]
    
    
    if (length(lo_filtered50) == length(hi_filtered50) && length(hi_filtered50) == length(cutrial_no_filtered50)) {
      plot(-1000, 1000, type = "n",
           main = "Aiming Strategies with a 50° Rotation", 
           xlab = "Trial", ylab = "Mean Aim Deviation (°)",
           xlim = c(0, 256), ylim = c(-20, 70))
      
      #lines(all_data$Group.1, all_data$x, col = "hotpink", lwd = 2, pch = 16)  
      all_data_clean50 <- all_data50[!is.na(all_data50$Group.1) & !is.na(all_data50$x) & 
                                       !is.infinite(all_data50$Group.1) & !is.infinite(all_data50$x), ]
      
      
      smoothall50 <- smooth.spline(all_data_clean50$Group.1, all_data_clean50$x, spar = 0.0001)
      lines(smoothall50, col='magenta4', lwd=2)
      abline(h = 0, col = "black", lty = 3)
      
      lo_smooth_filtered50 <- smooth.spline(cutrial_no_filtered50, lo_filtered50, spar = 0.008)
      hi_smooth_filtered50 <- smooth.spline(cutrial_no_filtered50, hi_filtered50, spar = 0.008)
      
      # Draw the polygon for the filtered CIs
      polygon(x = c(lo_smooth_filtered50$x, rev(hi_smooth_filtered50$x)),
              y = c(lo_smooth_filtered50$y, rev(hi_smooth_filtered50$y)),
              col =rgb(0.6, 0, 0.6, alpha = 0.2) ,
              border = NA)
    } else {
      stop("Lengths of lo_filtered, hi_filtered, and cutrial_no_filtered do not match!")
    }
}
