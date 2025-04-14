plotAligned60 <- function() {
  aligned60_data <- getAligned60()
  
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
  

plotRotated60 <- function () {
  
  rotated60_data <- getRotated60()
  
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


plotAfter60 <- function () {
  
  after60_data <- getAfter60()
  
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
  

plotAll60 <- function() {
  
  aligned60_data <- getAligned60()
  rotated60_data <- getRotated60()
  after60_data <- getAfter60()
  

  group1_c60 <- do.call(rbind, aligned60_data$group1)
  group2_c60 <- do.call(rbind, aligned60_data$group2)
  group1_combined60 <- do.call(rbind, rotated60_data$group1)
  group2_combined60 <- do.call(rbind, rotated60_data$group2)
  g1_combined60 <- do.call(rbind, after60_data$group1)
  g2_combined60 <- do.call(rbind, after60_data$group2)
  
  group1avg60 <- aggregate(group1_c60$reachdeviation_deg, by = list(group1_c60$cutrial_no), FUN = mean)
  group2avg60 <- aggregate(group2_c60$reachdeviation_deg, by = list(group2_c60$cutrial_no), FUN = mean)
  group1_avg60 <- aggregate(group1_combined60$reachdeviation_deg, by = list(group1_combined60$cutrial_no), FUN = mean)
  group2_avg60 <- aggregate(group2_combined60$reachdeviation_deg, by = list(group2_combined60$cutrial_no), FUN = mean)
  g1_avg60 <- aggregate(g1_combined60$reachdeviation_deg, by = list(g1_combined60$cutrial_no), FUN = mean)
  g2_avg60 <- aggregate(g2_combined60$reachdeviation_deg, by = list(g2_combined60$cutrial_no), FUN = mean)
  
  
all_data60 <- rbind(
  group1avg60, group2avg60, 
  group1_avg60, group2_avg60, 
  g1_avg60 ,  g2_avg60)

all_data60 <- all_data60[order(all_data60$Group.1), ] 
  
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

ci_list60 <- list(
  CI(group1_c60),
  CI(group2_c60),
  CI(g1_combined60),
  CI(g2_combined60),
  CI(group1_combined60),
  CI(group2_combined60)
)

ci_combined60 <- do.call(rbind, ci_list60)
ci_combined60 <- ci_combined60[order(ci_combined60$cutrial_no), ]

lo60 <- ci_combined60$reachdeviation_deg[, 1]
hi60 <- ci_combined60$reachdeviation_deg[ ,2]
cutrial_no60 <- ci_combined60$cutrial_no


y_lower_limit <- -20
y_upper_limit <- 80


valid_indices60 <- lo60 >= y_lower_limit & lo60 <= y_upper_limit & hi60 >= y_lower_limit & hi60 <= y_upper_limit
lo_filtered60 <- lo60[valid_indices60]
hi_filtered60 <- hi60[valid_indices60]
cutrial_no_filtered60 <- cutrial_no60[valid_indices60]


if (length(lo_filtered60) == length(hi_filtered60) && length(hi_filtered60) == length(cutrial_no_filtered60)) {
  plot(-1000, 1000, type = "n",
       main = "Reach Deviation Across All Trials", 
       xlab = "Trial", ylab = "Reach Deviation (degrees)",
       xlim = c(0, 256), ylim = c(-20, 80))
  

#lines(all_data$Group.1, all_data$x, col = "hotpink", lwd = 2, pch = 16)  
smoothreach60 <- smooth.spline(all_data60$Group.1, all_data60$x, spar = 0.02)
lines(smoothreach60, col='tomato', lwd=2)
abline(h = 0, col = "black", lty = 3)

lo_smooth_filtered60 <- smooth.spline(cutrial_no_filtered60, lo_filtered60, spar = 0.008)
hi_smooth_filtered60 <- smooth.spline(cutrial_no_filtered60, hi_filtered60, spar = 0.008)

# Draw the polygon for the filtered CIs
polygon(x = c(lo_smooth_filtered60$x, rev(hi_smooth_filtered60$x)),
        y = c(lo_smooth_filtered60$y, rev(hi_smooth_filtered60$y)),
        col = rgb(1, 0.388, 0.278, alpha = 0.2),
        border = NA)
} else {
  stop("Lengths of lo_filtered, hi_filtered, and cutrial_no_filtered do not match!")
}



#####AIMING

plotAlignedAim60 <- function() {
  aligned60_data <- getAligned60()
  
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


plotStrategy60 <- function () {
  
  rotated60_data <- getRotated60()
  
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


plotAfterAim60 <- function () {
  
  after60_data <- getAfter60()
  
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


plotAllAim60 <- function () {
  
  aligned60_data <- getAligned60()
  rotated60_data <- getRotated60()
  after60_data <- getAfter60()
  
  
  group1_c60 <- do.call(rbind, aligned60_data$group1)
  group2_c60 <- do.call(rbind, aligned60_data$group2)
  group1_combined60 <- do.call(rbind, rotated60_data$group1)
  group2_combined60 <- do.call(rbind, rotated60_data$group2)
  g1_combined60 <- do.call(rbind, after60_data$group1)
  g2_combined60 <- do.call(rbind, after60_data$group2)
  
  group1avg60 <- aggregate(group1_c60$aimdeviation_deg, by = list(group1_c60$cutrial_no), FUN = mean)
  group2avg60 <- aggregate(group2_c60$aimdeviation_deg, by = list(group2_c60$cutrial_no), FUN = mean)
  group1_avg60 <- aggregate(group1_combined60$aimdeviation_deg, by = list(group1_combined60$cutrial_no), FUN = mean)
  group2_avg60 <- aggregate(group2_combined60$aimdeviation_deg, by = list(group2_combined60$cutrial_no), FUN = mean)
  g1_avg60 <- aggregate(g1_combined60$aimdeviation_deg, by = list(g1_combined60$cutrial_no), FUN = mean)
  g2_avg60 <- aggregate(g2_combined60$aimdeviation_deg, by = list(g2_combined60$cutrial_no), FUN = mean)
  
  
  all_data60 <- rbind(
    group1avg60, group2avg60, 
    group1_avg60, group2_avg60, 
    g1_avg60 ,  g2_avg60)
  
  all_data60 <- all_data60[order(all_data60$Group.1), ] 
  
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
  
  ci_list60 <- list(
    CI(group1_c60),
    CI(group2_c60),
    CI(g1_combined60),
    CI(group1_combined60),
    CI(group2_combined60))
    
  
  ci_combined60 <- do.call(rbind, ci_list60)
  ci_combined60 <- ci_combined60[order(ci_combined60$cutrial_no), ]
  
  
  lo <- ci_combined60$aimdeviation_deg [, 1]
  hi <- ci_combined60$aimdeviation_deg[ ,2]
  cutrial_no <- ci_combined60$cutrial_no
  
  
  y_lower_limit <- -20
  y_upper_limit <- 80
  
  
  valid_indices60 <- lo >= y_lower_limit & lo <= y_upper_limit & hi >= y_lower_limit & hi <= y_upper_limit
  lo_filtered60 <- lo[valid_indices60]
  hi_filtered60 <- hi[valid_indices60]
  cutrial_no_filtered60 <- cutrial_no[valid_indices60]
  
  
  if (length(lo_filtered60) == length(hi_filtered60) && length(hi_filtered60) == length(cutrial_no_filtered60)) {
    plot(-1000, 1000, type = "n",
         main = "Aiming Strategies with a 60° Rotation", 
         xlab = "Trial", ylab = "Mean Aim Deviation (°)",
         xlim = c(0, 256), ylim = c(-20, 70))
    
    #lines(all_data$Group.1, all_data$x, col = "hotpink", lwd = 2, pch = 16)  
    all_data_clean60 <- all_data60[!is.na(all_data60$Group.1) & !is.na(all_data60$x) & 
                                     !is.infinite(all_data60$Group.1) & !is.infinite(all_data60$x), ]
    
    
    smoothall60 <- smooth.spline(all_data_clean60$Group.1, all_data_clean60$x, spar = 0.0001)
    lines(smoothall60, col='magenta4', lwd=2)
    abline(h = 0, col = "black", lty = 3)
    
    lo_smooth_filtered60 <- smooth.spline(cutrial_no_filtered60, lo_filtered60, spar = 0.008)
    hi_smooth_filtered60 <- smooth.spline(cutrial_no_filtered60, hi_filtered60, spar = 0.008)
    
    # Draw the polygon for the filtered CIs
    polygon(x = c(lo_smooth_filtered60$x, rev(hi_smooth_filtered60$x)),
            y = c(lo_smooth_filtered60$y, rev(hi_smooth_filtered60$y)),
            col =rgb(0.6, 0, 0.6, alpha = 0.2) ,
            border = NA)
  } else {
    stop("Lengths of lo_filtered, hi_filtered, and cutrial_no_filtered do not match!")
  }