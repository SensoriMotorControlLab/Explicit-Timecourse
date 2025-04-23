plotAllAimOverlay <- function() {
  plot(-1000, 1000, type = "n", xlim = c(1, 256), ylim = c(-5, 60),
       xlab = "Trial", ylab = "Mean Aim Deviation (°)",
       main = "Aiming Deviation Across Rotation Sizes")
  
  lines(smoothall40, col='orange', lwd=2) 
  lines(smoothall50, col='hotpink', lwd=2)  
  lines(smoothall60, col='cyan', lwd=2) 
  lines(smoothall30, col='springgreen', lwd=2)
  lines(smoothall20, col='darkviolet', lwd=2)
  abline(v=233, col="black", lty=3)
 # text(x = 233, y = 0, labels = "Left hand Trials for Group 1", col = "black", pos = 2, cex = 1)
  
  # Add a legend
  legend("topleft", legend = c("20°", "30°", "40°", "50°", "60°"),
         col = c("darkviolet", "springgreen","orange", "hotpink", "cyan"), lty = 1, lwd = 2)
  

}


plotAllReachOverlay <- function() {
  
  plot(-1000, 1000, type = "n", xlim = c(1, 256), ylim = c(-5, 60),
       xlab = "Trial", ylab = "Mean Aim Deviation (°)",
       main = "Reach Deviation Across Rotation Sizes")

  lines(smoothreach40, col='magenta', lwd=2) 
  lines(smoothreach50, col='orange', lwd=2)  
  lines(smoothreach60, col='cyan', lwd=2) 
  lines(smoothreach30, col='seagreen3', lwd=2)
  lines(smoothreach20, col='mediumpurple', lwd=2)
  
  #add exponential fit
  #lines(all_data40$Group.1, y_fit40, col="black", lty=1, lwd=2)
 # lines(all_data50$Group.1, y_fit50, col="black", lty=2, lwd=2)
  #lines(all_data60$Group.1, y_fit60, col= "black"", lty=3, lwd=2)
  avg_y <- (y_fit40 + y_fit50 + y_fit60) / 3
  #lines(all_data40$Group.1, avg_y, col="black", lty=1, lwd=1)  # Red, dashed line for the average
  

  abline(v=233, col="black", lty=3)
  #text(x = 233, y = 0, labels = "Left hand Trials for Group 1", col = "black", pos = 2, cex = 1)

  legend("topright", legend = c("20°", "30°", "40°", "50°", "60°"),
         col = c( "mediumpurple", "seagreen3", "magenta", "orange", "cyan"), lty = 1, lwd = 2)
}



#plot reach dev exponentials

plotExponentialReachOverlay <- function() {
  timepoints <- 0:256
  
  params_60 <- c(lambda=0.006605567, N0=59.463737995)
  params_50 <- c(lambda=0.005643455, N0=48.615362434)
  params_40 <- c(lambda=0.005190853, N0=37.533714916)
  params_30 <- c(lambda=0.007636479 , N0=29.624525083 )
  params_20 <- c(lambda=0.01035161 , N0=17.40091762  )
  
  curve_60 <- params_60["N0"] * (1 - exp(-params_60["lambda"] * timepoints))
  curve_50 <- params_50["N0"] * (1 - exp(-params_50["lambda"] * timepoints))
  curve_40 <- params_40["N0"] * (1 - exp(-params_40["lambda"] * timepoints))
  curve_30 <- params_30["N0"] * (1 - exp(-params_30["lambda"] * timepoints))
  curve_20 <- params_20["N0"] * (1 - exp(-params_20["lambda"] * timepoints))
  
  plot(timepoints, curve_60, type="l", col="cyan", lwd=2, 
       ylim=c(0, 60), 
       xlab="Trial", ylab="Reach Deviation (°)", 
       main="Exponential Learning Curves in Different Aiming Conditions")
  
  lines(timepoints, curve_50, col= "magenta", lwd=2)
  lines(timepoints, curve_40, col="orange", lwd=2)
  lines(timepoints, curve_30, col= "cornflowerblue", lwd=2)
  lines(timepoints, curve_20, col= "limegreen", lwd=2)
  
  legend("topleft", legend=c("60°", "50°", "40°", "30°", "20°"), 
         col=c("cyan", "magenta", "orange","cornflowerblue",'limegreen'), lty=1, lwd=2)
  
  
}


#plot aim dev exponentials

plotExponentialAimOverlay <- function() {
  timepoints <- 0:256
  
  params_60aim <- c(lambda=0.003549872, N0=48.987139824 )
  params_50aim <- c(lambda=0.003647977, N0=24.259366886 )
  params_40aim <- c(lambda=0.003168895, N0=20.519496217 )
  params_30aim <- c(lambda=0.0036941, N0=17.6704411)
  params_20aim <- c(lambda=0.003574507, N0=4.237782708 )
  
  curve_60aim <- params_60aim["N0"] * (1 - exp(-params_60aim["lambda"] * timepoints))
  curve_50aim <- params_50aim["N0"] * (1 - exp(-params_50aim["lambda"] * timepoints))
  curve_40aim <- params_40aim["N0"] * (1 - exp(-params_40aim["lambda"] * timepoints))
  curve_30aim <- params_30aim["N0"] * (1 - exp(-params_30aim["lambda"] * timepoints))
  curve_20aim <- params_20aim["N0"] * (1 - exp(-params_20aim["lambda"] * timepoints))
  
  plot(timepoints, curve_60aim, type="l", col="cyan", lwd=2, 
       ylim=c(0, 60), 
       xlab="Trial", ylab="Aim Deviation (°)", 
       main="Exponential Learning Curves in Different Aiming Conditions")
  
  lines(timepoints, curve_50aim, col="magenta", lwd=2)
  lines(timepoints, curve_40aim, col="orange", lwd=2)
  lines(timepoints, curve_30aim, col="dodgerblue", lwd=2)
  lines(timepoints, curve_20aim, col="limegreen", lwd=2)
  
  legend("topleft", legend=c("60°", "50°", "40°", "30°", "20°"), 
         col=c("cyan", "magenta", "orange","dodgerblue",'limegreen'), lty=1, lwd=2)
}


plotStrategies <- function () {
  plot_all_rotated <- rbind(
    merge(last20_rotated, ci_compare20[, c("participant_id", "aim_shift")], by = "participant_id"),
    merge(last50_rotated, ci_compare50[, c("participant_id", "aim_shift")], by = "participant_id"),
    merge(last40_rotated, ci_compare40[, c("participant_id", "aim_shift")], by = "participant_id"),
    merge(last30_rotated, ci_compare30[, c("participant_id", "aim_shift")], by = "participant_id"),
    merge(last60_rotated, ci_compare[, c("participant_id", "aim_shift")], by = "participant_id")
  )
  
#add col
  last20_rotated$rotation_size <- 20
  last30_rotated$rotation_size <- 30
  last40_rotated$rotation_size <- 40
  last50_rotated$rotation_size <- 50
  last60_rotated$rotation_size <- 60
  
  plot_all_rotated <- rbind(
    merge(last20_rotated, ci_compare20[, c("participant_id", "aim_shift")], by = "participant_id"),
    merge(last30_rotated, ci_compare30[, c("participant_id", "aim_shift")], by = "participant_id"),
    merge(last40_rotated, ci_compare40[, c("participant_id", "aim_shift")], by = "participant_id"),
    merge(last50_rotated, ci_compare50[, c("participant_id", "aim_shift")], by = "participant_id"),
    merge(last60_rotated, ci_compare[, c("participant_id", "aim_shift")], by = "participant_id")
  )
  
  
  yes_strategies <- subset(plot_all_rotated, aim_shift == "Yes")
  
  #make it categorical
  yes_strategies$rotation_size <- factor(yes_strategies$rotation_size)
  

  ggplot(yes_strategies, aes(x = rotation_size, y = aimdeviation_deg, color = rotation_size)) +
    geom_jitter(width = 0.1, size = 3, alpha = 0.7) +
    scale_color_manual(values = c(
      "20" = "deeppink",
      "30" = "orange",
      "40" = "blue",
      "50" = "black",
      "60" = "cyan"
    )) +
    scale_y_continuous(
      breaks = c(0,10,20, 30, 40, 50, 60),  # Adjust the breaks on the y-axis
      labels = c("0","10", "20", "30", "40", "50", "60"),
      limits = c(0, 60)
    ) + 
      labs(
      title = "Aiming Deviation for Strategy-Users by Rotation Size",
      x = "Rotation Size (Degrees)",
      y = "Aiming Deviation (Degrees)",
      color = "Rotation Size"
    ) +
    theme_minimal() +
    theme(legend.position = "top") 
}
