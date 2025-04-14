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

