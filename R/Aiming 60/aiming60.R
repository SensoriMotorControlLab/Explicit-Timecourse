#average aim deviation
aim60_path <- 'data/Instructed_summary/aiming60'
all_60data <- list()
aim60files <- list.files(aim60_path, pattern = "*.csv", full.names = TRUE)

all_aim_deviations <- list()

remove_na <- function(x) {
  return(x[!is.na(x)])  # Keep only non-NA values
}

for (i in 1:length(aim60files)) {
  df <- read.csv(aim60files[i], stringsAsFactors = FALSE)
  all_aim_deviations[[i]] <- remove_na(df$aimdeviation_deg)
}


combinedaimdev <- do.call(cbind, all_aim_deviations)
average_aim_deviation <- apply(combinedaimdev, 1, mean, na.rm = TRUE)


plot(average_aim_deviation, type = "n", col = "grey", lwd = 2, xlim = c(0, 256), ylim = c(-30, 60),
     main = "Average Aim Deviation with a 60 Degree Perturbation", xlab = "Trial", ylab = "Aim Deviation (degrees)")
smooth_line<- smooth.spline(average_aim_deviation, spar = 0.32)
lines(smooth_line, col = "black", lwd = 1)

se_aim <- sd(average_aim_deviation) / sqrt(length(average_aim_deviation))
upper_aim <- average_aim_deviation + 1.96 * se_aim
lower_aim <- average_aim_deviation - 1.96 * se_aim
smooth_line1 <- smooth.spline(average_aim_deviation, spar = 0.32)
smooth_upper_aim <- smooth.spline(upper_aim, spar = 0.32)
smooth_lower_aim <- smooth.spline(lower_aim, spar = 0.32)
polygon(c(smooth_upper_aim$x, rev(smooth_lower_aim$x)), 
        c(smooth_upper_aim$y, rev(smooth_lower_aim$y)), 
        col = rgb(0, 0, 0, alpha = 0.2), border = NA) #lower alpha = more transparent



rotation_start_G1 <- 89  # Rotation starts for Group 1
rotation_start_G2 <- 105  # Rotation starts for Group 2

abline(v = rotation_start_G1, col = "skyblue", lwd = 1.5, lty = 2)  # Rotation start for G1
abline(v = rotation_start_G2, col = "firebrick2", lwd = 1.5, lty = 2)  # Rotation start for G2
text(rotation_start_G1, 55, "OG", col = "skyblue", cex = 0.8, pos = 4)
text(rotation_start_G2, 55, "New ", col = "firebrick2", cex = 0.8, pos = 4)
#Where G2 is the participatns that got hte new paradigm (March 3)


abline (h=0, col="black", lwd = 2, lty=3)


#each participant
aim60_path <- 'data/Instructed_summary/aiming60'
all_60data <- list()
aim60files <- list.files(aim60_path, pattern = "*.csv", full.names = TRUE)

all_aim_deviations <- list()


for (i in 1:length(aim60files)) {
  df <- read.csv(aim60files[i], stringsAsFactors = FALSE)
  all_aim_deviations[[i]] <- df$aimdeviation_deg
}

# Remove empty cells (where aiming didnt occur)
remove_na <- function(x) {
  return(x[!is.na(x)])  # Keep only non-NA values
}

cleaned_aim <- lapply(all_aim_deviations, remove_na)

combinedaimdev <- do.call(cbind,all_aim_deviations)

summary(df$aimdeviation_deg)
which(is.na(df$aimdeviation_deg))

# Plot one participant to see trial by trial
plot(cleaned_aim[[13]], , col = "deeppink", type = "p", lwd = 2,
     xlim = c(0, 256), ylim = c(-20, 60),
     main = "Aim Deviation with a 60 Degree Perturbation",
     xlab = "Trial", ylab = "Aim Strategy (degrees)")

abline (h=0, col="black", lwd = 1.5, lty=3)

#for participant 13, trials above 200 are removed? 
plot(1:length(all_aim_deviations[[13]]), all_aim_deviations[[13]], col = "deeppink", type = "p", lwd = 2,
     xlim = c(0, 256), ylim = c(-20, 60),
     main = "Aim Deviation with a 60 Degree Perturbation",
     xlab = "Trial", ylab = "Aim Strategy (degrees)")


#8, 6, 13 has really messy data in beginning.. not sure how to go about cleaning that at the moment.



#compare reach and aim dev
#plot(average_aim_deviation, type = "l", col = "magenta2", lwd = 2, 
    # xlim = c(0, 256), ylim = c(-20, 70), 
   #  main = "Averages for Aim and Reach Deviation among Participants with a 60 Degree Pertuburbation", 
     #xlab = "Trial", ylab = "Aim Deviation (degrees)")
#lines(average_reach_deviation, col = "black", lwd = 2)
#legend("bottomright", legend = c("Aim Deviation", "Reach Deviation"), col = c("magenta2", "black"), lwd = 2)



#we can smooth out the data and add a sahded region that covers the CI 
smooth_line1 <- smooth.spline(average_aim_deviation, spar = 0.32)
lines(smooth_line1, col = "magenta2", lwd = 3)
smooth_line2 <- smooth.spline(average_reach_deviation, spar = 0.32)
lines(smooth_line2, col = "black", lwd = 3)
legend("topright", legend = c("Aim Deviation", "Reach Deviation"), col = c("magenta2", "black"), lwd = 2)

abline (h=0, col="black", lwd = 1.5, lty=3)

#find se for ci intervals
se_aim <- sd(average_aim_deviation) / sqrt(length(average_aim_deviation))
upper_aim <- average_aim_deviation + 1.96 * se_aim
lower_aim <- average_aim_deviation - 1.96 * se_aim

se_reach <- sd(average_reach_deviation) / sqrt(length(average_reach_deviation))
upper_reach <- average_reach_deviation + 1.96 * se_reach
lower_reach <- average_reach_deviation - 1.96 * se_reach

#"smooth" the data points
smooth_line1 <- smooth.spline(average_aim_deviation, spar = 0.32) #lower spar = less smooth
smooth_line2 <- smooth.spline(average_reach_deviation, spar = 0.32)
smooth_upper_aim <- smooth.spline(upper_aim, spar = 0.32)
smooth_lower_aim <- smooth.spline(lower_aim, spar = 0.32)
smooth_upper_reach <- smooth.spline(upper_reach, spar = 0.32)
smooth_lower_reach <- smooth.spline(lower_reach, spar = 0.32)

plot(1:length(average_aim_deviation), average_aim_deviation, type = "n", 
     xlim = c(0, length(average_aim_deviation)), 
     ylim = range(c(lower_aim, upper_aim, lower_reach, upper_reach)), 
     main = "Averages for Aim and Reach Deviation", 
     xlab = "Trial", ylab = "Deviation (degrees)")


polygon(c(smooth_upper_aim$x, rev(smooth_lower_aim$x)), 
        c(smooth_upper_aim$y, rev(smooth_lower_aim$y)), 
        col = rgb(1, 0, 1, alpha = 0.26), border = NA) #lower alpha = more transparent

polygon(c(smooth_upper_reach$x, rev(smooth_lower_reach$x)), 
        c(smooth_upper_reach$y, rev(smooth_lower_reach$y)), 
        col = rgb(0, 0, 0, alpha = 0.26), border = NA)

#plot
lines(smooth_line1, col = "magenta2", lwd = 3)
lines(smooth_line2, col = "black", lwd = 3)
legend("topright", legend = c("Aim Deviation", "Reach Deviation"), 
       col = c("magenta2", "black"), lwd = 2)

abline(h = 0, col = "black", lwd = 1.5, lty = 3)




#Extract the Aligned Phase 

getAligned <- function () {
  data_path <- "data/Instructed_summary/aiming60/"
  remove_na <- function(x) {
    return(x[!is.na(x)])  # Keep only non-NA values
  
  
  # Group 1 file paths
    group1_files <- file.path(data_path, c("SUMMARY_aiming60_7eec53.csv", 
                                           "SUMMARY_aiming60_13d986.csv", "SUMMARY_aiming60_33e532.csv", 
                                           "SUMMARY_aiming60_4093e8.csv", 
                                           "SUMMARY_aiming60_a23b35.csv"))
    
    group2_files <- file.path(data_path, c("SUMMARY_aiming60_7cd1bd.csv", 
                                           "SUMMARY_aiming60_654648.csv", "SUMMARY_aiming60_f275ca.csv"))
  group1_data <- list()
  group2_data <- list()
  
  for (file in group1_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    df$cutrial_no <- as.integer(df$cutrial_no)
    aligned <- df[df$cutrial_no >= 1 & df$cutrial_no <= 88, c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    #print(nrow(aligned)) 
    
    group1_data[[length(group1_data) + 1]] <- aligned
  }
  
  # Extract trials for Group 2 (Trial 113 to 232)
  for (file in group2_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    aligned <- df[df$cutrial_no %in% c(1:24, 41:56, 65:72, 81:88, 97:104), c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group2_data[[length(group2_data) + 1]] <- aligned
  }
  return(list(group1 = group1_data, group2 = group2_data))
  #print(nrow(group1_data[[1]]))
  #print(nrow(group2_data[[1]]))
  
}

aligned_data <- getAligned()




#Extract the Rotated Phase
getRotated <- function () {
  
  data_path <- "data/Instructed_summary/aiming60/"
  
  group1_files <- file.path(data_path, c("SUMMARY_aiming60_7eec53.csv", 
                                         "SUMMARY_aiming60_13d986.csv", "SUMMARY_aiming60_33e532.csv", 
                                         "SUMMARY_aiming60_4093e8.csv", 
                                         "SUMMARY_aiming60_a23b35.csv"))
  
  group2_files <- file.path(data_path, c("SUMMARY_aiming60_7cd1bd.csv", 
                                         "SUMMARY_aiming60_654648.csv", "SUMMARY_aiming60_f275ca.csv"))
  group1_rotated <- list()
  group2_rotated <- list()
  
  # Group 1 (trial 89 to 208)
  for (file in group1_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    rotated <- df[df$cutrial_no >= 89 & df$cutrial_no <= 208,c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group1_rotated[[length(group1_rotated) + 1]] <- rotated
  }
  
  # Group 2 (trial 113 to 232)
  for (file in group2_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    rotated <- df[df$cutrial_no >= 113 & df$cutrial_no <= 232, c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group2_rotated[[length(group2_rotated) + 1]] <- rotated
  }
  
  return(list(group1 = group1_rotated, group2 = group2_rotated))
  #print(nrow(rotated_data$group1[[1]]))
  #print(nrow(rotated_data$group2[[1]]))
  
}
rotated_data <- getRotated()


getAfter <- function() {
  
  data_path <- "data/Instructed_summary/aiming60/"
  
  group1_files <- file.path(data_path, c("SUMMARY_aiming60_7eec53.csv", 
                                         "SUMMARY_aiming60_13d986.csv", "SUMMARY_aiming60_33e532.csv", 
                                         "SUMMARY_aiming60_4093e8.csv", 
                                         "SUMMARY_aiming60_a23b35.csv"))
  
  group2_files <- file.path(data_path, c("SUMMARY_aiming60_7cd1bd.csv", 
                                         "SUMMARY_aiming60_654648.csv", "SUMMARY_aiming60_f275ca.csv"))
  
  group1_after <- list()
  group2_after <- list()
  
  # Group 1 (trial 233 to 256) left hand trials
  for (file in group1_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    after <- df[df$cutrial_no >= 209 & df$cutrial_no <= 232,c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group1_after[[length(group1_after) + 1]] <- after
  }
  
  # Group 2 (trial 113 to 232) aftereffect trials
  for (file in group2_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    after <- df[df$cutrial_no >= 233 & df$cutrial_no <= 256, c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group2_after[[length(group2_after) + 1]] <- after
    
    return(list(group1 = group1_after, group2 = group2_after))
    #print(nrow(after_data$group1[[1]]))
    #print(nrow(after_data$group2[[1]])) 
    
    
  }
  
}
after_data <- getAfter()

