#Aiming 60

# Create a function that includes all participants under the aiming 60 folder.
get60_Data <- function() {
  aim60_path <- 'data/Instructed_summary/aiming60'
  all_60data <- list()
  aim60files <- list.files(aim60_path, pattern = "*.csv", full.names = TRUE)
  
  for (file_path in aim60files) {
    df <- read.csv(file_path)
    all_60data[[length(all_60data) + 1]] <- df

    
      }
  
  
  return(all_60data)
}

aim60_data <- get60_Data()
print(length(aim60_data))


for (i in 1:length(aim60_data)) {
  df <- aim60_data[[i]]
  print(head(df))
}

length(aim60_data)
  
#quick visualization 
plot(df$reachdeviation_deg, main = "Reach Deviation Aiming 60 ", xlab = "Trial", ylab = "Reach Deviation (degrees)")


#REMOVE OUTLIERS 
df1 <- aim60_data[[1]]
df1$reachdeviation_deg[df1$reachdeviation_deg < 0] <- 0  
df1$reachdeviation_deg[df1$reachdeviation_deg > 85] <- 85  

plot(df1$reachdeviation_deg, type="l")


#apply to all participants

for (i in 1:length(aim60_data)) {
  df <- aim60_data[[i]]
  
  # Remove values outside the range 0 to 90 degrees
  df$reachdeviation_deg[df$reachdeviation_deg < -20] <--10
  df$reachdeviation_deg[df$reachdeviation_deg > 85] <- 85
  
}

df2 <- aim60_data[[1]]
df1$aimhdeviation_deg[df1$reachdeviation_deg < 0] <- 0  
df1$aimdeviation_deg[df1$reachdeviation_deg > 85] <- 85  

plot(df2$aimdeviation_deg, type="l")


#apply to all participants

for (i in 1:length(aim60_data)) {
  df <- aim60_data[[i]]
  
  # Remove values outside the range 0 to 90 degrees
  df$aimdeviation_deg[df$raimdeviation_deg < -20] <--10
  df$reachdeviation_deg[df$reachdeviation_deg > 85] <- 85
  
}

#COMFIRM LEARNERS---------
data_path <- "data/Instructed_summary/aiming60/"
group1_files <- file.path(data_path, c("SUMMARY_aiming60_1ad447.csv", "SUMMARY_aiming60_7eec53.csv", 
                                       "SUMMARY_aiming60_13d986.csv", "SUMMARY_aiming60_33e532.csv", 
                                       "SUMMARY_aiming60_86f3b3.csv", "SUMMARY_aiming60_98e5cb.csv", 
                                       "SUMMARY_aiming60_4093e8.csv", "SUMMARY_aiming60_a02c67.csv", 
                                       "SUMMARY_aiming60_a23b35.csv"))
learners <- 0  


for (file in group1_files) {

  df <- read.csv(file, stringsAsFactors = FALSE)
  rotated <- df[df$task_idx == 8, , drop = FALSE]
  close_to_60 <- sum(rotated$reachdeviation_deg >= 25 & rotated$reachdeviation_deg <= 85, na.rm = TRUE)
  proportion_close_to_60 <- close_to_60 / nrow(rotated)
  
  if (proportion_close_to_60 >= 0.5) {
    learners <- learners + 1
  }
}

print(learners)
#there are 9 out of 9 learners that countered the pertubation with a reach deviation ranging from 25 to 85 degrees

data_path <- "data/Instructed_summary/aiming60/"
group2_files <- file.path(data_path, c("SUMMARY_aiming60_7cd1bd.csv", "SUMMARY_aiming60_3091de.csv", "SUMMARY_aiming60_654648.csv", 
                                       "SUMMARY_aiming60_f275ca.csv"))



#now for the group within the new paradigm, which had the perturbation introduced at a different trial
learners <- 0  
for (file in group2_files) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  rotated <- df[df$task_idx == 12, , drop = FALSE]
  close_to_60 <- sum(rotated$reachdeviation_deg >= 30 & rotated$reachdeviation_deg <= 80, na.rm = TRUE)
  proportion_close_to_60 <- close_to_60 / nrow(rotated)
  
  if (proportion_close_to_60 >= 0.5) {
    learners <- learners + 1
  }
  
  
}

print(learners)
#there are 4/4 learners


#Extract the Aligned Phase 

getAligned60 <- function () {
  data_path <- "data/Instructed_summary/aiming60/"
  
  # Group 1 file paths
  group1_files <- file.path(data_path, c("SUMMARY_aiming60_1ad447.csv", "SUMMARY_aiming60_7eec53.csv", 
                                         "SUMMARY_aiming60_13d986.csv", "SUMMARY_aiming60_33e532.csv", 
                                         "SUMMARY_aiming60_86f3b3.csv", "SUMMARY_aiming60_98e5cb.csv", 
                                         "SUMMARY_aiming60_4093e8.csv", "SUMMARY_aiming60_a02c67.csv", 
                                         "SUMMARY_aiming60_a23b35.csv", "SUMMARY_aiming60_a23b35.csv"))
  
group2_files <- file.path(data_path, c("SUMMARY_aiming60_7cd1bd.csv", "SUMMARY_aiming60_3091de.csv", "SUMMARY_aiming60_654648.csv", 
                                       "SUMMARY_aiming60_f275ca.csv"))

  group1_data <- list()
  group2_data <- list()
  
  for (file in group1_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    df$cutrial_no <- as.integer(df$cutrial_no)
    aligned1 <- df[df$cutrial_no >= 1 & df$cutrial_no <= 88, c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group1_data[[length(group1_data) + 1]] <- aligned1
  }
  combined_g1_aligned60 <- do.call(rbind, group1_data)
  
  # Extract trials for Group 2 (Trial 113 to 232)
  for (file in group2_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    aligned2 <- df[df$cutrial_no %in% c(1:24, 41:56, 65:72, 81:88, 97:112), c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group2_data[[length(group2_data) + 1]] <- aligned2
  }
 
   combined_g2_aligned60 <- do.call(rbind, group2_data)
  
  return(list(group1 = group1_data, group2 = group2_data))
  #print(nrow(group1_data[[1]]))
  #print(nrow(group2_data[[1]]))
  
}

aligned60_data <- getAligned60()


  
  
#Extract the Rotated Phase
getRotated60 <- function () {
  
  data_path <- "data/Instructed_summary/aiming60/"
  
  group1_files <- file.path(data_path, c("SUMMARY_aiming60_1ad447.csv", "SUMMARY_aiming60_7eec53.csv", 
                                         "SUMMARY_aiming60_13d986.csv", "SUMMARY_aiming60_33e532.csv", 
                                         "SUMMARY_aiming60_86f3b3.csv", "SUMMARY_aiming60_98e5cb.csv", 
                                         "SUMMARY_aiming60_4093e8.csv", "SUMMARY_aiming60_a02c67.csv", 
                                         "SUMMARY_aiming60_a23b35.csv"))
  
  group2_files <- file.path(data_path, c("SUMMARY_aiming60_7cd1bd.csv", "SUMMARY_aiming60_3091de.csv", "SUMMARY_aiming60_654648.csv", 
                                         "SUMMARY_aiming60_f275ca.csv"))
  
  group1_rotated <- list()
  group2_rotated <- list()
  
  # Group 1 (trial 89 to 208)
  for (file in group1_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    rotated1 <- df[df$cutrial_no >= 89 & df$cutrial_no <= 208,c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group1_rotated[[length(group1_rotated) + 1]] <- rotated1
  }
  
  combined_g1_rotated60 <- do.call(rbind, group1_rotated)
  
  # Group 2 (trial 113 to 232)
  for (file in group2_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    rotated2 <- df[df$cutrial_no >= 113 & df$cutrial_no <= 232, c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group2_rotated[[length(group2_rotated) + 1]] <- rotated2
  }
  
  combined_g2_rotated60 <- do.call(rbind, group2_rotated)
  
  return(list(group1 = group1_rotated, group2 = group2_rotated))
  #print(nrow(rotated_data$group1[[1]]))
  #print(nrow(rotated_data$group2[[1]]))

}
rotated60_data <- getRotated60()


getAfter60 <- function() {
  
  data_path <- "data/Instructed_summary/aiming60/"
  
  group1_files <- file.path(data_path, c("SUMMARY_aiming60_1ad447.csv", "SUMMARY_aiming60_7eec53.csv", 
                                         "SUMMARY_aiming60_13d986.csv", "SUMMARY_aiming60_33e532.csv", 
                                         "SUMMARY_aiming60_86f3b3.csv", "SUMMARY_aiming60_98e5cb.csv", 
                                         "SUMMARY_aiming60_4093e8.csv", "SUMMARY_aiming60_a02c67.csv", 
                                         "SUMMARY_aiming60_a23b35.csv"))
  
  group2_files <- file.path(data_path, c("SUMMARY_aiming60_7cd1bd.csv", "SUMMARY_aiming60_3091de.csv", "SUMMARY_aiming60_654648.csv", 
                                         "SUMMARY_aiming60_f275ca.csv"))
  
  group1_after <- list()
  group2_after <- list()
  
  # Group 1 (trial 233 to 256) left hand trials
  for (file in group1_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    after1 <- df[df$cutrial_no >= 209 & df$cutrial_no <= 256,c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group1_after[[length(group1_after) + 1]] <- after1
  }
  
  combined_g1_after60 <- do.call(rbind, group1_after)
  
  # Group 2 (trial 113 to 232) aftereffect trials
  for (file in group2_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    after2 <- df[df$cutrial_no >= 233 & df$cutrial_no <= 256, c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group2_after[[length(group2_after) + 1]] <- after2
  }
  
  combined_g2_after60 <- do.call(rbind, group2_after)
  
  return(list(group1 = group1_after, group2 = group2_after))
    #print(nrow(after_data$group1[[1]]))
    #print(nrow(after_data$group2[[1]])) 
    
    
  }
after60_data <- getAfter60()



#lets take the average and do a line graph
#aim60_path <- 'data/Instructed_summary/aiming60'
#all_60data <- list()
#aim60files <- list.files(aim60_path, pattern = "*.csv", full.names = TRUE)

#all_reach_deviations <- list()


#for (i in 1:length(aim60files)) {
 # df <- read.csv(aim60files[i], stringsAsFactors = FALSE)
 # all_reach_deviations[[i]] <- df$reachdeviation_deg
#}

#combinedreachdev <- do.call(cbind,all_reach_deviations)

#average_reach_deviation <- apply(combinedreachdev, 1, mean, na.rm = TRUE)

#plot(average_reach_deviation, type = "l", col = "hotpink2", lwd = 2,   xlim = c(0, 256), ylim = c(-30,60),
   #  main = "Average Reach Deviation with a 60 Degree Perturbation", xlab = "Trial", ylab = "Reach Deviation (degrees)")
#smooth_line2 <- smooth.spline(average_reach_deviation)
#lines(smooth_line2, col = "black", lwd = 3, lty=3)

#abline(h=0, col="black", lwd = 1, lty=3)


#combine data so we can extract columns
df_g1_60 <- rbind(combined_g1_aligned60, combined_g1_rotated60, combined_g1_after60)
df_g2_60 <- rbind(combined_g2_aligned60, combined_g2_rotated60, combined_g2_after60)

df_60 <- rbind(df_g1_60,df_g2_60)



#Files with strategy:
group1_files <- file.path(data_path, c("SUMMARY_aiming60_7eec53.csv", 
                                         "SUMMARY_aiming60_13d986.csv", "SUMMARY_aiming60_33e532.csv",
                                         "SUMMARY_aiming60_4093e8.csv", "SUMMARY_aiming60_a23b35.csv"))

group2_files <- file.path(data_path, c("SUMMARY_aiming60_7cd1bd.csv", 
                                       "SUMMARY_aiming60_654648.csv", "SUMMARY_aiming60_f275ca.csv"))

