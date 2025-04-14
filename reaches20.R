#Aiming 20

# Create a function that includes all participants under the aiming 20 folder.
get20_Data <- function() {
  aim20_path <- 'data/Instructed_summary/aiming20'
  all_20data <- list()
  aim20files <- list.files(aim20_path, pattern = "*.csv", full.names = TRUE)
  
  for (file_path in aim20files) {
    df <- read.csv(file_path)
    all_20data[[length(all_20data) + 1]] <- df
  }
  
  return(all_20data)
}

aim20_data <- get20_Data()
print(length(aim20_data))

#quick visualization 
plot(df$reachdeviation_deg, main = "Reach Deviation Aiming 20 ", xlab = "Trial", ylab = "Reach Deviation (degrees)")


#REMOVE OUTLIERS 
df1 <- aim20_data[[1]]
df1$reachdeviation_deg[df1$reachdeviation_deg < -5] <- -5  
df1$reachdeviation_deg[df1$reachdeviation_deg > 60] <- 60  

plot(df1$reachdeviation_deg, type="l")


#apply to all participants

for (i in 1:length(aim20_data)) {
  df <- aim20_data[[i]]
  
  # Remove values outside the range 0 to 90 degrees
  df$reachdeviation_deg[df$reachdeviation_deg < -20] <--5
  df$reachdeviation_deg[df$reachdeviation_deg > 60] <- 60
  
}

df2 <- aim20_data[[1]]
df1$aimhdeviation_deg[df1$reachdeviation_deg < 0] <- 0  
df1$aimdeviation_deg[df1$reachdeviation_deg > 85] <- 85  

plot(df2$aimdeviation_deg, type="l")


#apply to all participants

for (i in 1:length(aim20_data)) {
  df <- aim20_data[[i]]
  
  # Remove values outside the range 0 to 90 degrees
  df$aimdeviation_deg[df$raimdeviation_deg < -20] <--10
  df$reachdeviation_deg[df$reachdeviation_deg > 85] <- 85
  
}

#COMFIRM LEARNERS---------

data_path <- "data/Instructed_summary/aiming20/"
group2_files <- file.path(data_path, c("SUMMARY_aiming20_0bac45.csv", "SUMMARY_aiming20_d45fca.csv", "SUMMARY_aiming20_ee29d6.csv", 
                                       "SUMMARY_aiming20_4fb1e1.csv"))



#now for the group within the new paradigm, which had the perturbation introduced at a different trial
learners <- 0  
for (file in group2_files) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  rotated <- df[df$task_idx == 12, , drop = FALSE]
  close_to_20 <- sum(rotated$reachdeviation_deg >= 15 & rotated$reachdeviation_deg <= 60, na.rm = TRUE)
  proportion_close_to_20 <- close_to_20 / nrow(rotated)
  
  if (proportion_close_to_20 >= 0.5) {
    learners <- learners + 1
  }
  
  
}

print(learners)
#there are 4/4 learners


#Extract the Aligned Phase 

getAligned20 <- function () {
  data_path <- "data/Instructed_summary/aiming20/"
  
  # Group 1 file paths
  group2_files <- file.path(data_path, c("SUMMARY_aiming20_0bac45.csv", "SUMMARY_aiming20_d45fca.csv", "SUMMARY_aiming20_ee29d6.csv", 
                                         "SUMMARY_aiming20_4fb1e1.csv"))
  
  group2_data <- list()
  
  
  # Extract trials for Group 2 (Trial 113 to 232)
  for (file in group2_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    aligned2 <- df[df$cutrial_no %in% c(1:24, 41:56, 65:72, 81:88, 97:112), c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group2_data[[length(group2_data) + 1]] <- aligned2
  }
  
  combined_g2_aligned20 <- do.call(rbind, group2_data)
  
  return(list( group2 = group2_data))
  #print(nrow(group1_data[[1]]))
  #print(nrow(group2_data[[1]]))
  
}

aligned20_data <- getAligned20()




#Extract the Rotated Phase
getRotated20 <- function () {
  
  data_path <- "data/Instructed_summary/aiming20/"
  
  group2_files <- file.path(data_path, c("SUMMARY_aiming20_0bac45.csv", "SUMMARY_aiming20_d45fca.csv", "SUMMARY_aiming20_ee29d6.csv", 
                                         "SUMMARY_aiming20_4fb1e1.csv"))
  
  
  group2_rotated <- list()
  
  
  for (file in group2_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    rotated2 <- df[df$cutrial_no >= 113 & df$cutrial_no <= 232, c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group2_rotated[[length(group2_rotated) + 1]] <- rotated2
  }
  
  combined_g2_rotated20 <- do.call(rbind, group2_rotated)
  
  return(list(group1 = group1_rotated, group2 = group2_rotated))
  #print(nrow(rotated_data$group1[[1]]))
  #print(nrow(rotated_data$group2[[1]]))
  
}
rotated20_data <- getRotated20()


getAfter20 <- function() {
  
  data_path <- "data/Instructed_summary/aiming20/"
  
  group2_files <- file.path(data_path, c("SUMMARY_aiming20_0bac45.csv", "SUMMARY_aiming20_d45fca.csv", "SUMMARY_aiming20_ee29d6.csv", 
                                         "SUMMARY_aiming20_4fb1e1.csv"))
  
  
  group2_after <- list()
  
  for (file in group2_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    after <- df[df$cutrial_no >= 233 & df$cutrial_no <= 256, c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group2_after[[length(group2_after) + 1]] <- after
  }
  return(list(group1 = group1_after, group2 = group2_after))
  #print(nrow(after_data$group1[[1]]))
  #print(nrow(after_data$group2[[1]])) 
  
  
}
after20_data <- getAfter20()