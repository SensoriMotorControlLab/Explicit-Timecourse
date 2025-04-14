#Aiming 40

# Create a function that includes all participants under the aiming 40 folder.
get40_Data <- function() {
  aim40_path <- 'data/Instructed_summary/aiming40'
  all_40data <- list()
  aim40files <- list.files(aim40_path, pattern = "*.csv", full.names = TRUE)
  
  for (file_path in aim40files) {
    df <- read.csv(file_path)
    all_40data[[length(all_40data) + 1]] <- df
  }
  
  return(all_40data)
}

aim40_data <- get40_Data()
print(length(aim40_data))

#quick visualization 
plot(df$reachdeviation_deg, main = "Reach Deviation Aiming 40 ", xlab = "Trial", ylab = "Reach Deviation (degrees)")


#REMOVE OUTLIERS 
df1 <- aim40_data[[1]]
df1$reachdeviation_deg[df1$reachdeviation_deg < 0] <- 0  
df1$reachdeviation_deg[df1$reachdeviation_deg > 85] <- 85  

plot(df1$reachdeviation_deg, type="l")


#apply to all participants

for (i in 1:length(aim40_data)) {
  df <- aim40_data[[i]]
  
  # Remove values outside the range 0 to 90 degrees
  df$reachdeviation_deg[df$reachdeviation_deg < -20] <--10
  df$reachdeviation_deg[df$reachdeviation_deg > 85] <- 85
  
}

df2 <- aim40_data[[1]]
df1$aimhdeviation_deg[df1$reachdeviation_deg < 0] <- 0  
df1$aimdeviation_deg[df1$reachdeviation_deg > 85] <- 85  

plot(df2$aimdeviation_deg, type="l")


#apply to all participants

for (i in 1:length(aim40_data)) {
  df <- aim40_data[[i]]
  
  # Remove values outside the range 0 to 90 degrees
  df$aimdeviation_deg[df$raimdeviation_deg < -20] <--10
  df$reachdeviation_deg[df$reachdeviation_deg > 85] <- 85
  
}

#COMFIRM LEARNERS---------
data_path <- "data/Instructed_summary/aiming40/"
group1_files <- file.path(data_path, c("SUMMARY_aiming40_4a6642.csv", "SUMMARY_aiming40_04f558.csv", 
                                       "SUMMARY_aiming40_6ebd75.csv", "SUMMARY_aiming40_15f2a1.csv", 
                                       "SUMMARY_aiming40_31b753.csv", "SUMMARY_aiming40_42f286.csv", 
                                       "SUMMARY_aiming40_43f26a.csv", "SUMMARY_aiming40_81d984.csv", 
                                       "SUMMARY_aiming40_aa25ec.csv",
                                       "SUMMARY_aiming40_bde44b.csv", "SUMMARY_aiming40_c6c949.csv"))
learners <- 0  


for (file in group1_files) {
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  rotated <- df[df$task_idx == 8, , drop = FALSE]
  close_to_40 <- sum(rotated$reachdeviation_deg >= 10 & rotated$reachdeviation_deg <=50, na.rm = TRUE)
  proportion_close_to_40 <- close_to_40 / nrow(rotated)
  
  if (proportion_close_to_40 >= 0.5) {
    learners <- learners + 1
  }
}

print(learners)
#there are 11 out of 11 learners that countered the pertubation with a changed reach deviation 

data_path <- "data/Instructed_summary/aiming40/"
group2_files <- file.path(data_path, c("SUMMARY_aiming40_83926f.csv",    "SUMMARY_aiming40_96634a.csv",  
                                       "SUMMARY_aiming40_f59399.csv" ))



#now for the group within the new paradigm, which had the perturbation introduced at a different trial
learners <- 0  
for (file in group2_files) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  rotated <- df[df$task_idx == 12, , drop = FALSE]
  close_to_40 <- sum(rotated$reachdeviation_deg >= 20 & rotated$reachdeviation_deg <= 50, na.rm = TRUE)
  proportion_close_to_40 <- close_to_40 / nrow(rotated)
  
  if (proportion_close_to_40 >= 0.5) {
    learners <- learners + 1
  }
  
  
}

print(learners)
#there are 3/3 learners


#Extract the Aligned Phase 

getAligned40 <- function () {
  data_path <- "data/Instructed_summary/aiming40/"
  
  # Group 1 file paths
  group1_files <- file.path(data_path, c("SUMMARY_aiming40_4a6642.csv", "SUMMARY_aiming40_04f558.csv", 
                                         "SUMMARY_aiming40_6ebd75.csv", "SUMMARY_aiming40_15f2a1.csv", 
                                         "SUMMARY_aiming40_31b753.csv", "SUMMARY_aiming40_42f286.csv", 
                                         "SUMMARY_aiming40_43f26a.csv", "SUMMARY_aiming40_81d984.csv", 
                                         "SUMMARY_aiming40_aa25ec.csv",
                                         "SUMMARY_aiming40_bde44b.csv", "SUMMARY_aiming40_c6c949.csv"))
  
  group2_files <- file.path(data_path, c("SUMMARY_aiming40_83926f.csv",    "SUMMARY_aiming40_96634a.csv",  
                                         "SUMMARY_aiming40_f59399.csv" ))
  
  group1_data <- list()
  group2_data <- list()
  
  for (file in group1_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    df$cutrial_no <- as.integer(df$cutrial_no)
    aligned1 <- df[df$cutrial_no >= 1 & df$cutrial_no <= 88, c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    #print(nrow(aligned)) 
    
    group1_data[[length(group1_data) + 1]] <- aligned1
  }
  
  combined_g1_aligned40 <- do.call(rbind, group1_data)
  
  # Extract trials for Group 2 (Trial 113 to 232)
  for (file in group2_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    aligned2 <- df[df$cutrial_no %in% c(1:24, 41:56, 65:72, 81:88, 97:104), c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group2_data[[length(group2_data) + 1]] <- aligned2
  }
  
  combined_g2_aligned40 <- do.call(rbind, group2_data)
  
  return(list(group1 = group1_data, group2 = group2_data))
  #print(nrow(group1_data[[1]]))
  #print(nrow(group2_data[[1]]))
  
}

aligned40_data <- getAligned40()




#Extract the Rotated Phase
getRotated40 <- function () {
  
  data_path <- "data/Instructed_summary/aiming40/"
  
  group1_files <- file.path(data_path, c("SUMMARY_aiming40_4a6642.csv", "SUMMARY_aiming40_04f558.csv", 
                                         "SUMMARY_aiming40_6ebd75.csv", "SUMMARY_aiming40_15f2a1.csv", 
                                         "SUMMARY_aiming40_31b753.csv", "SUMMARY_aiming40_42f286.csv", 
                                         "SUMMARY_aiming40_43f26a.csv", "SUMMARY_aiming40_81d984.csv", 
                                         "SUMMARY_aiming40_aa25ec.csv",
                                         "SUMMARY_aiming40_bde44b.csv", "SUMMARY_aiming40_c6c949.csv"))
  
  group2_files <- file.path(data_path, c("SUMMARY_aiming40_83926f.csv",    "SUMMARY_aiming40_96634a.csv",  
                                         "SUMMARY_aiming40_f59399.csv" ))
  
  group1_rotated <- list()
  group2_rotated <- list()
  
  # Group 1 (trial 89 to 208)
  for (file in group1_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    rotated1 <- df[df$cutrial_no >= 89 & df$cutrial_no <= 208,c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group1_rotated[[length(group1_rotated) + 1]] <- rotated1
  }
  
  combined_g1_rotated40 <- do.call(rbind, group1_rotated)
  
  # Group 2 (trial 113 to 232)
  for (file in group2_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    rotated2 <- df[df$cutrial_no >= 113 & df$cutrial_no <= 232, c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group2_rotated[[length(group2_rotated) + 1]] <- rotated2
  }
  
  combined_g2_rotated40 <- do.call(rbind, group2_rotated)
  
  return(list(group1 = group1_rotated, group2 = group2_rotated))
  #print(nrow(rotated_data$group1[[1]]))
  #print(nrow(rotated_data$group2[[1]]))
  
}
rotated40_data <- getRotated40()


getAfter40 <- function() {
  
  data_path <- "data/Instructed_summary/aiming40/"
  
  group1_files <- file.path(data_path, c("SUMMARY_aiming40_4a6642.csv", 
                                         "SUMMARY_aiming40_6ebd75.csv", 
                                         "SUMMARY_aiming40_31b753.csv",  
                                         "SUMMARY_aiming40_81d984.csv", 
                                         "SUMMARY_aiming40_bde44b.csv"))
  
  group2_files <- file.path(data_path, c("SUMMARY_aiming40_96634a.csv"))
  
  group1_after <- list()
  group2_after <- list()
  
  # Group 1 (trial 233 to 256) left hand trials
  for (file in group1_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    after <- df[df$cutrial_no >= 209 & df$cutrial_no <= 256,c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group1_after[[length(group1_after) + 1]] <- after
  }
  
  # Group 2 (trial 113 to 232) aftereffect trials
  for (file in group2_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    after <- df[df$cutrial_no >= 233 & df$cutrial_no <= 256, c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group2_after[[length(group2_after) + 1]] <- after
  }
  return(list(group1 = group1_after, group2 = group2_after))
  #print(nrow(after_data$group1[[1]]))
  #print(nrow(after_data$group2[[1]])) 
  
  
}
after40_data <- getAfter40()
