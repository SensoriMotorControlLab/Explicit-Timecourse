# Create a function that includes all participants under the aiming 50 folder.
get50_Data <- function() {
  aim50_path <- 'data/Instructed_summary/aiming50'
  all_50data <- list()
  aim50files <- list.files(aim50_path, pattern = "*.csv", full.names = TRUE)
  
  for (file_path in aim50files) {
    df <- read.csv(file_path)
    all_50data[[length(all_50data) + 1]] <- df
  }
  
  return(all_50data)
}

aim50_data <- get50_Data()
print(length(aim50_data))

#quick visualization 
plot(df$reachdeviation_deg, main = "Reach Deviation Aiming 50 ", xlab = "Trial", ylab = "Reach Deviation (degrees)")


#REMOVE OUTLIERS 
df1 <- aim50_data[[1]]
df1$reachdeviation_deg[df1$reachdeviation_deg < 0] <- 0  
df1$reachdeviation_deg[df1$reachdeviation_deg > 85] <- 85  

plot(df1$reachdeviation_deg, type="l")


#apply to all participants

for (i in 1:length(aim50_data)) {
  df <- aim50_data[[i]]
  
  # Remove values outside the range 0 to 85 degrees
  df$reachdeviation_deg[df$reachdeviation_deg < -20] <--20
  df$reachdeviation_deg[df$reachdeviation_deg > 85] <- 85
  
}

df2 <- aim50_data[[2]]
df1$aimdeviation_deg[df1$reachdeviation_deg < 0] <- -10  
df1$aimdeviation_deg[df1$reachdeviation_deg > 85] <- 85  

plot(df2$cutrial_no, df2$aimdeviation_deg,  type="l")
  

#COMFIRM LEARNERS---------
data_path <- "data/Instructed_summary/aiming50/"
group1_files <- file.path(data_path, c("SUMMARY_aiming50_4eeaee.csv", "SUMMARY_aiming50_5f4177.csv", 
                  "SUMMARY_aiming50_051bcc.csv", "SUMMARY_aiming50_3824c0.csv", 
                  "SUMMARY_aiming50_54044d.csv", "SUMMARY_aiming50_94709f.csv", 
                  "SUMMARY_aiming50_901482.csv", "SUMMARY_aiming50_c363b6.csv", 
                  "SUMMARY_aiming50_d9ff04.csv","SUMMARY_aiming50_fb333f.csv" ))

non_learner_files <- c()
learners <- 0

for (file in group1_files) {
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  rotated <- df[df$task_idx == 8, , drop = FALSE]
  if (nrow(rotated) > 0) {  
    close_to_50 <- sum(rotated$reachdeviation_deg >= 25 & rotated$reachdeviation_deg <= 70, na.rm = TRUE)
    proportion_close_to_50 <- close_to_50 / nrow(rotated)
    
    if (proportion_close_to_50 >= 0.5) {
      learners <- learners + 1
    } else {
      non_learner_files <- c(non_learner_files, file)  
    }
  } else {
    non_learner_files <- c(non_learner_files, file)  
  }
  }

cat("\nFiles that are NOT learners:\n")
print(non_learner_files)
cat("\nTotal Learners:", learners, "out of", length(group1_files), "\n")

#there are 8 out of 10 learners that countered the pertubation with a reach deviation ranging from 25 to 85 degrees
#[1] "data/Instructed_summary/aiming50//SUMMARY_aiming50_3824c0.csv"
#[2] "data/Instructed_summary/aiming50//SUMMARY_aiming50_fb333f.csv"


data_path <- "data/Instructed_summary/aiming50/"
group2_files <- file.path(data_path, c("SUMMARY_aiming50_0f6fbf.csv", "SUMMARY_aiming50_194dab.csv", 
                                                        "SUMMARY_aiming50_b13e41.csv", "SUMMARY_aiming50_e066de.csv"))
                          



#now for the group within the new paradigm, which had the perturbation introduced at a different trial
learners <- 0  
for (file in group2_files) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  rotated <- df[df$task_idx == 12, , drop = FALSE]
  close_to_50 <- sum(rotated$reachdeviation_deg >= 25 & rotated$reachdeviation_deg <= 70, na.rm = TRUE)
  proportion_close_to_50 <- close_to_50 / nrow(rotated)
  
  if (proportion_close_to_50 >= 0.5) {
    learners <- learners + 1
  }
  
  
}

print(learners)
#there are 4/4 learners


#Extract the Aligned Phase 

getAligned50 <- function () {
  data_path <- "data/Instructed_summary/aiming50/"
  
  # Group 1 file paths
  group1_files <- file.path(data_path, c("SUMMARY_aiming50_4eeaee.csv", "SUMMARY_aiming50_5f4177.csv", 
                                         "SUMMARY_aiming50_051bcc.csv", "SUMMARY_aiming50_3824c0.csv", 
                                         "SUMMARY_aiming50_54044d.csv", "SUMMARY_aiming50_94709f.csv", 
                                         "SUMMARY_aiming50_901482.csv", "SUMMARY_aiming50_c363b6.csv", 
                                         "SUMMARY_aiming50_d9ff04.csv","SUMMARY_aiming50_fb333f.csv"))
  
  group2_files <- file.path(data_path, c("SUMMARY_aiming50_0f6fbf.csv", "SUMMARY_aiming50_194dab.csv", 
                                                        "SUMMARY_aiming50_b13e41.csv", "SUMMARY_aiming50_e066de.csv"))
  
  
  group1_data <- list()
  group2_data <- list()
  
  for (file in group1_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    df$cutrial_no <- as.integer(df$cutrial_no)
    aligned1 <- df[df$cutrial_no >= 1 & df$cutrial_no <= 88, c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    #print(nrow(aligned)) 
    
    group1_data[[length(group1_data) + 1]] <- aligned1
  }
  
  combined_g1_aligned50 <- do.call(rbind, group1_data)
  
  # Extract trials for Group 2 (Trial 113 to 232)
  for (file in group2_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    aligned2 <- df[df$cutrial_no %in% c(1:24, 41:56, 65:72, 81:88, 97:104), c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group2_data[[length(group2_data) + 1]] <- aligned2
  }
  combined_g2_aligned50 <- do.call(rbind, group2_data)
  
  return(list(group1 = group1_data, group2 = group2_data))
  #print(nrow(group1_data[[1]]))
  #print(nrow(group2_data[[1]]))
  
}


aligned50_data <- getAligned50()




#Extract the Rotated Phase
getRotated50 <- function () {
  
  data_path <- "data/Instructed_summary/aiming50/"
  
  group1_files <- file.path(data_path, c("SUMMARY_aiming50_4eeaee.csv", "SUMMARY_aiming50_5f4177.csv", 
                                         "SUMMARY_aiming50_051bcc.csv", "SUMMARY_aiming50_3824c0.csv", 
                                         "SUMMARY_aiming50_54044d.csv", "SUMMARY_aiming50_94709f.csv", 
                                         "SUMMARY_aiming50_901482.csv", "SUMMARY_aiming50_c363b6.csv", 
                                         "SUMMARY_aiming50_d9ff04.csv","SUMMARY_aiming50_fb333f.csv"))
  
  group2_files <- file.path(data_path, c("SUMMARY_aiming50_0f6fbf.csv", "SUMMARY_aiming50_194dab.csv", 
                                         "SUMMARY_aiming50_b13e41.csv", "SUMMARY_aiming50_e066de.csv"))
  
  group1_rotated <- list()
  group2_rotated <- list()
  
  # Group 1 (trial 89 to 208)
  for (file in group1_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    rotated1 <- df[df$cutrial_no >= 89 & df$cutrial_no <= 208,c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group1_rotated[[length(group1_rotated) + 1]] <- rotated1
  }
  combined_g1_rotated50 <- do.call(rbind, group1_rotated)
  
  # Group 2 (trial 113 to 232)
  for (file in group2_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    rotated2 <- df[df$cutrial_no >= 113 & df$cutrial_no <= 232, c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group2_rotated[[length(group2_rotated) + 1]] <- rotated2
  }
 
  #combine all participant trials
   combined_g2_rotated50 <- do.call(rbind, group2_rotated)
  
  return(list(group1 = group1_rotated, group2 = group2_rotated))
  #print(nrow(rotated_data$group1[[1]]))
  #print(nrow(rotated_data$group2[[1]]))
  
}
rotated50_data <- getRotated50()


getAfter50 <- function() {
  
  data_path <- "data/Instructed_summary/aiming50/"
  
  group1_files <- file.path(data_path, c( "SUMMARY_aiming50_54044d.csv", "SUMMARY_aiming50_94709f.csv", 
                                          "SUMMARY_aiming50_901482.csv",
                                          "SUMMARY_aiming50_d9ff04.csv"))
  
  group2_files <- file.path(data_path, c("SUMMARY_aiming50_0f6fbf.csv", "SUMMARY_aiming50_194dab.csv", 
                                         "SUMMARY_aiming50_e066de.csv"))
  

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
after50_data <- getAfter50()