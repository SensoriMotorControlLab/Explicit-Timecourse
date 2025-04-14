identifyCorrectAimChangeWithFiles <- function() {
  rotated_data <- getRotated()  # Get the rotated data
  
  # List of all files for reference
  group1_files <- c("SUMMARY_aiming50_4eeaee.csv", "SUMMARY_aiming50_5f4177.csv", 
                    "SUMMARY_aiming50_051bcc.csv",  
                    "SUMMARY_aiming50_54044d.csv", "SUMMARY_aiming50_94709f.csv", 
                    "SUMMARY_aiming50_901482.csv", "SUMMARY_aiming50_c363b6.csv", 
                    "SUMMARY_aiming50_d9ff04.csv"))

  group2_files <- c("SUMMARY_aiming50_0f6fbf.csv", "SUMMARY_aiming50_194dab.csv", 
                    "SUMMARY_aiming50_b13e41.csv", "SUMMARY_aiming50_e066de.csv"))

  # Function to check if aim deviation is in 40-80 range for at least 40% of trials
  hasConsistentAimChange <- function(df) {
    valid_trials <- df$aimdeviation_deg >= 20 & df$aimdeviation_deg <= 80  # Check which trials meet the range
    ratio_valid <- sum(valid_trials) / length(valid_trials)  # Calculate percentage of valid trials
    
    return(ratio_valid >= 0.50)  # Return TRUE if at least 40% of trials are within the range
  }
  
  # Group 1: Identify correct aim change (from 40 to 80 degrees) in at least 40% of trials
  group1_correct_change <- mapply(function(df, file_name) {
    if (hasConsistentAimChange(df)) return(file_name) else return(NULL)
  }, rotated_data$group1, group1_files)
  
  # Group 2: Identify correct aim change (from 40 to 80 degrees) in at least 40% of trials
  group2_correct_change <- mapply(function(df, file_name) {
    if (hasConsistentAimChange(df)) return(file_name) else return(NULL)
  }, rotated_data$group2, group2_files)
  
  # Combine the results from both groups
  all_correct_changes <- c(group1_correct_change, group2_correct_change)
  
  # Filter out NULL values (participants who didn't meet the 40% requirement)
  valid_files <- all_correct_changes[!sapply(all_correct_changes, is.null)]
  
  # Print out the corresponding CSV files for participants with valid aim changes
  print(valid_files)
  
} 

#THOSE WITH STRATEGY
[1] "data/Instructed_summary/aiming50//SUMMARY_aiming50_54044d.csv"
[2] "data/Instructed_summary/aiming50//SUMMARY_aiming50_94709f.csv"
[3] "data/Instructed_summary/aiming50//SUMMARY_aiming50_901482.csv"
[4] "data/Instructed_summary/aiming50//SUMMARY_aiming50_d9ff04.csv"
[5] "data/Instructed_summary/aiming50//SUMMARY_aiming50_0f6fbf.csv"
[6] "data/Instructed_summary/aiming50//SUMMARY_aiming50_194dab.csv"
[7] "data/Instructed_summary/aiming50//SUMMARY_aiming50_e066de.csv"