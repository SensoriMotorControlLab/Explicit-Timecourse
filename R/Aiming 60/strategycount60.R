identifyCorrectAimChangeWithFiles <- function() {
  rotated_data <- getRotated()  # Get the rotated data
  
  # List of all files for reference
  group1_files <- c("SUMMARY_aiming60_1ad447.csv", "SUMMARY_aiming60_7eec53.csv", 
                    "SUMMARY_aiming60_13d986.csv", "SUMMARY_aiming60_33e532.csv", 
                    "SUMMARY_aiming60_86f3b3.csv", "SUMMARY_aiming60_98e5cb.csv", 
                    "SUMMARY_aiming60_4093e8.csv", "SUMMARY_aiming60_a02c67.csv", 
                    "SUMMARY_aiming60_a23b35.csv")
  
  group2_files <- c("SUMMARY_aiming60_7cd1bd.csv", "SUMMARY_aiming60_3091de.csv", 
                    "SUMMARY_aiming60_654648.csv", "SUMMARY_aiming60_f275ca.csv")
  
  # Group 1: Identify correct aim change (from 40 to 80 degrees) across all trials
  group1_correct_change <- mapply(function(df, file_name) {
    change_in_aim <- abs(df$aimdeviation_deg - df$aimdeviation_deg[1])  # Change from the initial trial
    valid_changes <- change_in_aim >= 40 & change_in_aim <= 80  # Check if the change is within the range
    
    # Return file name if there is a valid change in aim deviation, otherwise NULL
    if(any(valid_changes)) return(file_name) else return(NULL)
  }, rotated_data$group1, group1_files)
  
  # Group 2: Identify correct aim change (from 40 to 80 degrees) across all trials
  group2_correct_change <- mapply(function(df, file_name) {
    change_in_aim <- abs(df$aimdeviation_deg - df$aimdeviation_deg[1])
    valid_changes <- change_in_aim >= 40 & change_in_aim <= 80
    
    if(any(valid_changes)) return(file_name) else return(NULL)
  }, rotated_data$group2, group2_files)
  
  # Combine the results from both groups
  all_correct_changes <- c(group1_correct_change, group2_correct_change)
  
  # Filter out NULL values (indicating no valid changes in aim)
  valid_files <- all_correct_changes[!sapply(all_correct_changes, is.null)]
  
  # Print out the corresponding CSV files for participants with valid aim changes
  print(valid_files)
  
  return(valid_files)
}

# Call the function to identify participants with a proper change in aim deviation and print out the files
correct_change_rotated_with_files <- identifyCorrectAimChangeWithFiles()



identifyCorrectAimChangeWithFiles <- function() {
  rotated_data <- getRotated()  # Get the rotated data
  
  # List of all files for reference
  group1_files <- c("SUMMARY_aiming60_1ad447.csv", "SUMMARY_aiming60_7eec53.csv", 
                    "SUMMARY_aiming60_13d986.csv", "SUMMARY_aiming60_33e532.csv", 
                    "SUMMARY_aiming60_86f3b3.csv", "SUMMARY_aiming60_98e5cb.csv", 
                    "SUMMARY_aiming60_4093e8.csv", "SUMMARY_aiming60_a02c67.csv", 
                    "SUMMARY_aiming60_a23b35.csv")
  
  group2_files <- c("SUMMARY_aiming60_7cd1bd.csv", "SUMMARY_aiming60_3091de.csv", 
                    "SUMMARY_aiming60_654648.csv", "SUMMARY_aiming60_f275ca.csv")
  
  # Function to check if aim deviation is in 40-80 range for at least 40% of trials
  hasConsistentAimChange <- function(df) {
    valid_trials <- df$aimdeviation_deg >= 30 & df$aimdeviation_deg <= 100  # Check which trials meet the range
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

bad = 86f, 1ad, 98e, 309, a02 


#8/13 had aiming strategies within 30 and 100 degrees 50% of the time during rotation trials.




