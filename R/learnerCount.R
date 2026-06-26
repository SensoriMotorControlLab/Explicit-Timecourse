library(readr)
library(dplyr)

load_total_group_data <- function(dir = "data/Group_two_summary/") {
  
  rotations <- c(20, 30, 40, 50, 60)
  all_data <- list()
  
  for (rot in rotations) {
    folder_path <- file.path(dir, paste0("aiming", rot))
    csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
    
    for (file in csv_files) {
      df <- read_csv(file, show_col_types = FALSE)
      
      # Check required columns
      if (!all(c("task_idx", "cutrial_no") %in% names(df))) {
        warning(paste("Missing task_idx or cutrial_no in file:", file))
        next
      }
      
      # Extract participant ID
      participant_id <- gsub(paste0("SUMMARY_aiming", rot, "_(.*)\\.csv"), "\\1", basename(file))
      
      # Assign group based on max task_idx
      max_idx <- max(df$task_idx, na.rm = TRUE)
      group <- if (max_idx <= 10) {
        "Group 1"
      } else if (max_idx <= 13) {
        "Group 2"
      } else {
        "Unknown"
      }
      
      # Add participant info
      df <- df %>%
        mutate(participant_id = participant_id,
               group = group,
               rotation = rot)
      
      # Assign trial_type safely
      df <- df %>%
        mutate(trial_type = case_when(
          # Group 1
          # group == "Group 1" & cutrial_no >= 1   & cutrial_no <= 24   ~ "aligned",
          # group == "Group 1" & cutrial_no >= 25  & cutrial_no <= 40   ~ "zeroclamp",
          # group == "Group 1" & cutrial_no >= 41  & cutrial_no <= 48   ~ "aligned",
          # group == "Group 1" & cutrial_no >= 49  & cutrial_no <= 65   ~ "lefthand",
          # group == "Group 1" & cutrial_no >= 66  & cutrial_no <= 80   ~ "aligned",
          # group == "Group 1" & cutrial_no >= 81  & cutrial_no <= 88   ~ "zeroclamp",
          # group == "Group 1" & cutrial_no >= 89  & cutrial_no <= 208  ~ "rotated",
          # group == "Group 1" & cutrial_no >= 209 & cutrial_no <= 232  ~ "zeroclamp_rotated",
          # group == "Group 1" & cutrial_no >= 233 & cutrial_no <= 256  ~ "lefthand_rotated",
          # Group 2
          group == "Group 2" & cutrial_no >= 1   & cutrial_no <= 24   ~ "aligned",
          group == "Group 2" & cutrial_no >= 25  & cutrial_no <= 40   ~ "nocursor",
          group == "Group 2" & cutrial_no >= 41  & cutrial_no <= 56   ~ "aligned",
          group == "Group 2" & cutrial_no >= 57  & cutrial_no <= 64   ~ "errorclamp",
          group == "Group 2" & cutrial_no >= 65  & cutrial_no <= 72   ~ "aligned",
          group == "Group 2" & cutrial_no >= 73  & cutrial_no <= 80   ~ "nocursor",
          group == "Group 2" & cutrial_no >= 81  & cutrial_no <= 88   ~ "aligned",
          group == "Group 2" & cutrial_no >= 89  & cutrial_no <= 96   ~ "nocursor",
          group == "Group 2" & cutrial_no >= 97  & cutrial_no <= 112  ~ "aligned",
          group == "Group 2" & cutrial_no >= 113 & cutrial_no <= 232  ~ "rotated",
          group == "Group 2" & cutrial_no >= 233 & cutrial_no <= 256  ~ "nocursor",
          TRUE ~ NA_character_
        ))
      
      all_data[[length(all_data) + 1]] <- df
    }
  }
  
  total_group_data <- bind_rows(all_data)
  
  # Save to CSV
  write_csv(total_group_data, file.path(dir, "total_group_data.csv"))
  
  return(total_group_data)
}


####LEARNERS
getLearners <- function() {
  total_group_data <- load_total_group_data("data/Group_two_summary/")
 # total_group_data <- total_group_data %>%
   # filter(group != "Group 1")
  
  total_group_data %>%
    group_by(participant_id) %>%
    summarise(n_rot = n_distinct(rotation)) %>%
    filter(n_rot > 1)
  
learner_df <- total_group_data %>%
  group_by(participant_id, group) %>%
  group_modify(~{

    df <- .x
    group_name <- .y$group

    # ----- DEFINE TASK STRUCTURE -----
    if(group_name == "Group 1"){
      rot_task <- 8
      baseline_trials <- df %>%
        filter(task_idx == 7)   # final baseline before rotation
    } else {
      rot_task <- 12
      baseline_trials <- df %>%
        filter(
          (task_idx == 2 & trial_idx > 16) |
          (task_idx == 4 & trial_idx > 4) |
          task_idx %in% c(6, 8, 10)
        )
    }


    baseline <- median(baseline_trials$reachdeviation_deg, na.rm = TRUE)

    rotated <- df %>%
      filter(task_idx == rot_task, trial_idx > 104)

    rotated_median <- median(rotated$reachdeviation_deg, na.rm = TRUE)
    rotation <- unique(rotated$rotation_deg)[1]

    # ----- BASELINE CORRECTION -----
    meandev <- rotated_median - baseline

    # ----- DIRECTION NORMALIZE -----
    meandev <- -sign(rotation) * meandev


    is_learner <- meandev > (abs(rotation) / 2)

    tibble(
      rotation = rotation,
      baseline = baseline,
      rotated_median = rotated_median,
      adjusted_dev = meandev,
      is_learner = is_learner
    )
  }) %>%
  ungroup()

  

  learner_summary <- learner_df %>%
    group_by(rotation) %>%
    summarise(
      total_n = n(),
      n_learners = sum(is_learner),
      percent_learners = 100 * n_learners / total_n
    )
  
  
  print(learner_summary)
  return(learner_df)
  
}

LearnerCSV <- function() {
  total_group_data <- load_total_group_data("data/Group_two_summary/")
 
   learner_id <- getLearners() %>%
    distinct(participant_id, rotation, group, .keep_all = TRUE)
  learner_id <- learner_id %>%
    mutate(rotation = abs(rotation))
  
  total_learners_data <- total_group_data %>%
    inner_join(
      learner_id %>% filter(is_learner == TRUE),
      by = c("participant_id", "rotation", "group")
    )
  
  write.csv(total_learners_data, "data/total_learners_data.csv", row.names = FALSE)
}

nonCSV <- function () {
  total_group_data <- load_total_group_data("data/Group_two_summary/")
  
  learner_data <- getLearners()
  
  # Step 1: get non-learner IDs
  nonlearners_ids <- learner_data %>%
    filter(is_learner == FALSE) %>%
    select(participant_id)
  
  # Step 2: filter trial-level data
  nonlearners_data <- total_group_data %>%
    semi_join(nonlearners_ids, by = "participant_id") %>%
    select(participant_id, trial_type, reachdeviation_deg, rotation, aimdeviation_deg,cutrial_no)
  
  write.csv(nonlearners_data, "data/nonlearners_data.csv", row.names = FALSE)
}
#learner_id <- getLearners(total_group_data)
#total_learners_data <- LearnerCSV(total_group_data)




