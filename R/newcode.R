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

####STRATEGY

#use last 16 rotated trials which represents where strategies are most consistent

getCI <- function () {
  
  total_learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors =FALSE)
  
  last_16_rotated_learners <- total_learners_data[
    (total_learners_data$cutrial_no %in% 193:208 & total_learners_data$group == 'Group 1') |
      (total_learners_data$cutrial_no %in% 217:232 & total_learners_data$group == 'Group 2'),
  ]
  
  CI <- aggregate(
    aimdeviation_deg ~ participant_id + rotation,
    data = last_16_rotated_learners,
    FUN = function(x) 
      Reach::getConfidenceInterval(x)
  )
  return(list(CI = CI, data = last_16_rotated_learners))
}

#using CI function, we can now use a 95% interval approach to figure out strategy-users
getStrategies <- function() {
  
  ci_result <- getCI()
  
  CI_df <- ci_result$CI
  raw_df <- ci_result$data
  
  # 1. compute sign flip metric from RAW data
  sign_df <- raw_df %>%
    group_by(participant_id, rotation) %>%
    summarise(
      neg_prop = mean(aimdeviation_deg < 0),
      sign_flips = sum(diff(sign(aimdeviation_deg)) != 0),
      .groups = "drop"
    )
  
  # 2.flag unstable participants (NO strategy automatically)
  sign_df <- sign_df %>%
    mutate(
      flip_flag = sign_flips > 2   
    )
  
  # 3. apply CI only to stable participants
  strategy_df <- CI_df %>%
    rowwise() %>%
    mutate(
      lower = aimdeviation_deg[1],
      upper = aimdeviation_deg[2]
    ) %>%
    ungroup() %>%
    left_join(sign_df, by = c("participant_id", "rotation")) %>%
    mutate(
      strategy = case_when(
        flip_flag ~ "No",         
        lower < 0 ~ "No",
        lower > 5 ~ "Yes",
        TRUE ~ "No"
      )
    ) %>%
    dplyr::select(participant_id, rotation, lower, upper, neg_prop, sign_flips, strategy)
  
  return(strategy_df)
}


countStrategies <- function() {
  strategy_df <- getStrategies()
   total_learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors =FALSE)

   strategy_users <- sum(  strategy_df$strategy == "Yes", na.rm = TRUE)
  print(strategy_users)
}


strategySummary <- function() {
  
  
  # Get strategies
  strategy_df <- getStrategies()
  
  # Set strategy = "No" for the specified participants
 # strategy_df$strategy[strategy_df$participant_id %in% force_no_ids] <- "No"
  
  # Summarise
  strategy_df %>% 
    group_by(rotation) %>%
    summarise(
      total_n = n(),
      strategy_users = sum(strategy %in% c('Yes')),
      percent_strategy_users = round(100 * strategy_users / total_n, 1),
      .groups = "drop"
    )
}



#make new strategy file

Strategyfile <- function() {
  
  total_learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors = FALSE)
  
  strategy_df <- getStrategies()
  
  strategy_ids <- strategy_df$participant_id[
    strategy_df$strategy == "Yes"
  ]
  
  strategy_data <- total_learners_data %>%
    filter(participant_id %in% strategy_ids)
  
  total_learners_data <- total_learners_data %>%
    mutate(strategy = ifelse(participant_id %in% strategy_ids, 1, 0))
  
  write.csv(strategy_data, "data/strategy_only_participants.csv", row.names = FALSE)
  
  return(invisible(list(
    strategy_data = strategy_data,
    total_learners_data = total_learners_data
  )))
}
#add sanity check
getTargets <- function () {
  strategy_data <- read.csv("data/strategy_only_participants.csv")

target_check <- strategy_data %>%
  distinct(participant_id, aimdeviation_deg, targetangle_deg, cutrial_no, trial_type.x=="rotated", as.factor=TRUE)
#print(target_check)


#compare aim to previous trial target location

reldifference <- strategy_data %>%
  filter(trial_type.x == "rotated") %>%
  distinct(participant_id, cutrial_no, aim_deg, aimdeviation_deg, targetangle_deg) %>%
  arrange(participant_id, cutrial_no) %>%
  group_by(participant_id) %>%
  mutate(
    # aim deviation relative to current target
    aim_dev_current = aimdeviation_deg,
   
    prev_targetangle = lag(targetangle_deg),
  
    aim_dev_prev_target = aim_deg - prev_targetangle
  ) %>%
  ungroup()

participants <- unique(reldifference$participant_id)

for(pid in participants){
  df <- reldifference[reldifference$participant_id == pid, ]
  
  plot(df$cutrial_no, df$aim_dev_current, type="l", col="blue", ylim=range(c(df$aim_dev_current, df$aim_dev_prev_target), na.rm = TRUE),
       xlab="Trial", ylab="Aim Deviation (deg)", main=paste("Participant:", pid))
  lines(df$cutrial_no, df$aim_dev_prev_target, col="red")
  legend("topright", legend=c("Recorded Aim Dev", "Aim Dev from Prev Target"), col=c("blue","red"), lty=1)
  
  readline(prompt="Press [Enter] to view next participant...")
}



}



