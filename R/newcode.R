dir <- "data/Instructed_summary/"  
library(readr)
library(dplyr)
rotations <- c(20, 30, 40, 50, 60)

all_data <- list()
participant_counter <- 1

for (rot in rotations) {
  folder_path <- file.path(dir, paste0("aiming", rot))
  
  csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  for (file in csv_files) {
    df <- read_csv(file, show_col_types = FALSE)
    
    participant_id <- gsub(paste0("SUMMARY_aiming", rot, "_(.*)\\.csv"), "\\1", basename(file))
    
    
    # Determine group based on task_idx (if max task_idx ≤ 13 → group 2)
  
    max_idx <- max(df$task_idx, na.rm = TRUE)
    group <- if (max_idx <= 10) {
      "Group 1"
    } else if (max_idx <= 13) {
      "Group 2"
    } else {
      "Unknown"
    }
    
    df <- df %>%
      mutate(participant_id = participant_id,
             group = group,
             rotation = rot)
    
    all_data[[length(all_data) + 1]] <- df
    participant_counter <- participant_counter + 1
  }
}


total_group_data <- bind_rows(all_data)
write_csv(total_group_data, "data/total_group_data.csv")


total_group_data <- total_group_data %>%
  mutate(trial_type= case_when(
    group == "Group 1" & cutrial_no >= 1   & cutrial_no <= 24   ~ "aligned",
    group == "Group 1" & cutrial_no >= 25  & cutrial_no <= 40   ~ "zeroclamp",
    group == "Group 1" & cutrial_no >= 41  & cutrial_no <= 48   ~ "aligned",
    group == "Group 1" & cutrial_no >= 49  & cutrial_no <= 65   ~ "lefthand",
    group == "Group 1" & cutrial_no >= 66  & cutrial_no <= 80   ~ "aligned",
    group == "Group 1" & cutrial_no >= 81  & cutrial_no <= 88   ~ "zeroclamp",
    group == "Group 1" & cutrial_no >= 89  & cutrial_no <= 208  ~ "rotated",
    group == "Group 1" & cutrial_no >= 209 & cutrial_no <= 232  ~ "zeroclamprotated",
    group == "Group 1" & cutrial_no >= 233 & cutrial_no <= 256  ~ "lefthandrotated",
    
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
    
    TRUE ~ NA_character_  # Default if no condition matches
  ))




####LEARNERS

getLearners <- function() {
  learner_df <- total_group_data %>%
    filter(
      (group == "Group 1" & cutrial_no %in% 193:208) |
        (group == "Group 2" & cutrial_no %in% 217:232)
    ) %>%
    group_by(participant_id, rotation) %>%
    reframe(
      is_learner = median(reachdeviation_deg, na.rm = TRUE) > (rotation / 2),
      .groups = "drop"
    )
  
  learner_id <- learner_df %>%
    group_by(participant_id, rotation) %>%
    summarise(
      is_learner = any(is_learner),  # or if it's unique per participant, just take first()
      .groups = "drop"
    )
  
  learner_summary <- learner_id %>%
    group_by(rotation) %>%
    summarise(
      total_n = n(),  # count unique participants in each rotation
      n_learners = sum(is_learner),
      percent_learners = round(100 * n_learners / total_n, 1),
      .groups = "drop"
    )
  print(learner_summary)
  
  return(learner_id) 
}


#filter out non learners
LearnerCSV <- function(learner_id) {
  total_learners_data <- total_group_data %>%
    left_join(learner_id, by = c("participant_id", "rotation")) %>% 
    filter(is_learner) %>%    
    select(participant_id, cutrial_no, aimdeviation_deg, reachdeviation_deg,
           rotation, trial_type, group)
  
  write_csv(total_learners_data, "data/total_learners_data.csv")
  return(total_learners_data)
}

learner_id <- getLearners()
total_learners_data <- LearnerCSV(learner_id)


####STRATEGY

#use last 16 rotated trials which represents where strategies are most consistent

getCI <- function () {
  
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
  return(CI)
}

#using CI function, we can now use a 95% interval approach to figure out strategy-users
getStrategies <- function() {
  ci_compare <<- getCI()  
  ci_compare <- ci_compare %>%
    group_by(participant_id) %>%
    mutate(
      final_trials = list(tail(
        last_16_rotated_learners$aimdeviation_deg[
          last_16_rotated_learners$participant_id == participant_id
        ], 8
      )),
      strategy = ifelse(
        (aimdeviation_deg[,1] > 0 & aimdeviation_deg[,2] > 0) &
          (length(final_trials[[1]]) == 8 & all(final_trials[[1]] >= 0)) &
          (first(aimdeviation_deg[,1]) > 5),
        "Yes", "No"
      )
    ) %>%
    ungroup()
  
  return(ci_compare)
}

countStrategies <- function(ci_compare = getStrategies()) {
  strategy_users <- sum(ci_compare$strategy == "Yes", na.rm = TRUE)
  print(strategy_users)
}


strategySummary <- function (ci_compare = getStrategies()) {
  ci_compare %>% 
    group_by(rotation) %>%
    summarise(
      total_n = n(),
      strategy_users = sum(strategy %in% c('Yes')),
      percent_strategy_users = round(100 * strategy_users / total_n, 1),
      .groups = "drop"
    )
}

#make new strategy file

Strategyfile <- function(total_learners_data, ci_compare) {

  strategy_ids <- ci_compare$participant_id[ci_compare$strategy %in% c("Yes")]
  
  strategy_data <- total_learners_data %>%
    filter(participant_id %in% strategy_ids)
  
  strategy_data <- strategy_data %>%
    left_join(
      total_learners_data %>% select(participant_id, cutrial_no, trial_type),
      by = c("participant_id", "cutrial_no")
    ) 
  
  total_learners_data <- total_learners_data %>%
    mutate(strategy = ifelse(participant_id %in% strategy_ids, "Yes", "No"))
  
  total_learners_data$strategy <- ifelse(total_learners_data$strategy == "Yes", 1,
                                         ifelse(total_learners_data$strategy == "No", 0, NA))
  write.csv(strategy_data, "data/strategy_only_participants.csv", row.names = FALSE)
  
  return(list(strategy_data = strategy_data, total_learners_data = total_learners_data))
}



