dir <- "data/Instructed_summary/"  
library(readr)
library(dplyr)
rotations <- c(20, 30, 40, 50, 60)

all_data <- list()
participant_counter <- 1

for (rot in rotations) {
  folder_path <- file.path(dir, paste0("aiming", rot))  # e.g., aiming40
  
  csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  for (file in csv_files) {
    df <- read_csv(file)
    
    participant_id <- gsub(paste0("SUMMARY_aiming", rot, "_(.*)\\.csv"), "\\1", basename(file))
    
    
    # Determine group based on task_idx (if max task_idx ≤ 13 → group 2)
    group <- ifelse(max(df$task_idx, na.rm = TRUE) <= 13, "Group 2", "Group 1")
    
    max_idx <- max(df$task_idx, na.rm = TRUE)
    group <- if (max_idx <= 10) {
      "Group 1"
    } else if (max_idx <= 13) {
      "Group 2"
    } else {
      "Unknown"
    }
    
    # Add new columns
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

getLearners <- function(total_group_data) {
  learner_df <- total_group_data %>%
    filter(
      (group == "Group 1" & cutrial_no %in% 194:209) |
        (group == "Group 2" & cutrial_no %in% 218:233)
    ) %>%
    group_by(participant_id, rotation) %>%
    summarise(
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
}

#filter out non learners
LearnerCSV <- function () {
  total_learners_data <- total_group_data %>%
  semi_join(learner_id, by = c("rotation", "participant_id")) %>%
  select(participant_id, cutrial_no, aimdeviation_deg, reachdeviation_deg, rotation, trial_type, group)
write_csv(total_learners_data, "data/total_learners_data.csv")
}

####STRATEGY

#use last 16 rotated trials which represents where strategies are most consistent

getCI <- function () {
  
  last_16_rotated <- total_learners_data[
  (total_learners_data$cutrial_no %in% 193:208 & total_learners_data$group == 'Group 1') |
    (total_learners_data$cutrial_no %in% 217:232 & total_learners_data$group == 'Group 2'),
]

CI <- aggregate(
    aimdeviation_deg ~ participant_id + rotation,
    data = last_16_rotated,
    FUN = function(x) 
    Reach::getConfidenceInterval(x)
  )
return(CI)
}

#using CI function, we can now use a 95% interval approach to figure out strategy-users
getStrategies <- function () {
  rotated_CI <- CI(last_16_rotated_learners)
  ci_compare <- rotated_CI
  
  ci_compare$strategy <- ifelse(
    (ci_compare$aimdeviation_deg[,1] > 0 | ci_compare$aimdeviation_deg[,2] < 0) & 
      ci_compare$aimdeviation_deg[,1] > 4.9, 
    "Yes", 
    "No")
  print(ci_compare[, c("participant_id", "rotation", "aimdeviation_deg", "strategy")])
}

countStrategies <- function () {
  strategy_users <- sum(ci_compare$strategy %in% c('Yes'))
  percent_strategy_users <- round(100 * strategy_users / nrow(ci_compare), 1)
  print(percent_strategy_users)
}

strategySummary <- function () {
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

Strategyfile <- function () {
  strategy_ids <- ci_compare$participant_id[ci_compare$strategy %in% c("Yes")]

  
strategy_data <- total_learners_data %>%
  filter(participant_id %in% strategy_ids)

strategy_data <- strategy_data %>%
  left_join(
    total_learners_data %>% select(participant_id, cutrial_no, trial_type),
    by = c("participant_id", "cutrial_no")
  ) 

write.csv(strategy_data, "data/strategy_only_participants.csv", row.names = FALSE)
}

total_learners_data <- total_learners_data %>%
  mutate(strategy = ifelse(participant_id %in% strategy_ids, "Yes", "No"))

total_learners_data$strategy <- ifelse(total_learners_data$strategy == "yes", 1,
                    ifelse(total_learners_data$strategy == "no", 0, NA))



  


###not a part of my data analysis###

#exploratory
df <- total_group_data[total_group_data$participant_id == "f275ca", ] 
plot(df$aimdeviation_deg, type = "l", main = "aiming 60", ylim = c(-10, 60),
     col= "#DAA520", lwd = 1)
abline(h = 0, col = "red", lty = 2)
abline(h = 60, col = "red", lty = 2)


#slow insight
df <- total_group_data[total_group_data$participant_id == "54044d", ] 
plot(df$aimdeviation_deg, type = "l", main = "50", ylim = c(-10, 60))
abline(h = 0, col = "red", lty = 2)
abline(h = 50, col = "red", lty = 2)

#fast insight - two step
df <- total_group_data[total_group_data$participant_id == "31b753", ] 
plot(df$aimdeviation_deg, type = "l", main = "60", ylim = c(-10, 60))
abline(h = 0, col = "red", lty = 2)
abline(h = 60, col = "red", lty = 2)

#slow insight
df <- total_group_data[total_group_data$participant_id == "9fb9fe", ] 
plot(df$aimdeviation_deg, type = "l", main = "aiming 30", ylim = c(-10, 60),
     col= "darkred", lwd = 1)
abline(h = 0, col = "red", lty = 2)
abline(h = 30, col = "red", lty = 2)

#fast insight
df <- total_group_data[total_group_data$participant_id == "4093e8", ] 
plot(df$aimdeviation_deg, type = "l", main = "aiming 60", ylim = c(-10, 60),
     col= "purple", lwd = 1)
abline(h = 0, col = "red", lty = 2)
abline(h = 60, col = "red", lty = 2)

df <- total_group_data[total_group_data$participant_id == "94709f", ] 
plot(df$aimdeviation_deg, type = "l", main = "50", ylim = c(-10, 60))
abline(h = 0, col = "red", lty = 2)
abline(h = 60, col = "red", lty = 2)

#systemic exploration 
df <- total_group_data[total_group_data$participant_id == "901482", ] 
plot(df$aimdeviation_deg, type = "l", main = "aiming 50", ylim = c(-10, 60),
     col= "forestgreen", lwd = 1)
abline(h = 0, col = "red", lty = 2)
abline(h = 50, col = "red", lty = 2)

#60 aiming: 98e5cb noisy and non consistent strategy & 3091de
#40 aiming: 6ebd75 ??? i think they get the startegy bc all left hand trials have a good strategy too 
#40 aa25ec is also back and forth

#let me check if they aimed above 10 for half of the trials which might determine if motivation factors are at play!
