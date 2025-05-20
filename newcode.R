dir <- "data/Instructed_summary/"  
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


total_data <- bind_rows(all_data)
write_csv(total_data, "data/total_group_data.csv")

####LEARNERS

getLearners <- function(total_group_data) {
  
  learner_df <- total_group_data %>%
    filter((task_idx == 8 & group == 'Group 1') | (task_idx == 12 & group == 'Group 2')) %>%
    group_by(rotation, participant_id) %>%
    summarise(
      is_learner = mean(reachdeviation_deg >= 10, na.rm = TRUE) >= 0.5,
      .groups = "drop"
    )
  
  learners_only <- learner_df %>% 
    filter(is_learner) %>%
    select(participant_id, rotation)
  
  learners_summary <- learner_df %>%
    group_by(rotation) %>%
    summarise(
      n_total = n(),
      n_learners = sum(is_learner),
      percent_learners = round(100 * n_learners / n_total, 1),
      learner_ids = paste(participant_id[is_learner], collapse = ", "),
      .groups = "drop"
    )
  
  return(list(learners_only = learners_only, learners_summary = learners_summary))
}

printLearners <- function () {
learner_data <- getLearners(total_group_data)
learners_only <- learner_data$learners_only
learners_summary <- learner_data$learners_summary
print(learners_summary, n = Inf, width = Inf)
}


####STRATEGY

#use last 8 rotated trials which represents where strategies are most consistent

last_8_rotated <- total_group_data[
  (total_group_data$cutrial_no %in% 201:208 & total_group_data$group == 'Group 1') |
    (total_group_data$cutrial_no %in% 225:232 & total_group_data$group == 'Group 2'),
]



last_8_rotated_learners <- last_8_rotated %>%
  semi_join(learners_only, by = c("rotation", "participant_id"))

CI <- function(last_8_rotated_learners) {
  aggregate(
    aimdeviation_deg ~ participant_id + rotation,
    data = last_8_rotated_learners,
    FUN = function(x) 
    Reach::getConfidenceInterval(x)
    
  )
}

rotated_CI <- CI(last_8_rotated_learners)


getStrategies <- function () {
  ci_compare <- rotated_CI
  
  ci_compare$strategy <- ifelse(
    (ci_compare$aimdeviation_deg[,1] > 0 | ci_compare$aimdeviation_deg[,2] < 0) & 
      ci_compare$aimdeviation_deg[,1] > 5, 
    "Yes", 
    "No")
  ci_compare$strategy[ci_compare$participant_id =='98e5cb'] <- "Noisy"
  ci_compare$strategy[ci_compare$participant_id == '6ebd75'] <- "Noisy"
  print(ci_compare[, c("participant_id", "rotation", "aimdeviation_deg", "strategy")])
}

countStrategies <- function () {
  strategy_users <- sum(ci_compare$strategy %in% c('Yes', 'Noisy'))
  percent_strategy_users <- round(100 * strategy_users / nrow(ci_compare), 1)
  print(percent_strategy_users)
}

# 60 aiming: 98e5cb noisy and non consistent strategy 
#40 aiming: 6ebd75 ??? maybe

strategy_summary <- ci_compare %>% 
  group_by(rotation) %>%
  summarise(
    total_n = n(),
    strategy_users = sum(strategy %in% c('Yes', 'Noisy')),
    percent_strategy_users = round(100 * strategy_users / total_n, 1),
    .groups = "drop"
  )


#make new strategy file

Strategyfile <- function () {
  strategy_ids <- ci_compare$participant_id[ci_compare$strategy %in% c("Yes", "Noisy")]

  
strategy_data <- total_group_data %>%
  filter(participant_id %in% strategy_ids)


write.csv(strategy_data, "data/strategy_only_participants.csv", row.names = FALSE)
}




alignedphase <- total_group_data[
  (total_group_data$cutrial_no %in% 1:88 & total_group_data$group == 'Group 1') |
    (total_group_data$cutrial_no %in% 1:112 & total_group_data$group == 'Group 2'),
]


rotatedphase <- total_group_data[
  (total_group_data$cutrial_no %in% 89:208 & total_group_data$group == 'Group 1') |
    (total_group_data$cutrial_no %in% 105:232 & total_group_data$group == 'Group 2'),
]


meanaim <- function () {
  
  grouped_strategy_data <- total_group_data %>%
    mutate(group = ifelse(participant_id %in% strategy_ids, "Yes", "No"))

  plot_mean_aim_data <- grouped_data %>%
   filter(cutrial_no %in% c(201:208, 225:232)) %>%
   group_by(rotation, participant_id, group) %>%
   summarise(mean_aim = mean(aimdeviation_deg, na.rm = TRUE), .groups = "drop")
  
  plot_mean_aim_data <- plot_mean_aim_data %>%
    mutate(fill_color = ifelse(group == "Yes", as.character(rotation), "white"))
  
 p <- ggplot(plot_mean_aim_data, aes(x = factor(rotation), y = mean_aim, fill = fill_color, color = factor(rotation))) +
    geom_point(aes(shape = group), size = 3, stroke = 1.2) +
    scale_shape_manual(
      values = c("Yes" = 21, "No" = 21),
      name = "Aiming Strategy?"
    ) +
    scale_fill_manual(
      values = c(
        "20" = "deeppink",
        "30" = "orange",
        "40" = "blue",
        "50" = "black",
        "60" = "cyan",
        "white" = "white"
      ),
      name = "Aiming Strategy?"
    ) +
    scale_color_manual(
      values = c(
        "20" = "deeppink",
        "30" = "orange",
        "40" = "blue",
        "50" = "black",
        "60" = "cyan"
      ),
      name = "Rotation (degrees)"
    ) +
   
    guides(color = "none") +    
    guides(
      fill = "none",  
      shape = guide_legend(override.aes = list(fill = c("white", "black")))
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),  # remove minor grids
      axis.line = element_line(color = "black")  
    )+
    theme(legend.position = "top") +
   labs(
     x = "Rotation Group",
     y = expression("Aim Deviation ("*degree*")")
   ) 
 
 
}
    


  
density_plot <- ggplot(plot_mean_aim_data, aes(x = mean_aim, fill = group)) +
  geom_density(alpha = 0.6, color = NA, adjust = 2.5) +
  scale_fill_manual(values = c("Yes" = "darkblue", "No" = "lightblue")) +
  coord_flip() + 
  xlim(-5, 60) +
  theme_void() +  # Clean plot
  theme(legend.position = "none")
  
  
  
final_plot <- density_plot + p + plot_layout(widths = c(1, 4))
print(final_plot)
  
  
  

