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
    dplyr::select(participant_id, rotation)
  
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
      ci_compare$aimdeviation_deg[,1] > 4.9, 
    "Yes", 
    "No")
  ci_compare$strategy[ci_compare$participant_id == '98e5cb'] <- 'No' 
  print(ci_compare[, c("participant_id", "rotation", "aimdeviation_deg", "strategy")])
}

countStrategies <- function () {
  strategy_users <- sum(ci_compare$strategy %in% c('Yes'))
  percent_strategy_users <- round(100 * strategy_users / nrow(ci_compare), 1)
  print(percent_strategy_users)
}

# 60 aiming: 98e5cb noisy and non consistent strategy 
#40 aiming: 6ebd75 ??? & 3091de

#so in Tsay (2024), He acknowledges that some strategies are more "exploratory" rather than fast or slow insight.
#perhaps we include these "noisy strategies" because participants are giving feedback that they feel their hand moving
#in a pattern of back and forth.. (n=3 have said this already)

#"Rule-based strategy"
        #Participants adopt a consistent internal rule 
        #(e.g., “the correct answer alternates”) and stick with it, regardless
        # of actual task contingencies.



strategy_summary <- ci_compare %>% 
   group_by(rotation) %>%
  summarise(
    total_n = n(),
    strategy_users = sum(strategy %in% c('Yes')),
    percent_strategy_users = round(100 * strategy_users / total_n, 1),
    .groups = "drop"
  )


#make new strategy file

Strategyfile <- function () {
  strategy_ids <- ci_compare$participant_id[ci_compare$strategy %in% c("Yes")]

  
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

  grouped_strategy_data <- grouped_strategy_data %>%
    mutate(group = ifelse(participant_id == "4eeaee", "Yes", group))
  
  plot_mean_aim_data <- grouped_strategy_data %>%
   filter(cutrial_no %in% c(201:208, 225:232)) %>%
   group_by(rotation, participant_id, group) %>%
   summarise(mean_aim = mean(aimdeviation_deg, na.rm = TRUE), .groups = "drop")
  
  plot_mean_aim_data <- plot_mean_aim_data %>%
    mutate(fill_color = ifelse(group == "Yes", as.character(rotation), "white"))
  max_y <- max(plot_mean_aim_data$mean_aim, na.rm = TRUE)
  
 p <- ggplot(plot_mean_aim_data, aes(x = factor(rotation), y = mean_aim, color = factor(rotation))) +
   geom_point(aes(shape = group, fill = factor(rotation)), size = 3, stroke = 1.2) +
   
   scale_shape_manual(
     values = c("Yes" = 21, "No" = 4),
     name = "Correct aiming strategy"
   ) +
   
   scale_color_manual(
     values = c(
       "20" = "#b9d6e5",
       "30" = "#9fbdd8",
       "40" = "#848fbe",
       "50" = "#74599c",
       "60" = "#6b0077"
     ),
     name = "Rotation (degrees)"
   ) +
   
   scale_fill_manual(
     values = c(
       "20" = "#b9d6e5",
       "30" = "#9fbdd8",
       "40" = "#848fbe",
       "50" = "#74599c",
       "60" = "#6b0077"
     ),
     guide = "none"
   ) +
   
   guides(
     color = "none",
     fill = "none",
     shape = guide_legend(
       override.aes = list(
         shape = c(4, 21),                     # Yes = filled circle (21), No = x (4)
         color = c("#C7E5Be", "#165660"),      # outline color for both
         fill = c(NA, "#165660")))
     ) +
  
   theme_minimal() +
   theme(
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     axis.line = element_line(color = "black"),
     legend.position = "right"
   ) +
   
   labs(
     x = "Rotation Group",
     y = expression("Aim Deviation ("*degree*")")
   )
   
}
    

library(patchwork)
  
density_plot <- ggplot(plot_mean_aim_data, aes(x = mean_aim, fill = group)) +
  geom_density(alpha = 0.6, color = NA, adjust = 2.5) +
  scale_fill_manual(values = c("Yes" = "#165660", "No" = "#C7E5BE")) +
  coord_flip() + 
  xlim(-5, 60) +
  theme_void() +  
  theme(legend.position = "none") 
  
  
  
final_plot <- density_plot + p + plot_layout(widths = c(1, 4))
print(final_plot)

  





df <- total_group_data[total_group_data$participant_id == "f96e89", ] 
plot(df$aimdeviation_deg, type = "l", main = "19187c ~50", ylim = c(-10, 60))
abline(h = 0, col = "red", lty = 2)
abline(h = 5, col = "red", lty = 2)



#60 aiming: 98e5cb noisy and non consistent strategy & 3091de
#40 aiming: 6ebd75 ??? i think they get the startegy bc all left hand trials have a good strategy too 
#40 aa25ec is also back and forth

#let me check if they aimed above 10 for half of the trials which might determine if motivation factors are at play!
