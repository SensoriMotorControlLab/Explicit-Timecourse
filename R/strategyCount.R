density <- function() {
  
  total_learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors =FALSE)
  
  last_16_participant_means <- total_learners_data %>%
    filter(
      cutrial_no %in% 224:232,
      group == "Group 2"
    ) %>%
    group_by(participant_id) %>%
    summarise(
      mean_aim = mean(aimdeviation_deg, na.rm = TRUE),
      .groups = "drop"
    )
  
  density_data <- density(
    last_16_participant_means$mean_aim,
    bw = 1
  )
  
  plot(
    density_data,
    main = "",
    xlab = "Final 8 mean Aiming Deviation (degrees)",
    ylab = "Density",
    xlim = c(-20, 70),
    col = "blue",
    lwd = 2
  )

  polygon(density_data, col = "lightblue", border = "cadetblue")
  # Find the minimum density between 1 and 10 degrees
  range_idx <- which(
    density_data$x >= 1 &
      density_data$x <= 10
  )
  
  min_idx <- range_idx[which.min(density_data$y[range_idx])]
  
  min_x <- density_data$x[min_idx]
  min_y <- density_data$y[min_idx]
  
  # Mark the minimum
  points(min_x, min_y, col = "red", pch = 19, cex = 1.2)

  cat("Minimum at:", round(min_x, 2), "degrees\n")
  cat("Density:", round(min_y, 5), "\n")
  abline(v = min_x, col = "red", lty = "dashed")
  
  
  abline(v = 0, col = "grey30", lwd = 2, lty = "dotted")
  abline(v = 14.8, col = "grey30", lwd = 2, lty = "dotted")
  
  text(
    x = 0,
    y = max(density_data$y) * 1.05,
    labels = "Non-strategy users",
    col = "grey30",
    cex = 0.8,
    pos = 3,
    xpd = TRUE
  )
  
  text(
    x = 14.8,
    y = max(density_data$y) * 1.05,
    labels = "Strategy users",
    col = "grey30",
    cex = 0.8,
    pos = 3,
    xpd = TRUE
  )
  
  
}



#use last 16 rotated trials which represents where strategies are most consistent

getCI <- function () {
  
  total_learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors =FALSE)
  
  last_16_rotated_learners <- total_learners_data[
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
# 
# #using CI function, we can now use a 95% interval approach to figure out strategy-users
# getStrategies <- function() {
#   
#   ci_result <- getCI()
#   
#   CI_df <- ci_result$CI
#   raw_df <- ci_result$data
#   
#   sign_df <- raw_df %>%
#     group_by(participant_id, rotation) %>%
#     summarise(
#       negative_trials = sum(aimdeviation_deg < 0, na.rm = TRUE),
#       .groups = "drop"
#     )
#   
#   # Flag participants with more than 1 negative-aim trial
#   sign_df <- sign_df %>%
#     mutate(
#       flip_flag = negative_trials > 1
#     )
#   
#   
#   # final <- raw_df %>%
#   #   group_by(participant_id, rotation) %>%
#   #   summarise(
#   #      final = tail(aimdeviation_deg, 8),
#   #     .groups = "drop"
#   #   )
#   # 
#   
#   # 3. apply CI only to stable participants
#   strategy_df <- CI_df %>%
#     rowwise() %>%
#     mutate(
#       lower = aimdeviation_deg[1],
#       upper = aimdeviation_deg[2]
#     ) %>%
#     ungroup() %>%
#     left_join(sign_df, by = c("participant_id", "rotation")) %>%
#     mutate(
#       strategy = case_when(
#         participant_id == "657fba" ~ "No", #this was the weird one
#         flip_flag ~ "No",
#         lower > 5 ~ "Yes",
#         
#         TRUE ~ "No"
#       )
#     ) %>%
#     dplyr::select(participant_id, rotation, lower, upper, negative_trials, strategy)
#   
#   return(strategy_df)
# }


getStrategies <- function() {
  
  total_learners_data <- read.csv(
    "data/total_learners_data.csv",
    stringsAsFactors = FALSE
  )
  
  # final 8 trials
  raw_df <- total_learners_data %>%
    filter(
      cutrial_no %in% 224:232,
      group == "Group 2"
    )
  
  # each participant's mean aiming deviation
  participant_means <- raw_df %>%
    group_by(participant_id, rotation) %>%
    summarise(
      mean_aim = mean(aimdeviation_deg, na.rm = TRUE),
      .groups = "drop"
    )
  
  # density-based threshold
  density_data <- stats::density(
    participant_means$mean_aim,
    bw = 2
  )
  
  range_idx <- which(
    density_data$x >= 1 &
      density_data$x <= 10
  )
  
  min_idx <- range_idx[which.min(density_data$y[range_idx])]
  threshold <- density_data$x[min_idx]
  
  #cat("Density-based threshold:", round(threshold, 2), "degrees\n")
  

  sign_df <- raw_df %>%
    group_by(participant_id, rotation) %>%
    summarise(
      negative_trials = sum(aimdeviation_deg < 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(flip_flag = negative_trials > 1)
  
  # classify strategy users 
  strategy_df <- participant_means %>%
    left_join(sign_df, by = c("participant_id", "rotation")) %>%
    mutate(
      threshold = threshold,
      strategy = case_when(
        participant_id == "657fba" ~ "No",
        flip_flag ~ "No",
        mean_aim > threshold ~ "Yes",
        TRUE ~ "No"
      )
    ) %>%
    dplyr::select(
      participant_id, rotation, mean_aim,
      threshold, negative_trials, flip_flag, strategy
    )
  
  return(strategy_df)
}


countStrategies <- function() {
  
  strategy_df <- getStrategies()
  
  strategy_users <- strategy_df %>%
    filter(strategy == "Yes") %>%
    summarise(n = n_distinct(participant_id)) %>%
    pull(n)
  
  cat("Number of strategy users:", strategy_users, "\n")
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


#add sanity check -- those with erratic timecourses might just be aiming where target was at previous trial!
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

