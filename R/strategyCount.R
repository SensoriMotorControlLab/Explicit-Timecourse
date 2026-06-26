
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
        participant_id == "657fba" ~ "No", #this was the weird one
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

