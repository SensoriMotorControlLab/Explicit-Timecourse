##informed labeling function fitting


participant_first_aim <- strategy_data_clusters %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  summarise(
    first_trial_above7 = cutrial_no[which(aimdeviation_deg > 7)[1]],
    aim_at_first_trial = aimdeviation_deg[which(aimdeviation_deg > 7)[1]],
    strategy_type      = first(cluster),
    .groups = "drop"
  ) %>%
  mutate(strategy_type = case_when(
    strategy_type == 1 ~ "erratic",
    strategy_type == 2 ~ "delayed",
    strategy_type == 3 ~ "rapid"
  ))

View(participant_first_aim)



                                #----- rapid -----#
### perhaps fit a step function of above 7 deg early in aimdeviation_deg (within 10 trials)?
# - mean time course (based on actual data from unsupervised), small sd
# - mean aim(based on actual data), small sd too


meanaimRapid <- mean(participant_first_aim %>%
                       filter(strategy_type == "rapid") %>%
                       pull(aim_at_first_trial),
                     na.rm = TRUE
) #24.83

sdaimRapid <- sd(participant_first_aim %>%
                   filter(strategy_type == "rapid") %>%
                   pull(aim_at_first_trial),
                 na.rm = TRUE
) #27.21

meanchangeRapid <- mean(participant_first_aim %>%
                          filter(strategy_type == "rapid") %>%
                          pull(first_trial_above7),
                        na.rm = TRUE
) #6.13

sdchangeRapid <- sd(participant_first_aim %>%
                      filter(strategy_type == "rapid") %>%
                      pull(first_trial_above7),
                    na.rm = TRUE
) #3.72

                               #----- delayed -----#

# Same as rapid but t0 should be above 10

meanaimDelayed <- mean(participant_first_aim %>%
                       filter(strategy_type == "delayed") %>%
                       pull(aim_at_first_trial),
                     na.rm = TRUE
) #16,73

sdaimDelayed <- sd(participant_first_aim %>%
                   filter(strategy_type == "delayed") %>%
                   pull(aim_at_first_trial),
                 na.rm = TRUE
) #9,31

meanchangeDelayed <- mean(participant_first_aim %>%
                          filter(strategy_type == "delayed") %>%
                          pull(first_trial_above7),
                        na.rm = TRUE
) #trial 32.14

sdchangeDelayed <- sd(participant_first_aim %>%
                      filter(strategy_type == "delayed") %>%
                      pull(first_trial_above7),
                    na.rm = TRUE
) #12.90


                           #----- erratic -----#
#there will be flips in signs

meanaimErratic <- mean(participant_first_aim %>%
                         filter(strategy_type == "erratic") %>%
                         pull(aim_at_first_trial),
                       na.rm = TRUE
) # 100.33

sdaimErratic <- sd(participant_first_aim %>%
                     filter(strategy_type == "erratic") %>%
                     pull(aim_at_first_trial),
                   na.rm = TRUE
) # 31.63

meanchangeErratic<- mean(participant_first_aim %>%
                            filter(strategy_type == "erratic") %>%
                            pull(first_trial_above7),
                          na.rm = TRUE
) # 3.67

sdchangeErratic <- sd(participant_first_aim %>%
                        filter(strategy_type == "erratic") %>%
                        pull(first_trial_above7),
                      na.rm = TRUE
) #1.53


#a point where they learned. it (one parameter) - time change
#second p arameter, is how mucnh they learned, like s tep. function
#increase weight,if. sd before step is. super high
#have aligned


fit_onestep_model <- function(trials, aim,
                              init_baseline = 0,
                              init_mean = 20,
                              init_tchange = 10,
                              init_aimsd1 = 10,
                              min_step = 5) {
  
  step_model <- function(par, trials) {
    baseline <- par[1]   # baseline before change
    mean2 <- par[2]      # plateau after change
    tchange <- par[3]    # trial of change
    pred <- ifelse(trials < tchange, baseline, mean2)
    return(pred)
  }
  
  sse_function <- function(par) {
    pred <- step_model(par, trials)
    sum((aim - pred)^2, na.rm = TRUE)
  }
  
  fit <- optim(
    par = c(init_baseline, init_mean, init_tchange),
    fn = sse_function,
    method = "L-BFGS-B",
    lower = c(-90, 0, min_step),
    upper = c(90, 180, max(trials))
  )
  
  return(fit)
}

strategy_params <- list(
  rapid = list(
    init_baseline = 0,
    init_mean = 15,
    init_tchange = 6,
    init_sd = 5,        # relatively low noise
    min_step = 5
  ),
  delayed = list(
    init_baseline = 0,
    init_mean = 15,
    init_tchange = 30,
    init_sd = 5,        # also low/moderate noise
    min_step = 11
  ),
  erratic = list(
    init_baseline = 0,
    init_mean = 15,
    init_tchange = 10,  # can be midrange
    init_sd = 25,       # **high SD** → large variability
    min_step = 5
  )
)



fits <- trial_features %>%
  group_by(participant_id) %>%
  group_modify(~{
    trials <- .x$trial_after_rot
    aim <- .x$aimdeviation_deg
    
    models <- lapply(strategy_params, function(p) {
      fit_onestep_model(
        trials = trials,
        aim = aim,
        init_mean = p$init_mean,
        init_tchange = p$init_tchange,
        init_aimsd1 = p$init_aimsd1,
        min_step = p$min_step
      )
    })
    
    sse_values <- sapply(models, function(f) f$value)
    best_model <- names(which.min(sse_values))
    
    tibble(
      participant_id = unique(.x$participant_id),
      best_model = best_model,
      sse_rapid = sse_values["rapid"],
      sse_delayed = sse_values["delayed"],
      sse_erratic = sse_values["erratic"]
    )
  })
