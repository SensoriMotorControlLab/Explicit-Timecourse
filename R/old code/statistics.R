allLeaners <- function () {
  learners_60_count <- learners_60()  
  learners_50_count <- learners_50()  
  learners_40_count <- learners_40() 
  learners_30_count <- learners_30()  
  learners_20_count <- learners_20()  
  
} 



allStrategies <- function () {
  yes_count <- nrow(yes_strategies)
  total_count <- nrow(all_mean_aims)
  percentage_yes <- (yes_count / total_count) * 100
  print(paste("Percentage of participants with 'Yes' strategy:", round(percentage_yes, 2), "%"))
  
}


#DOES MEAN AIMING DEV DIFFER ACROSS ROTATION GROUPS??

getANOVAStrategies <- function () {
  yes_strategies <- all_mean_aims %>%
    filter(participant_id %in% plot_all_rotated$participant_id[plot_all_rotated$strategy == "Yes"]) %>%
    mutate(strategy = "Yes") #26 strategies
  
  yes_strategies_aov <- yes_strategies %>%
    filter(rotation != 20)
  print(yes_strategies)
}

aimANOVA <- function () {
yes_strategies_aov$rotation <- as.factor(yes_strategies_aov$rotation)
    
anova_result <- aov(aim_shift ~ rotation, data = yes_strategies_aov)
summary(anova_result)

emm_result <- emmeans(anova_result, ~ rotation)
pairs(emm_result)

}

#conduct post hoc test
TukeyHSD(anova_result)

emm_result <- emmeans(anova_result, ~ rotation)
pairs(emm_result) #60 and 30 and 60 and 40 are sig!


#DOES NUMBER OF STRATEGY-USERS DIFFER ACROSS ROTATION GROUPS?


    #we are working with binary data here: did a participant have a strategy, and 
    #does the probability of having a strategy differ across rotation groups?? so lets use a 
    #logistic regression model!

logistic_reg_output <- function () {
  all_mean_aims <- all_mean_aims %>%
    mutate(strategy_binary = ifelse(strategy == "Yes", 1, 0))
  reg_model <- glm(strategy_binary ~ rotation, data = all_mean_aims, family = binomial)
  summary(reg_model)
}

    #no strong statistical evidence that rotation affects strategy use — 
    #but the positive trend suggests that with more data, a real effect might emerge.




#one-way anova  
strategy_counts <- all_mean_aims %>%
group_by(rotation) %>%
summarise(num_strategies = sum(strategy == "Yes"))

strategy_counts
# A tibble: 5 × 2
rotation num_strategies
<dbl>          <int>
  1       20              1
2       30              2
3       40              7
4       50              7
5       60              9

countANOVA <- function () {anova_strategy <- aov(num_strategies ~ rotation, data = strategy_counts)
summary(anova_strategy)
}


#Does the average trial at which the step occurs differ across rotation sizes?

first_60rotated_aov <- rbind(
  df60_aim[df60_aim$participant_id %in% c(1,4,5,8,11,12) & df60_aim$cutrial_no %in% 89:138, ],
  df60_aim[df60_aim$participant_id %in% c(6,7,13) & df60_aim$cutrial_no %in% 113:162, ]
)

first_60rotated_aov$cutrial_no <- ifelse(first_60rotated_aov$cutrial_no >= 113,
                                         first_60rotated_aov$cutrial_no - 112,
                                         first_60rotated_aov$cutrial_no - 88)


first_50rotated_aov <- rbind(
  df50_aim[df50_aim$participant_id %in% c(6,8,9,12) & df50_aim$cutrial_no %in% 89:138, ],
  df50_aim[df50_aim$participant_id %in% c(2,3,13) & df50_aim$cutrial_no %in% 113:162, ]
)
first_50rotated_aov$cutrial_no <- ifelse(first_50rotated_aov$cutrial_no >= 113,
                                         first_50rotated_aov$cutrial_no - 112,
                                         first_50rotated_aov$cutrial_no - 88)


first_40rotated_aov <- rbind(
  df40_aim[df40_aim$participant_id %in% c(2,3,6,7,8,12) & df40_aim$cutrial_no %in% 89:138, ],
  df40_aim[df40_aim$participant_id %in% 10 & df40_aim$cutrial_no %in% 113:162, ]
)
first_40rotated_aov$cutrial_no <- ifelse(first_40rotated_aov$cutrial_no >= 113,
                                         first_40rotated_aov$cutrial_no - 112,
                                         first_40rotated_aov$cutrial_no - 88)


# 30° Rotation Group — only has cutrial_no 113–130
first_30rotated_aov <- df30_aim[df30_aim$participant_id %in% c(1,2) & df30_aim$cutrial_no %in% 113:162, ]
first_30rotated_aov$cutrial_no <- first_30rotated_aov$cutrial_no - 112

AllTrials <- rbind(first_60rotated_aov, first_50rotated_aov, first_40rotated_aov,first_30rotated_aov)

getStepTrial <- function(AllTrials, threshold = 10) {
  participants <- unique(AllTrials$participant_id)
  step_trials <- data.frame(participant_id = integer(0), rotation_deg = numeric(0), step_trial = integer(0))
  
  for (p in participants) {
    # Subset data for the current participant
    df_p <- AllTrials[AllTrials$participant_id == p, ]
    rotation <- unique(df_p$rotation_deg)
    
    # Check which trials exceed the threshold
    step_idx <- which(abs(df_p$aimdeviation_deg) > threshold)
    
    # If there are trials exceeding the threshold, capture the first one
    if (length(step_idx) > 0) {
      step_trial <- df_p$cutrial_no[step_idx[1]]
    } else {
      step_trial <- NA  # No trials exceed the threshold
    }
    
    # Append results directly to the data frame
    step_trials <- rbind(step_trials, data.frame(
      participant_id = p,
      rotation_deg = rotation,
      step_trial = step_trial
    ))
  }
  
  return(step_trials)
}

s60 <- getStepTrial(first_60rotated_aov, threshold = 10)
s50 <- getStepTrial(first_50rotated_aov, threshold = 10)
s40 <- getStepTrial(first_40rotated_aov, threshold = 10)
s30 <- getStepTrial(first_30rotated_aov, threshold = 10)


StepAOV <- function () {
  combinedSteps <- rbind (s60,s50,s40,s30)
  avgSteps <- aggregate(step_trial ~ rotation_deg, data = combinedSteps, FUN = mean)
  stepaov <- aov(step_trial ~ factor(rotation_deg), data = combinedSteps)
  emms <- emmeans(stepaov, ~ rotation_deg)
  pairs(emms, adjust = "tukey", infer = TRUE, effect.size = "d")
}






#####BAYESIAN TESTING

install.packages("BayesFactor")  
library(BayesFactor)
library(brms)

aimBAYES <- function () {
  bf_anova <- anovaBF(aim_shift ~ rotation, data = yes_strategies_aov)
  print(bf_anova)
}

countBAYES <- function () {
  bf_regression <- regressionBF(num_strategies ~ rotation, data = strategy_counts)
  
  print(bf_regression)
}

stepBAYES <- function () {
  combinedSteps$rotation_deg <- as.factor(combinedSteps$rotation_deg)
  bf_anova <- anovaBF(step_trial ~ rotation_deg, data = combinedSteps)
  print(bf_anova)
}


model_full <- brm(
  aim_shift ~ rotation,
  data = yes_strategies_aov,
  family = gaussian(),
  seed = 123
)

# Fit null model (intercept only)
model_null <- brm(
  aim_shift ~ 1,
  data = yes_strategies_aov,
  family = gaussian(),
  seed = 123
)

bf_result <- bayes_factor(model_full, model_null)
print(bf_result)










