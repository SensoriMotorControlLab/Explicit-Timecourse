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



#####BAYESIAN TESTING


library(brms)

aimBAYES <- function () {
  bf_anova <- anovaBF(aim_shift ~ rotation, data = yes_strategies_aov)
  print(bf_anova)
}

countBAYES <- function () {
  bf_regression <- regressionBF(num_strategies ~ rotation, data = strategy_counts)
  
  print(bf_regression)
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




